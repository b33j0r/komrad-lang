use ratatui::crossterm::event::{
    self,
    DisableMouseCapture,
    EnableMouseCapture,
    Event as CrosstermEvent,
    KeyCode,
    KeyEvent as TuiKeyEvent, // We'll use the re-exported TuiKeyEvent type
};
use ratatui::crossterm::{
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::prelude::{Line, Stylize};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout},
    style::{Color, Style},
    widgets::{Block, Borders},
    Terminal,
};
use std::path::{Path, PathBuf};
use std::{error::Error, io, thread, time::Duration};
use tokio::{select, sync::mpsc, time};
use tokio_util::sync::CancellationToken;
use tracing::{debug, error, info, warn};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::registry::Registry;
use tui_logger::{
    ExtLogRecord, LevelFilter, LogFormatter, TuiLoggerWidget, TuiTracingSubscriberLayer,
    TuiWidgetState,
};
use tui_textarea::TextArea;

use komrad_core::ToSExpr;
use komrad_interpreter::Interpreter;
use komrad_parser::parse_toplevel::parse_snippet_complete;

/// An enum for our internal events.
enum Event<I> {
    Input(I),
    Tick,
}

/// Spawn a blocking thread to read crossterm events and send them over a standard mpsc channel.
/// The loop checks the provided cancellation token on each iteration.
fn spawn_blocking_event_reader(tx: mpsc::Sender<CrosstermEvent>, shutdown: CancellationToken) {
    thread::spawn(move || {
        while !shutdown.is_cancelled() {
            // Wait for an event up to 100ms
            if let Ok(true) = event::poll(Duration::from_millis(10)) {
                if let Ok(ev) = event::read() {
                    match tx.try_send(ev) {
                        Ok(_) => {}
                        Err(_) => break,
                    }
                }
            }
        }
    });
}

/// Interpreter hook.
async fn interpret_input(
    input: &str,
    interpreter: &mut Interpreter,
) -> Result<String, Box<dyn Error>> {
    let mut codemaps = komrad_core::CodeAtlas::new();
    let top_level = parse_snippet_complete(&mut codemaps, input)
        .map_err(|e| format!("Parse error: {:?}", e))?;
    let sexpr = top_level.to_sexpr();
    debug!("SEXPR:  {}", sexpr.to_plain_string());

    let result = interpreter.run_top_level(top_level).await;
    match result {
        Ok(value) => {
            let result_str = value.to_sexpr().to_plain_string();
            Ok(result_str)
        }
        Err(e) => Err(format!("Runtime error: {:?}", e).into()),
    }
}

/// A custom log formatter for the TUI logger that uses colors based on log levels.
struct MyLogFormatter;

impl LogFormatter for MyLogFormatter {
    fn min_width(&self) -> u16 {
        0
    }

    fn format(&self, width: usize, evt: &ExtLogRecord) -> Vec<Line> {
        let color = match evt.level {
            log::Level::Trace => Color::Gray,
            log::Level::Debug => Color::LightBlue,
            log::Level::Info => Color::LightGreen,
            log::Level::Warn => Color::LightYellow,
            log::Level::Error => Color::LightRed,
        };
        textwrap::wrap(evt.msg(), width)
            .iter()
            .map(|line| {
                let span =
                    ratatui::prelude::Span::styled(line.to_string(), Style::default().fg(color));
                Line::from(span)
            })
            .collect()
    }
}

// -- snip previous imports --

pub async fn main(mut interpreter: Interpreter, file: &Option<PathBuf>) -> Result<(), Box<dyn Error>> {
    tui_logger::init_logger(LevelFilter::Trace)?;
    tracing::subscriber::set_global_default(Registry::default().with(TuiTracingSubscriberLayer))?;

    if let Some(file) = file {
        if !file.exists() {
            return Err(format!("File not found: {}", file.display()).into());
        }
        match interpreter.load_and_run_file_path(&file).await {
            Ok(_) => {
                info!("Loaded {}", file.display());
            }
            Err(e) => {
                error!("Error executing file: {}", e);
                return Err(e.into());
            }
        }
    }

    let shutdown_token = CancellationToken::new();
    let shutdown_ctrlc = shutdown_token.clone();

    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let (blocking_tx, mut blocking_rx) = mpsc::channel::<CrosstermEvent>(8);
    spawn_blocking_event_reader(blocking_tx, shutdown_token.clone());

    let (ui_tx, mut ui_rx) = mpsc::channel::<Event<CrosstermEvent>>(100);

    let ui_tx_clone = ui_tx.clone();

    // Forward blocking events into async channel.
    tokio::spawn(async move {
        loop {
            if let Some(ev) = blocking_rx.recv().await {
                if ui_tx.send(Event::Input(ev)).await.is_err() {
                    break;
                }
            }
        }
    });

    let tick_rate = Duration::from_millis(100);
    let token_tick = shutdown_token.child_token();

    tokio::spawn(async move {
        loop {
            if token_tick.is_cancelled() {
                break;
            }
            if ui_tx_clone.send(Event::Tick).await.is_err() {
                break;
            }
            time::sleep(tick_rate).await;
        }
    });

    // Listen for Ctrl+C separately to immediately cancel the main loop
    tokio::spawn(async move {
        tokio::signal::ctrl_c()
            .await
            .expect("Failed to listen for Ctrl+C");
        shutdown_ctrlc.cancel();
    });

    let mut input_area = TextArea::default();
    input_area.set_style(Style::default().fg(Color::Blue));
    input_area.set_cursor_line_style(Style::default().fg(Color::LightGreen));

    let mut history: Vec<String> = Vec::new();
    let mut history_index: Option<usize> = None;
    let mut logger_state = TuiWidgetState::new();

    debug!("Starting REPL...");

    loop {
        let log_formatter = Box::new(MyLogFormatter);
        terminal.draw(|f| {
            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .margin(0)
                .constraints([Constraint::Fill(100), Constraint::Min(3)])
                .split(f.area());

            let logger_widget = TuiLoggerWidget::default()
                .block(
                    Block::default()
                        .fg(Color::LightBlue)
                        .borders(Borders::ALL)
                        .title(""),
                )
                .formatter(log_formatter)
                .state(&mut logger_state);
            f.render_widget(logger_widget, chunks[0]);

            input_area.set_block(
                Block::default()
                    .fg(Color::LightMagenta)
                    .borders(Borders::ALL)
                    .title(""),
            );
            input_area.set_style(Style::default().fg(Color::LightGreen));
            f.render_widget(&input_area, chunks[1]);
        })?;

        let ctrl_c = tokio::signal::ctrl_c();
        select! {
            _ = ctrl_c => {
                shutdown_token.cancel();
                break;
            }
            _ = shutdown_token.cancelled() => {
                error!("Shutdown initiated.");
                break;
            }
            Some(event) = ui_rx.recv() => {
                match event {
                    Event::Input(CrosstermEvent::Key(key_event)) => {
                        let tui_key_event = TuiKeyEvent::from(key_event);
                        // manually capture ctrl-c
                        if tui_key_event.modifiers == event::KeyModifiers::CONTROL && tui_key_event.code == KeyCode::Char('c') {
                            shutdown_token.cancel();
                            break;
                        }
                        match key_event.code {
                            KeyCode::Esc => {
                                shutdown_token.cancel();
                                break;
                            }
                            KeyCode::Enter => {
                                let user_input = input_area.lines().join("\n");
                                info!("INPUT:  {}", user_input);
                                history.push(user_input.clone());
                                history_index = None;
                                input_area = TextArea::default();

                                match interpret_input(&user_input, &mut interpreter).await {
                                    Ok(result) => warn!("OUTPUT: {}", result),
                                    Err(e) => error!("ERROR:  {}", e),
                                }
                            }
                            KeyCode::Up => {
                                if !history.is_empty() {
                                    let new_index = history_index.unwrap_or(history.len()).saturating_sub(1);
                                    history_index = Some(new_index);
                                    input_area = TextArea::from(vec![history[new_index].clone()]);
                                } else {
                                    input_area.input(tui_key_event);
                                }
                            }
                            KeyCode::Down => {
                                if let Some(idx) = history_index {
                                    if idx + 1 < history.len() {
                                        history_index = Some(idx + 1);
                                        input_area = TextArea::from(vec![history[idx + 1].clone()]);
                                    } else {
                                        history_index = None;
                                        input_area = TextArea::default();
                                    }
                                } else {
                                    input_area.input(tui_key_event);
                                }
                            }
                            _ => {
                                input_area.input(tui_key_event);
                            },
                        }
                    }
                    Event::Tick | Event::Input(_) => {}
                }
            }
        }
    }

    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;
    Ok(())
}
