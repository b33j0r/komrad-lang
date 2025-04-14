// Import the ratatui key types.
use ratatui::crossterm::event::{KeyCode as TuiKeyCode, KeyEvent as TuiKeyEvent};
use ratatui::crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event as CrosstermEvent, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::prelude::{Line, Text};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout},
    style::{Color, Style},
    widgets::{Block, Borders, Paragraph},
    Terminal,
};
use std::{
    error::Error,
    io,
    sync::mpsc,
    thread,
    time::{Duration, Instant},
};
use tokio::{select, sync::mpsc as async_mpsc, time};
use tracing::{debug, error, info, warn};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::registry::Registry;
use tracing_subscriber::{EnvFilter, Layer};
use tui_logger::{ExtLogRecord, LevelFilter, LogFormatter, TuiLoggerWidget, TuiTracingSubscriberLayer, TuiWidgetState};
use tui_textarea::TextArea;

use komrad_core::ToSExpr;
use komrad_parser::parser::parse_snippet_complete;

/// An enum for our internal events.
enum Event<I> {
    Input(I),
    Tick,
}

/// Spawn a blocking thread to read crossterm events and send them over a standard mpsc channel.
fn spawn_blocking_event_reader(tx: mpsc::Sender<CrosstermEvent>) {
    thread::spawn(move || {
        loop {
            if let Ok(ev) = event::read() {
                if tx.send(ev).is_err() {
                    break;
                }
            }
        }
    });
}

/// Interpreter hook.
async fn interpret_input(input: &str) -> Result<String, Box<dyn Error>> {
    let mut codemaps = komrad_core::CodeMaps::new();
    let top_level = parse_snippet_complete(&mut codemaps, input)
        .map_err(|e| format!("Parse error: {:?}", e))?;
    let sexpr = top_level.to_sexpr();
    debug!("SEXPR: {:}", sexpr.to_colored_string());

    // Simulate a computation delay.
    time::sleep(Duration::from_millis(0)).await;
    Ok("Interpretation result".to_string())
}

struct MyLogFormatter;

impl LogFormatter for MyLogFormatter {
    fn min_width(&self) -> u16 {
        return 8;
    }

    fn format(&self, width: usize, evt: &ExtLogRecord) -> Vec<Line> {
        let color = match evt.level {
            log::Level::Trace => Color::Gray,
            log::Level::Debug => Color::Yellow,
            log::Level::Info => Color::Green,
            log::Level::Warn => Color::Magenta,
            log::Level::Error => Color::Red,
        };
        textwrap::wrap(
            evt.msg(),
            width as usize,
        ).iter().map(
            |line| {
                // Create a Span with the styling instead of Text
                let span = ratatui::prelude::Span::styled(
                    line.to_string(),
                    Style::default().fg(color),
                );
                // Line can be created from Span
                Line::from(span)
            }
        ).collect()
    }
}

pub async fn main() -> Result<(), Box<dyn Error>> {
    // // --- Setup tracing with tui-logger integration ---
    tui_logger::init_logger(LevelFilter::Trace).unwrap();
    //
    // // Set the default level for unknown targets
    // tui_logger::set_default_level(LevelFilter::Trace);
    //
    // // Initialize the tracing subscriber with tui-logger layer
    // let subscriber = Registry::default().with(TuiTracingSubscriberLayer);
    // tracing::subscriber::set_global_default(subscriber).expect("Failed to set tracing subscriber");

    let subscriber = Registry::default().with(
        TuiTracingSubscriberLayer
    );
    tracing::subscriber::set_global_default(subscriber).expect("Failed to set tracing subscriber");

    // --- Setup Terminal ---
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // --- Setup event channels ---
    let (blocking_tx, blocking_rx) = mpsc::channel::<CrosstermEvent>();
    spawn_blocking_event_reader(blocking_tx);

    let (ui_tx, mut ui_rx) = async_mpsc::channel::<Event<CrosstermEvent>>(100);
    // Forward events from the blocking channel into the async channel.
    let ui_tx_clone = ui_tx.clone();
    tokio::spawn(async move {
        while let Ok(ev) = blocking_rx.recv() {
            if ui_tx.send(Event::Input(ev)).await.is_err() {
                break;
            }
        }
    });
    // Spawn tick task for periodic UI updates.
    let tick_rate = Duration::from_millis(100);
    tokio::spawn(async move {
        let mut last_tick = Instant::now();
        loop {
            if last_tick.elapsed() >= tick_rate {
                if ui_tx_clone.send(Event::Tick).await.is_err() {
                    break;
                }
                last_tick = Instant::now();
            }
            time::sleep(Duration::from_millis(10)).await;
        }
    });

    // --- Setup UI state ---
    // Initialize tui-textarea for the input.
    let mut input_area = TextArea::default();
    input_area.set_style(Style::default().fg(Color::Blue));
    // Optionally, set the cursor line style.
    input_area.set_cursor_line_style(Style::default().fg(Color::Yellow));

    // REPL history state.
    let mut history: Vec<String> = Vec::new();
    let mut history_index: Option<usize> = None;

    // A vector to store command log or messages.
    let mut logger_state = TuiWidgetState::new();

    // --- Main UI Loop ---
    loop {
        let log_formatter = Box::new(MyLogFormatter);
        // -------- Draw the interface --------
        terminal.draw(|f| {
            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .margin(1)
                // Use `.area()` instead of `.size()`
                .constraints([
                    Constraint::Percentage(80), // Log display
                    Constraint::Length(3),      // Input area
                ])
                .split(f.area());

            // Render the tui-logger widget:
            let logger_widget = TuiLoggerWidget::default()
                .block(Block::default().borders(Borders::ALL).title("Output"))
                .formatter(log_formatter)
                .state(&mut logger_state);
            f.render_widget(logger_widget, chunks[0]);

            // Render the input area.
            input_area.set_block(Block::default().borders(Borders::ALL).title("Input"));
            f.render_widget(&input_area, chunks[1]);
        })?;

        // -------- Handle input events --------
        select! {
            _ = tokio::signal::ctrl_c() => {
                error!("Received Ctrl-C, exiting...");
                break;
            }
            Some(event) = ui_rx.recv() => {
                match event {
                    Event::Input(CrosstermEvent::Key(key_event)) => {
                        // To pass the key event to `TextArea::input`, we manually convert it.
                        // Create a ratatui key event by converting the key code using its From implementation.
                        let tui_key_event = TuiKeyEvent::from(key_event);

                        match key_event.code {
                            KeyCode::Esc => break, // Exit on Escape

                            KeyCode::Enter => {
                                // On Enter, gather the input, store it, clear the textarea, and process it.
                                let user_input = input_area.lines().join("\n");
                                info!("INPUT: {}", user_input);
                                history.push(user_input.clone());
                                history_index = None;
                                input_area = TextArea::default();

                                match interpret_input(&user_input).await {
                                    Ok(result) => warn!("OUTPUT: {}", result),
                                    Err(e) => error!("Error: {}", e),
                                }
                            }

                            KeyCode::Up => {
                                if !history.is_empty() {
                                    let new_index = match history_index {
                                        Some(idx) if idx > 0 => idx - 1,
                                        None => history.len() - 1,
                                        _ => 0,
                                    };
                                    history_index = Some(new_index);
                                    // Convert the history entry into the expected type (a Vec<String>).
                                    input_area = TextArea::from(vec![history[new_index].clone()]);
                                } else {
                                    input_area.input(tui_key_event);
                                }
                            }
                            KeyCode::Down => {
                                if let Some(idx) = history_index {
                                    let new_index = if idx + 1 < history.len() {
                                        idx + 1
                                    } else {
                                        history_index = None;
                                        input_area = TextArea::default();
                                        continue;
                                    };
                                    history_index = Some(new_index);
                                    input_area = TextArea::from(vec![history[new_index].clone()]);
                                } else {
                                    input_area.input(tui_key_event);
                                }
                            }

                            // For all other keys, forward the converted key event.
                            _ => {
                                input_area.input(tui_key_event);
                            }
                        }
                    }
                    // Ignore non-key events.
                    Event::Input(_) => {}
                    Event::Tick => {
                        // Periodic tasks can be added here.
                    }
                }
            }
        }
    }

    // --- Cleanup Terminal ---
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;
    Ok(())
}
