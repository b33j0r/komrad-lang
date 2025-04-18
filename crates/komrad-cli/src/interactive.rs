use komrad_core::{Block as KomradBlock, Expr, Spanned, Statement, TopLevel};
use tracing::trace;

pub trait MakeInteractive {
    fn make_interactive(self) -> Self;
}

impl MakeInteractive for TopLevel {
    fn make_interactive(self) -> Self {
        match &self {
            TopLevel::Statement(s) => {
                match &*s.value {
                    Statement::Tell { target, value } => {
                        trace!("Converting Tell to Ask");
                        TopLevel::Statement(Spanned {
                            span: s.span.clone(),
                            value: Box::new(Statement::Expr(Spanned {
                                span: s.span.clone(),
                                value: Box::new(Expr::Ask {
                                    target: target.clone(),
                                    value: value.clone(),
                                }),
                            })),
                        })
                    }
                    _ => {
                        self
                    }
                }
            }
            TopLevel::Block(b) => {
                // convert the last statement to an interactive expression if it's a Tell
                let mut statements = b.0.clone();
                if let Some(last) = statements.last_mut() {
                    if let Statement::Tell { target, value } = &*last.value {
                        last.value = Box::new(Statement::Expr(Spanned {
                            span: last.span.clone(),
                            value: Box::new(Expr::Ask {
                                target: target.clone(),
                                value: value.clone(),
                            }),
                        }));
                    }
                }
                trace!("Converting block to interactive");
                TopLevel::Block(
                    KomradBlock(statements),
                )
            }
        }
    }
}