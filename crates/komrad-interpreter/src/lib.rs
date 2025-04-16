#![feature(box_patterns)]

mod interpreter;
mod dynamic_agent;
mod spawn_agent;
mod agents;

pub use interpreter::{Interpreter, InterpreterResult};
