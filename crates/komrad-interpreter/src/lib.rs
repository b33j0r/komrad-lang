#![feature(box_patterns)]

mod interpreter;
mod dynamic_agent;
mod spawn_agent;

pub use interpreter::{Interpreter, InterpreterResult};
