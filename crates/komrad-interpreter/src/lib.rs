#![feature(box_patterns)]

mod interpreter;
mod dynamic_agent;
mod spawn_agent;
mod fs_agent;
mod io_agent;

pub use interpreter::{Interpreter, InterpreterResult};
