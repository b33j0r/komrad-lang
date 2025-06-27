// Re-export all components from the destructure module
mod base;
mod pattern;
mod predicate;
mod assignment;
mod command;

pub use base::*;
pub use pattern::*;
pub use predicate::*;
pub use assignment::*;
pub use command::*;