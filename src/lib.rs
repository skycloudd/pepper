use lasso::ThreadedRodeo;
use std::sync::LazyLock;

pub mod diagnostics;
pub mod lexer;
pub mod parser;
pub mod scopes;
pub mod span;

static RODEO: LazyLock<ThreadedRodeo> = LazyLock::new(ThreadedRodeo::new);
