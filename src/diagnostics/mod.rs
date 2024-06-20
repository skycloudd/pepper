use crate::lexer::tokens::Span;
use codespan_reporting::diagnostic::Severity;
use error::Error;

pub mod error;
pub mod report;

pub trait Diag {
    fn message(&self) -> String;
    fn spans(&self) -> Vec<ErrorSpan>;
    fn notes(&self) -> Vec<String>;
    fn kind(&self) -> Severity;
}

#[derive(Debug)]
pub enum ErrorSpan {
    Primary(Option<String>, Span),
    Secondary(Option<String>, Span),
}

#[salsa::accumulator]
pub struct Diagnostics(pub Error);
