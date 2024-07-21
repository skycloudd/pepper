use crate::span::Span;
use codespan_reporting::diagnostic::Severity;
use std::borrow::Cow;

pub mod error;
pub mod report;

pub trait Diag {
    fn message(&self) -> Cow<str>;
    fn spans(&self) -> Vec<ErrorSpan>;
    fn notes(&self) -> Vec<String>;
    fn kind(&self) -> Severity;
}

#[derive(Debug)]
pub enum ErrorSpan {
    Primary(Option<String>, Span),
    Secondary(Option<String>, Span),
}
