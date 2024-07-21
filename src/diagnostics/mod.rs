use crate::span::Span;
use codespan_reporting::diagnostic::{LabelStyle, Severity};
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
pub struct ErrorSpan {
    pub message: Option<String>,
    pub span: Span,
    pub error_type: ErrorType,
}

impl ErrorSpan {
    #[must_use]
    pub const fn primary(message: Option<String>, span: Span) -> Self {
        Self {
            message,
            span,
            error_type: ErrorType::Primary,
        }
    }

    #[must_use]
    pub const fn secondary(message: Option<String>, span: Span) -> Self {
        Self {
            message,
            span,
            error_type: ErrorType::Secondary,
        }
    }
}

#[derive(Debug)]
pub enum ErrorType {
    Primary,
    Secondary,
}

impl From<ErrorType> for LabelStyle {
    fn from(error_type: ErrorType) -> Self {
        match error_type {
            ErrorType::Primary => Self::Primary,
            ErrorType::Secondary => Self::Secondary,
        }
    }
}
