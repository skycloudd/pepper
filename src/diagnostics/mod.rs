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
    message: Option<String>,
    span: Span,
    label_style: LabelStyle,
}

impl ErrorSpan {
    #[must_use]
    pub const fn primary(span: Span) -> Self {
        Self {
            message: None,
            span,
            label_style: LabelStyle::Primary,
        }
    }

    #[must_use]
    pub const fn primary_message(message: String, span: Span) -> Self {
        Self {
            message: Some(message),
            span,
            label_style: LabelStyle::Primary,
        }
    }

    #[must_use]
    pub const fn secondary(span: Span) -> Self {
        Self {
            message: None,
            span,
            label_style: LabelStyle::Secondary,
        }
    }

    #[must_use]
    pub const fn secondary_message(message: String, span: Span) -> Self {
        Self {
            message: Some(message),
            span,
            label_style: LabelStyle::Secondary,
        }
    }
}
