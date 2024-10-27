use super::{Diag, ErrorSpan};
use crate::span::Span;
use chumsky::error::{Rich, RichReason};
use codespan_reporting::diagnostic::Severity;
use core::fmt::Display;
use std::borrow::Cow;

#[derive(Clone, Debug)]
pub enum Error {
    ExpectedFound {
        expected: Vec<String>,
        found: Option<String>,
        span: Span,
    },
    Custom {
        message: String,
        span: Span,
    },
    FunctionRedefinition {
        name: &'static str,
        new_span: crate::span::Span,
        previous_span: crate::span::Span,
    },
    UndefinedVariable {
        name: &'static str,
        span: Span,
    },
}

impl Diag for Error {
    fn message(&self) -> Cow<str> {
        match self {
            Self::ExpectedFound {
                expected,
                found,
                span: _,
            } => format!(
                "Expected one of {}, but found {}",
                expected.join(", "),
                found.as_deref().unwrap_or("end of file")
            )
            .into(),
            Self::Custom { message, span: _ } => message.into(),
            Self::FunctionRedefinition {
                name,
                new_span: _,
                previous_span: _,
            } => format!("Function '{name}' has already been defined").into(),
            Self::UndefinedVariable { name, span: _ } => {
                format!("Variable '{name}' has not been defined").into()
            }
        }
    }

    fn spans(&self) -> Vec<ErrorSpan> {
        match self {
            Self::ExpectedFound {
                expected: _,
                found,
                span,
            } => vec![ErrorSpan::primary_message(
                format!("Found {}", found.as_deref().unwrap_or("end of file")),
                *span,
            )],
            Self::Custom { message: _, span } | Self::UndefinedVariable { name: _, span } => {
                vec![ErrorSpan::primary(*span)]
            }
            Self::FunctionRedefinition {
                name: _,
                new_span,
                previous_span,
            } => vec![
                ErrorSpan::primary_message("First defined here".into(), *previous_span),
                ErrorSpan::primary_message("Redefinition here".into(), *new_span),
            ],
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            Self::FunctionRedefinition { .. } => {
                vec![format!("Functions must have unique names")]
            }
            Self::ExpectedFound { .. } | Self::Custom { .. } | Self::UndefinedVariable { .. } => {
                vec![]
            }
        }
    }

    fn kind(&self) -> Severity {
        Severity::Error
    }
}

#[must_use]
pub fn convert(error: &Rich<impl Display, Span, &str>) -> Vec<Error> {
    fn convert_inner(reason: &RichReason<impl Display, &str>, span: Span) -> Vec<Error> {
        match reason {
            RichReason::ExpectedFound { expected, found } => vec![Error::ExpectedFound {
                expected: expected.iter().map(ToString::to_string).collect(),
                found: found.as_ref().map(|f| f.to_string()),
                span,
            }],
            RichReason::Custom(message) => vec![Error::Custom {
                message: message.to_owned(),
                span,
            }],
            RichReason::Many(reasons) => reasons
                .iter()
                .flat_map(|r| convert_inner(r, span))
                .collect(),
        }
    }

    convert_inner(error.reason(), *error.span())
}
