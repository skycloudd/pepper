use super::{Diag, ErrorSpan};
use crate::span::Span;
use chumsky::error::{Rich, RichReason};
use codespan_reporting::diagnostic::Severity;
use core::fmt::Display;
use owo_colors::OwoColorize;
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
}

impl Diag for Error {
    fn message(&self) -> Cow<str> {
        match self {
            Self::ExpectedFound {
                expected,
                found,
                span: _,
            } => if expected.is_empty() {
                "Unexpected end of file".to_string()
            } else if expected.len() == 1 {
                format!(
                    "Expected {}, but found {}",
                    expected[0].green(),
                    found.as_deref().unwrap_or("something else")
                )
            } else {
                format!(
                    "Expected one of {}, but found {}",
                    expected
                        .iter()
                        .map(|expected| expected.green().to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    found.as_deref().unwrap_or("something else")
                )
            }
            .into(),
            Self::Custom { message, span: _ } => message.into(),
        }
    }

    fn spans(&self) -> Vec<ErrorSpan> {
        #[allow(clippy::match_same_arms)]
        match self {
            Self::ExpectedFound {
                expected: _,
                found,
                span,
            } => found.as_ref().map_or_else(
                || vec![ErrorSpan::primary(*span)],
                |found| vec![ErrorSpan::primary_message(format!("Found {found}"), *span)],
            ),
            Self::Custom { message: _, span } => vec![ErrorSpan::primary(*span)],
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            Self::ExpectedFound { .. } | Self::Custom { .. } => vec![],
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
