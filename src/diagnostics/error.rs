use super::{Diag, ErrorSpan};
use crate::lexer::tokens::Span;
use chumsky::error::{Rich, RichReason};
use codespan_reporting::diagnostic::Severity;

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
    fn message(&self) -> String {
        match self {
            Self::ExpectedFound {
                expected,
                found,
                span: _,
            } => format!(
                "Expected one of {}, but found {}",
                expected.join(", "),
                found.as_deref().unwrap_or("end of file")
            ),
            Self::Custom { message, span: _ } => message.clone(),
        }
    }

    fn spans(&self) -> Vec<ErrorSpan> {
        match self {
            Self::ExpectedFound {
                expected: _,
                found,
                span,
            } => vec![ErrorSpan::Primary(
                Some(format!(
                    "Found {}",
                    found.as_deref().unwrap_or("end of file")
                )),
                span.clone(),
            )],
            Self::Custom { message: _, span } => vec![ErrorSpan::Primary(None, span.clone())],
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
pub fn convert(error: &Rich<String, Span, &str>) -> Vec<Error> {
    fn convert_inner(reason: &RichReason<String, &str>, span: Span) -> Vec<Error> {
        match reason {
            RichReason::ExpectedFound { expected, found } => vec![Error::ExpectedFound {
                expected: expected.iter().map(ToString::to_string).collect(),
                found: found.as_ref().map(|f| f.to_string()),
                span,
            }],
            RichReason::Custom(message) => vec![Error::Custom {
                message: message.clone(),
                span,
            }],
            RichReason::Many(reasons) => reasons
                .iter()
                .flat_map(|r| convert_inner(r, span.clone()))
                .collect(),
        }
    }

    convert_inner(error.reason(), error.span().clone())
}
