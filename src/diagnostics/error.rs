use super::{Diag, ErrorSpan};
use crate::{span::Span, typecheck::TyName};
use chumsky::error::{Rich, RichReason};
use codespan_reporting::diagnostic::Severity;
use core::fmt::Display;
use polytype::UnificationError;
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
    UnificationFailure {
        lhs_type: String,
        lhs_span: Span,
        rhs_type: String,
        rhs_span: Span,
    },
    CantFindType {
        name: &'static str,
        span: Span,
    },
    CantFindFunction {
        name: &'static str,
        span: Span,
    },
    NotFunction {
        span: Span,
    },
}

impl Error {
    pub fn from_unification_error(
        value: UnificationError<TyName>,
        lhs_span: Span,
        rhs_span: Span,
    ) -> Self {
        match value {
            polytype::UnificationError::Occurs(_id) => todo!(),
            polytype::UnificationError::Failure(failed_lhs, failed_rhs) => {
                Self::UnificationFailure {
                    lhs_type: failed_lhs.to_string(),
                    lhs_span,
                    rhs_type: failed_rhs.to_string(),
                    rhs_span,
                }
            }
        }
    }
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
            Self::UnificationFailure {
                lhs_type,
                lhs_span: _,
                rhs_type,
                rhs_span: _,
            } => format!("Cannot unify types {lhs_type} and {rhs_type}").into(),
            Self::CantFindType { name, span: _ } => {
                format!("Cannot find type with name {name}").into()
            }
            Self::CantFindFunction { name, span: _ } => {
                format!("Cannot find function with name {name}").into()
            }
            Self::NotFunction { span: _ } => "This expression is not a function".into(),
        }
    }

    #[allow(clippy::match_same_arms)]
    fn spans(&self) -> Vec<ErrorSpan> {
        match self {
            Self::ExpectedFound {
                expected: _,
                found,
                span,
            } => vec![ErrorSpan::primary(
                Some(format!(
                    "Found {}",
                    found.as_deref().unwrap_or("end of file")
                )),
                *span,
            )],
            Self::Custom { message: _, span } => vec![ErrorSpan::primary(None, *span)],
            Self::UnificationFailure {
                lhs_type,
                lhs_span,
                rhs_type,
                rhs_span,
            } => vec![
                ErrorSpan::primary(Some(lhs_type.to_string()), *lhs_span),
                ErrorSpan::primary(Some(rhs_type.to_string()), *rhs_span),
            ],
            Self::CantFindType { name: _, span } => vec![ErrorSpan::primary(None, *span)],
            Self::CantFindFunction { name: _, span } => vec![ErrorSpan::primary(None, *span)],
            Self::NotFunction { span } => vec![ErrorSpan::primary(
                Some("Must be a function".to_string()),
                *span,
            )],
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            Self::ExpectedFound { .. }
            | Self::Custom { .. }
            | Self::UnificationFailure { .. }
            | Self::CantFindType { .. }
            | Self::CantFindFunction { .. }
            | Self::NotFunction { .. } => {
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
