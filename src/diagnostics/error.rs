use super::{Diag, ErrorSpan};
use crate::{
    lexer::tokens::Span,
    parser::ast::{BinaryOp, Type, UnaryOp},
};
use chumsky::error::{Rich, RichReason};
use codespan_reporting::diagnostic::Severity;
use owo_colors::OwoColorize;

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
    BinaryOpTypeMismatch {
        op: BinaryOp,
        lhs_ty: Type,
        rhs_ty: Type,
        lhs_span: Span,
        rhs_span: Span,
    },
    UnaryOpTypeMismatch {
        op: UnaryOp,
        rhs_ty: Type,
        rhs_span: Span,
    },
    ExpectedReturnType {
        expected: Type,
        expected_span: Span,
        found: Type,
        found_span: Span,
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
            Self::BinaryOpTypeMismatch {
                op,
                lhs_ty,
                rhs_ty,
                lhs_span: _,
                rhs_span: _,
            } => format!(
                "Binary operator {} cannot be applied to types {} and {}",
                op.data.yellow(),
                lhs_ty.yellow(),
                rhs_ty.yellow(),
            ),
            Self::UnaryOpTypeMismatch {
                op,
                rhs_ty,
                rhs_span: _,
            } => format!(
                "Unary operator {} cannot be applied to type {}",
                op.data.yellow(),
                rhs_ty.yellow(),
            ),
            Self::ExpectedReturnType {
                expected,
                expected_span: _,
                found,
                found_span: _,
            } => format!(
                "Expected this function to return {}, but it returns {}",
                expected.yellow(),
                found.yellow()
            ),
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
                *span,
            )],
            Self::Custom { message: _, span } => vec![ErrorSpan::Primary(None, *span)],
            Self::BinaryOpTypeMismatch {
                op,
                lhs_ty,
                rhs_ty,
                lhs_span,
                rhs_span,
            } => vec![
                ErrorSpan::Primary(
                    Some(format!(
                        "Operator {} cannot be applied to these types",
                        op.data
                    )),
                    op.span,
                ),
                ErrorSpan::Secondary(Some(format!("{lhs_ty}")), *lhs_span),
                ErrorSpan::Secondary(Some(format!("{rhs_ty}")), *rhs_span),
            ],
            Self::UnaryOpTypeMismatch {
                op,
                rhs_ty,
                rhs_span,
            } => vec![
                ErrorSpan::Primary(
                    Some(format!(
                        "Operator {} cannot be applied to this type",
                        op.data
                    )),
                    op.span,
                ),
                ErrorSpan::Secondary(Some(format!("{rhs_ty}")), *rhs_span),
            ],
            Self::ExpectedReturnType {
                expected,
                expected_span,
                found,
                found_span,
            } => vec![
                ErrorSpan::Primary(Some(format!("Expected {expected}")), *expected_span),
                ErrorSpan::Primary(Some(format!("Found {found}")), *found_span),
            ],
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            Self::ExpectedFound { .. }
            | Self::Custom { .. }
            | Self::BinaryOpTypeMismatch { .. }
            | Self::UnaryOpTypeMismatch { .. }
            | Self::ExpectedReturnType { .. } => vec![],
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
                .flat_map(|r| convert_inner(r, span))
                .collect(),
        }
    }

    convert_inner(error.reason(), *error.span())
}
