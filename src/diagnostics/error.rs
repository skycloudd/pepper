use super::{Diag, ErrorSpan};
use crate::{
    parser::ast::BinaryOp,
    span::{Span, Spanned},
    typecheck::{engine::TypeInfo, typed_ast::Type},
};
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
    UndefinedType {
        name: String,
        span: Span,
    },
    CantInferType {
        span: Span,
    },
    UndefinedFunction {
        name: String,
        span: Span,
    },
    TypeMismatch {
        expected: Spanned<TypeInfo>,
        found: Spanned<TypeInfo>,
    },
    UndefinedVariable {
        name: String,
        span: Span,
    },
    BinaryOpTypeMismatch {
        expected: Spanned<Type>,
        found: Spanned<Type>,
        op: Spanned<BinaryOp>,
    },
}

impl Diag for Error {
    #[allow(clippy::match_same_arms)]
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
            Self::UndefinedType { name, span: _ } => format!("Undefined type `{name}`").into(),
            Self::CantInferType { span: _ } => "Can't infer type".into(),
            Self::UndefinedFunction { name, span: _ } => {
                format!("Undefined function `{name}`").into()
            }
            Self::TypeMismatch { expected, found } => {
                type_mismatch_message(&expected.0, &found.0).into()
            }
            Self::UndefinedVariable { name, span: _ } => {
                format!("Undefined variable `{name}`").into()
            }
            Self::BinaryOpTypeMismatch {
                expected,
                found,
                op,
            } => format!(
                "Can't apply operator `{}` to types `{}` and `{}`",
                op.0, expected.0, found.0
            )
            .into(),
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
            Self::UndefinedType { name, span } => vec![ErrorSpan::primary(
                Some(format!("Undefined type `{name}`")),
                *span,
            )],
            Self::CantInferType { span } => vec![ErrorSpan::primary(None, *span)],
            Self::UndefinedFunction { name, span } => vec![ErrorSpan::primary(
                Some(format!("Undefined function `{name}`")),
                *span,
            )],
            Self::TypeMismatch { expected, found } => vec![
                ErrorSpan::primary(
                    Some("Expected {todo add info here}".to_string()),
                    expected.1,
                ),
                ErrorSpan::primary(Some("Found {todo add info here}".to_string()), found.1),
            ],
            Self::UndefinedVariable { name, span } => vec![ErrorSpan::primary(
                Some(format!("Undefined variable `{name}`")),
                *span,
            )],
            Self::BinaryOpTypeMismatch {
                expected,
                found,
                op: _,
            } => vec![
                ErrorSpan::primary(Some(format!("Expected `{}`", expected.0)), expected.1),
                ErrorSpan::primary(Some(format!("Found `{}`", found.0)), found.1),
            ],
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            Self::ExpectedFound { .. }
            | Self::Custom { .. }
            | Self::UndefinedType { .. }
            | Self::CantInferType { .. }
            | Self::UndefinedFunction { .. }
            | Self::TypeMismatch { .. }
            | Self::UndefinedVariable { .. }
            | Self::BinaryOpTypeMismatch { .. } => vec![],
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

fn type_mismatch_message<'a>(expected: &TypeInfo, found: &TypeInfo) -> impl Into<Cow<'a, str>> {
    format!(
        "{}{}",
        match expected {
            TypeInfo::Unknown | TypeInfo::Ref(_) | TypeInfo::Error =>
                Cow::Borrowed("Expected a type, "),
            TypeInfo::Primitive(prim) => format!("Expected `{prim}`, ").into(),
        },
        match found {
            TypeInfo::Unknown | TypeInfo::Ref(_) | TypeInfo::Error =>
                Cow::Borrowed("but found something else"),
            TypeInfo::Primitive(prim) => format!("but found `{prim}`").into(),
        }
    )
}
