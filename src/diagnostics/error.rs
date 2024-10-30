use super::{Diag, ErrorSpan};
use crate::{
    parser::ast::{BinaryOp, Type, UnaryOp},
    span::Span,
    typecheck::typed_ast::Primitive,
};
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
    FunctionRedefinition {
        name: &'static str,
        new_span: crate::span::Span,
        previous_span: crate::span::Span,
    },
    UndefinedVariable {
        name: &'static str,
        span: Span,
    },
    ArgumentTypeMismatch {
        param_ty: Type<Primitive>,
        arg_ty: Type<Primitive>,
        param_span: Span,
        arg_span: Span,
    },
    UndefinedType {
        name: &'static str,
        span: Span,
    },
    BinaryOperatorTypeMismatch {
        op: BinaryOp,
        op_span: Span,
        lhs_ty: Type<Primitive>,
        rhs_ty: Type<Primitive>,
        lhs_span: Span,
        rhs_span: Span,
    },
    BodyTypeMismatch {
        return_ty: Type<Primitive>,
        body_ty: Type<Primitive>,
        return_span: Span,
        body_span: Span,
    },
    CantPerformOperation {
        op: BinaryOp,
        op_span: Span,
        lhs_ty: Type<Primitive>,
        rhs_ty: Type<Primitive>,
        lhs_span: Span,
        rhs_span: Span,
    },
    CantPerformUnaryOperation {
        op: UnaryOp,
        op_span: Span,
        ty: Type<Primitive>,
        span: Span,
    },
    ArgumentCountMismatch {
        expected: usize,
        found: usize,
        expected_span: Span,
        found_span: Span,
    },
    ArmTypeMismatch {
        arm_ty: Type<Primitive>,
        ty: Option<Type<Primitive>>,
        arm_span: Span,
        ty_span: Option<Span>,
        whole_span: Span,
    },
    CantCallType {
        ty: Type<Primitive>,
        span: Span,
    },
    PatternTypeMismatch {
        expected: Type<Primitive>,
        found: Type<Primitive>,
        expected_span: Span,
        found_span: Span,
    },
}

impl Diag for Error {
    #[allow(clippy::too_many_lines)]
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
            } => format!("Function {} is already defined", name.blue()).into(),
            Self::UndefinedVariable { name, span: _ } => {
                format!("Variable {} is not defined", name.blue()).into()
            }
            Self::ArgumentTypeMismatch {
                param_ty,
                arg_ty,
                param_span: _,
                arg_span: _,
            } => format!(
                "Expected an argument of type {}, but found {}",
                param_ty.yellow(),
                arg_ty.yellow()
            )
            .into(),
            Self::UndefinedType { name, span: _ } => {
                format!("Type {} is not defined", name.yellow()).into()
            }
            Self::BinaryOperatorTypeMismatch {
                op,
                op_span: _,
                lhs_ty,
                rhs_ty,
                lhs_span: _,
                rhs_span: _,
            } => format!(
                "Expected both operands of {} to be of the same type, but found {} and {}",
                op.yellow(),
                lhs_ty.yellow(),
                rhs_ty.yellow()
            )
            .into(),
            Self::BodyTypeMismatch {
                return_ty,
                body_ty,
                return_span: _,
                body_span: _,
            } => format!(
                "This function should return a value of type {}, but the body evaluates to {}",
                return_ty.yellow(),
                body_ty.yellow()
            )
            .into(),
            Self::CantPerformOperation {
                op,
                op_span: _,
                lhs_ty,
                rhs_ty,
                lhs_span: _,
                rhs_span: _,
            } => if lhs_ty == rhs_ty {
                format!(
                    "The operator {} cannot be applied to operands of type {}",
                    op.yellow(),
                    lhs_ty.yellow(),
                )
            } else {
                format!(
                    "The operator {} cannot be applied to operands of type {} and {}",
                    op.yellow(),
                    lhs_ty.yellow(),
                    rhs_ty.yellow()
                )
            }
            .into(),
            Self::CantPerformUnaryOperation {
                op,
                op_span: _,
                ty,
                span: _,
            } => format!(
                "The operator {} cannot be applied to an operand of type {}",
                op.yellow(),
                ty.yellow()
            )
            .into(),
            Self::ArgumentCountMismatch {
                expected,
                found,
                expected_span: _,
                found_span: _,
            } => format!(
                "Expected {} arguments, but found {}",
                expected.yellow(),
                found.yellow()
            )
            .into(),
            Self::ArmTypeMismatch {
                arm_ty,
                ty,
                arm_span: _,
                ty_span: _,
                whole_span: _,
            } => ty
                .as_ref()
                .map_or_else(
                    || format!("This arm evaluates to {}", arm_ty.yellow()),
                    |ty| {
                        format!(
                            "Incompatible types: {} and {}",
                            ty.yellow(),
                            arm_ty.yellow(),
                        )
                    },
                )
                .into(),
            Self::CantCallType { ty, span: _ } => {
                format!("Cannot call a value of type {}", ty.yellow()).into()
            }
            Self::PatternTypeMismatch {
                expected,
                found,
                expected_span: _,
                found_span: _,
            } => format!(
                "Expected a pattern of type {}, but found {}",
                expected.yellow(),
                found.yellow()
            )
            .into(),
        }
    }

    #[allow(clippy::too_many_lines)]
    fn spans(&self) -> Vec<ErrorSpan> {
        #[allow(clippy::match_same_arms)]
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
                ErrorSpan::primary_message("First defined here", *previous_span),
                ErrorSpan::primary_message("Redefinition here", *new_span),
            ],
            Self::ArgumentTypeMismatch {
                param_ty,
                arg_ty,
                param_span,
                arg_span,
            } => vec![
                ErrorSpan::primary_message(
                    format!("It should be of type: {param_ty}"),
                    *param_span,
                ),
                ErrorSpan::primary_message(
                    format!("This argument is of type: {arg_ty}"),
                    *arg_span,
                ),
            ],
            Self::UndefinedType { name: _, span } => vec![ErrorSpan::primary(*span)],
            Self::BinaryOperatorTypeMismatch {
                op: _,
                op_span,
                lhs_ty,
                rhs_ty,
                lhs_span,
                rhs_span,
            } => vec![
                ErrorSpan::primary_message(format!("{lhs_ty}"), *lhs_span),
                ErrorSpan::secondary(*op_span),
                ErrorSpan::primary_message(format!("{rhs_ty}"), *rhs_span),
            ],
            Self::BodyTypeMismatch {
                return_ty,
                body_ty,
                return_span,
                body_span,
            } => vec![
                ErrorSpan::primary_message(
                    format!("The expected return type is: {return_ty}"),
                    *return_span,
                ),
                ErrorSpan::primary_message(
                    format!("The function body evaluates to: {body_ty}"),
                    *body_span,
                ),
            ],
            Self::CantPerformOperation {
                op: _,
                op_span,
                lhs_ty,
                rhs_ty,
                lhs_span,
                rhs_span,
            } => vec![
                ErrorSpan::primary_message(format!("{lhs_ty}"), *lhs_span),
                ErrorSpan::secondary(*op_span),
                ErrorSpan::primary_message(format!("{rhs_ty}"), *rhs_span),
            ],
            Self::CantPerformUnaryOperation {
                op: _,
                op_span,
                ty,
                span,
            } => vec![
                ErrorSpan::primary_message(format!("{ty}"), *span),
                ErrorSpan::secondary(*op_span),
            ],
            Self::ArgumentCountMismatch {
                expected,
                found,
                expected_span,
                found_span,
            } => vec![
                ErrorSpan::primary_message(
                    format!("This function takes {expected} arguments"),
                    *expected_span,
                ),
                ErrorSpan::primary_message(
                    format!("It is being called with {found} arguments"),
                    *found_span,
                ),
            ],
            Self::ArmTypeMismatch {
                arm_ty,
                ty,
                arm_span,
                ty_span,
                whole_span,
            } => {
                let mut spans = vec![
                    ErrorSpan::primary_message(format!("This is of type: {arm_ty}"), *arm_span),
                    ErrorSpan::secondary_message(
                        "The values are outputs of this match expression",
                        *whole_span,
                    ),
                ];

                if let (Some(ty), Some(ty_span)) = (ty, ty_span) {
                    spans.push(ErrorSpan::primary_message(
                        format!("This is of type: {ty}"),
                        *ty_span,
                    ));
                }

                spans
            }
            Self::CantCallType { ty, span } => vec![ErrorSpan::primary_message(
                format!("This is of type: {ty}"),
                *span,
            )],
            Self::PatternTypeMismatch {
                expected,
                found,
                expected_span,
                found_span,
            } => vec![
                ErrorSpan::primary_message(
                    format!("This expression is of type: {expected}"),
                    *expected_span,
                ),
                ErrorSpan::primary_message(
                    format!("This pattern is of type: {found}"),
                    *found_span,
                ),
            ],
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            Self::FunctionRedefinition { .. } => {
                vec![format!("Functions must have unique names")]
            }
            Self::ArmTypeMismatch { .. } => {
                vec![format!(
                    "Outputs of match expressions must all evaluate to the same type"
                )]
            }
            Self::ExpectedFound { .. }
            | Self::Custom { .. }
            | Self::UndefinedVariable { .. }
            | Self::ArgumentTypeMismatch { .. }
            | Self::UndefinedType { .. }
            | Self::BinaryOperatorTypeMismatch { .. }
            | Self::BodyTypeMismatch { .. }
            | Self::CantPerformOperation { .. }
            | Self::CantPerformUnaryOperation { .. }
            | Self::ArgumentCountMismatch { .. }
            | Self::CantCallType { .. }
            | Self::PatternTypeMismatch { .. } => {
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
