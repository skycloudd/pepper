use crate::lexer::tokens::Span;
use chumsky::{
    error::{Rich, RichReason},
    span::Span as _,
};
use core::ops::Range;

#[derive(Clone, Debug)]
pub enum Error {
    ExpectedFound {
        expected: Vec<String>,
        found: Option<String>,
        span: Range<usize>,
    },
    Custom {
        message: String,
        span: Range<usize>,
    },
    Many(Vec<Error>),
}

fn convert_inner(reason: &RichReason<String, &str>, span: Range<usize>) -> Error {
    match reason {
        RichReason::ExpectedFound { expected, found } => Error::ExpectedFound {
            expected: expected.iter().map(ToString::to_string).collect(),
            found: found.as_ref().map(|f| f.to_string()),
            span,
        },
        RichReason::Custom(message) => Error::Custom {
            message: message.clone(),
            span,
        },
        RichReason::Many(reasons) => Error::Many(
            reasons
                .iter()
                .map(|r| convert_inner(r, span.clone()))
                .collect(),
        ),
    }
}

impl From<Rich<'_, String, Span, &str>> for Error {
    fn from(error: Rich<String, Span, &str>) -> Self {
        convert_inner(error.reason(), error.span().start()..error.span().end())
    }
}

impl From<&Rich<'_, String, Span, &str>> for Error {
    fn from(error: &Rich<String, Span, &str>) -> Self {
        convert_inner(error.reason(), error.span().start()..error.span().end())
    }
}
