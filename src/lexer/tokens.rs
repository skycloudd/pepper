#![allow(clippy::unused_unit)]

use ordered_float::OrderedFloat;

#[salsa::input]
pub struct FileId {
    pub id: usize,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub file_id: FileId,
}

impl Span {
    #[must_use]
    pub const fn zero(file_id: FileId) -> Self {
        Self {
            start: 0,
            end: 0,
            file_id,
        }
    }
}

impl chumsky::span::Span for Span {
    type Context = FileId;

    type Offset = usize;

    fn new(context: Self::Context, range: core::ops::Range<Self::Offset>) -> Self {
        Self {
            start: range.start,
            end: range.end,
            file_id: context,
        }
    }

    fn context(&self) -> Self::Context {
        self.file_id
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

#[salsa::tracked]
pub struct Tokens<'db> {
    // #[return_ref]
    pub tokens: Vec<(TokenKind, Span)>,

    pub file_id: FileId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub enum TokenKind {
    Error,
    Simple(Simple),
    Parentheses(Vec<(TokenKind, Span)>),
    CurlyBraces(Vec<(TokenKind, Span)>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub enum Simple {
    Ident(String),
    Integer(i32),
    Float(OrderedFloat<f32>),
    Boolean(bool),
    Kw(Kw),
    Punc(Punc),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub enum Kw {
    Fn,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub enum Punc {
    Arrow,
    Equals,
    Colon,
    Comma,
    Plus,
    Minus,
    Star,
    Slash,
}

impl core::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Error => write!(f, "<error>"),
            Self::Simple(simple) => write!(f, "{simple}"),
            Self::Parentheses(tokens) => write!(
                f,
                "({})",
                tokens
                    .iter()
                    .map(|(kind, _)| kind.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::CurlyBraces(tokens) => write!(
                f,
                "{{{}}}",
                tokens
                    .iter()
                    .map(|(kind, _)| kind.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl core::fmt::Display for Simple {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Ident(name) => write!(f, "{name}"),
            Self::Integer(value) => write!(f, "{value}"),
            Self::Float(value) => write!(f, "{value}"),
            Self::Boolean(value) => write!(f, "{value}"),
            Self::Kw(kw) => write!(f, "{kw}"),
            Self::Punc(punc) => write!(f, "{punc}"),
        }
    }
}

impl core::fmt::Display for Kw {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Fn => write!(f, "fn"),
        }
    }
}

impl core::fmt::Display for Punc {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Arrow => write!(f, "->"),
            Self::Equals => write!(f, "="),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, ","),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
        }
    }
}
