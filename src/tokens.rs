use crate::ir::FileId;
use chumsky::prelude::*;
use ordered_float::OrderedFloat;

pub type Span<'db> = SimpleSpan<usize, FileId<'db>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Tokens<'db>(pub Vec<Token<'db>>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token<'db>(pub TokenKind<'db>, pub Span<'db>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind<'db> {
    Error,
    Simple(Simple),
    Parentheses(Tokens<'db>),
    CurlyBraces(Tokens<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Simple {
    Ident(String),
    Integer(i32),
    Float(OrderedFloat<f32>),
    Boolean(bool),
    Kw(Kw),
    Punc(Punc),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Kw {
    Fn,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Punc {
    Equals,

    Plus,
    Minus,
    Star,
    Slash,
}
