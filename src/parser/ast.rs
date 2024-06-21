#![allow(clippy::unused_unit)]

use crate::lexer::tokens::Span;
use ordered_float::OrderedFloat;

#[salsa::tracked]
pub struct Program<'db> {
    #[return_ref]
    pub functions: Vec<Function<'db>>,
}

#[salsa::tracked]
pub struct Function<'db> {
    #[id]
    pub name: FunctionId<'db>,

    name_span: Span,

    #[return_ref]
    pub args: Vec<VariableId<'db>>,

    #[return_ref]
    pub body: Expression<'db>,
}

#[salsa::interned]
pub struct FunctionId<'db> {
    #[return_ref]
    pub text: String,
}

#[salsa::interned]
pub struct VariableId<'db> {
    #[return_ref]
    pub text: String,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub struct Expression<'db> {
    pub span: Span,

    pub data: ExpressionData<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub enum ExpressionData<'db> {
    Integer(i32),
    Float(OrderedFloat<f32>),
    Variable(VariableId<'db>),
    Call(FunctionId<'db>, Vec<Expression<'db>>),
    UnaryOp(UnaryOp, Box<Expression<'db>>),
    BinaryOp(BinaryOp, Box<Expression<'db>>, Box<Expression<'db>>),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub struct UnaryOp {
    pub span: Span,

    pub data: UnaryOpKind,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub enum UnaryOpKind {
    Negate,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub struct BinaryOp {
    pub span: Span,

    pub data: BinaryOpKind,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub enum BinaryOpKind {
    Add,
    Subtract,
    Multiply,
    Divide,
}
