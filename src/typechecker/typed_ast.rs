#![allow(clippy::unused_unit)]

use crate::{
    lexer::tokens::Span,
    parser::ast::{FunctionId, VariableId},
};
use ordered_float::OrderedFloat;

#[salsa::tracked]
pub struct TypedProgram<'db> {
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
    pub body: TypedExpression<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub struct TypedExpression<'db> {
    pub span: Span,

    pub ty: Type,
    pub data: ExpressionData<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub enum ExpressionData<'db> {
    Integer(i32),
    Float(OrderedFloat<f32>),
    Variable(VariableId<'db>),
    Call(FunctionId<'db>, Vec<TypedExpression<'db>>),
    UnaryOp(UnaryOp, Box<TypedExpression<'db>>),
    BinaryOp(
        BinaryOp,
        Box<TypedExpression<'db>>,
        Box<TypedExpression<'db>>,
    ),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub enum Type {
    Integer,
    Float,
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
