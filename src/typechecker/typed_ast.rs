use crate::{
    lexer::tokens::Span,
    parser::ast::{BinaryOp, FunctionId, Type, UnaryOp, VariableId},
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
    pub name_span: Span,

    pub return_type: Type,
    pub return_type_span: Span,

    #[return_ref]
    pub params: Vec<FunctionParameter<'db>>,
    pub params_span: Span,

    #[return_ref]
    pub body: TypedExpression<'db>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub struct FunctionParameter<'db> {
    pub name: VariableId<'db>,
    pub name_span: Span,

    pub type_: Type,
    pub type_span: Span,
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
