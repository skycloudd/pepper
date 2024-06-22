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
    pub name_span: Span,

    pub return_type: Type,
    pub return_type_span: Span,

    #[return_ref]
    pub params: Vec<FunctionParameter<'db>>,
    pub params_span: Span,

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

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub struct FunctionParameter<'db> {
    pub name: VariableId<'db>,
    pub name_span: Span,

    pub type_: Type,
    pub type_span: Span,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
pub enum Type {
    Error,
    Integer,
    Float,
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

impl core::fmt::Display for UnaryOpKind {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Negate => write!(f, "-"),
        }
    }
}

impl core::fmt::Display for BinaryOpKind {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Subtract => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
        }
    }
}

impl core::fmt::Display for Type {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Error => write!(f, "<error>"),
            Self::Integer => write!(f, "int"),
            Self::Float => write!(f, "float"),
        }
    }
}
