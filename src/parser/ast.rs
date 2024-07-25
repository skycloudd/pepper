use crate::span::Spanned;
use lasso::Spur;
use ordered_float::OrderedFloat;

#[derive(Clone, Debug)]
pub struct Ast {
    pub functions: Vec<Spanned<Function>>,
    pub structs: Vec<Spanned<Struct>>,
    pub use_stmts: Vec<Spanned<UseStatement>>,
    pub module_stmts: Vec<Spanned<ModuleStatement>>,
}

#[derive(Clone, Debug)]
pub struct UseStatement(pub Spanned<Path>);

#[derive(Clone, Debug)]
pub struct ModuleStatement(pub Spanned<Identifier>);

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Function {
    pub name: Spanned<Identifier>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub return_ty: Option<Spanned<Type>>,
    pub body: Spanned<Vec<Spanned<Statement>>>,
    pub return_expr: Option<Spanned<Expression>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionParam {
    pub name: Spanned<Identifier>,
    pub ty: Spanned<Type>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Statement {
    Expression(Spanned<Expression>),
    Block(Spanned<Vec<Spanned<Statement>>>),
    Let {
        name: Spanned<Identifier>,
        ty: Option<Spanned<Type>>,
        value: Spanned<Expression>,
    },
    Assign {
        name: Spanned<Identifier>,
        value: Spanned<Expression>,
    },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    Integer(Spanned<u128>),
    Float(Spanned<OrderedFloat<f64>>),
    Bool(Spanned<bool>),
    Variable(Spanned<Identifier>),
    BinaryOp {
        op: Spanned<BinaryOp>,
        lhs: Spanned<Box<Expression>>,
        rhs: Spanned<Box<Expression>>,
    },
    UnaryOp {
        op: Spanned<UnaryOp>,
        expr: Spanned<Box<Expression>>,
    },
    Call {
        name: Spanned<Path>,
        args: Spanned<Vec<Spanned<Expression>>>,
    },
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnaryOp {
    Neg,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Struct {
    pub name: Spanned<Identifier>,
    pub fields: Spanned<Vec<Spanned<StructField>>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct StructField {
    pub name: Spanned<Identifier>,
    pub ty: Spanned<Type>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Primitive(Spanned<PrimitiveType>),
    User(Spanned<Path>),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrimitiveType {
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,
    Bool,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Path(pub Spanned<Vec<Spanned<Identifier>>>);

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Identifier(pub Spanned<Spur>);
