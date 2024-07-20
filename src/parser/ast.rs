use crate::span::Spanned;

#[derive(Clone, Debug)]
pub struct Program<'src>(pub Spanned<Vec<Spanned<Item<'src>>>>);

#[derive(Clone, Debug)]
pub enum Item<'src> {
    Function(Spanned<Function<'src>>),
    Struct(Spanned<Struct<'src>>),
}

#[derive(Clone, Debug)]
pub struct Function<'src> {
    pub name: Spanned<Identifier<'src>>,
    pub params: Spanned<Vec<Spanned<FunctionParam<'src>>>>,
    pub return_ty: Spanned<Type<'src>>,
    pub body: Spanned<Vec<Spanned<Statement<'src>>>>,
    pub return_expr: Option<Spanned<Expression<'src>>>,
}

#[derive(Clone, Debug)]
pub struct FunctionParam<'src> {
    pub name: Spanned<Identifier<'src>>,
    pub ty: Spanned<Type<'src>>,
}

#[derive(Clone, Debug)]
pub enum Statement<'src> {
    Expression(Spanned<Expression<'src>>),
    Block(Spanned<Vec<Spanned<Statement<'src>>>>),
    Let {
        name: Spanned<Identifier<'src>>,
        ty: Option<Spanned<Type<'src>>>,
        value: Spanned<Expression<'src>>,
    },
    Assign {
        name: Spanned<Identifier<'src>>,
        value: Spanned<Expression<'src>>,
    },
}

#[derive(Clone, Debug)]
pub enum Expression<'src> {
    Integer(Spanned<u128>),
    Float(Spanned<f64>),
    Bool(Spanned<bool>),
    Variable(Spanned<Identifier<'src>>),
    BinaryOp {
        op: Spanned<BinaryOp>,
        lhs: Spanned<Box<Expression<'src>>>,
        rhs: Spanned<Box<Expression<'src>>>,
    },
    UnaryOp {
        op: Spanned<UnaryOp>,
        expr: Spanned<Box<Expression<'src>>>,
    },
    Call {
        name: Spanned<Path<'src>>,
        args: Spanned<Vec<Spanned<Expression<'src>>>>,
    },
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Neg,
}

#[derive(Clone, Debug)]
pub struct Struct<'src> {
    pub name: Spanned<Identifier<'src>>,
    pub fields: Spanned<Vec<Spanned<StructField<'src>>>>,
}

#[derive(Clone, Debug)]
pub struct StructField<'src> {
    pub name: Spanned<Identifier<'src>>,
    pub ty: Spanned<Type<'src>>,
}

#[derive(Clone, Debug)]
pub enum Type<'src> {
    Primitive(Spanned<PrimitiveType>),
    User(Spanned<Path<'src>>),
}

#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Debug)]
pub struct Path<'src>(pub Spanned<Vec<Spanned<Identifier<'src>>>>);

#[derive(Clone, Copy, Debug)]
pub struct Identifier<'src>(pub Spanned<&'src str>);
