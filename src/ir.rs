use crate::error::Error;
use camino::Utf8PathBuf;

#[salsa::accumulator]
pub struct Diagnostics(pub Error);

#[salsa::input]
pub struct SourceProgram {
    #[return_ref]
    pub text: String,
}

// #[salsa::tracked]
// pub struct Program<'db> {
//     #[return_ref]
//     pub statements: Vec<Function<'db>>,
// }

// #[salsa::tracked]
// pub struct Function<'db> {
//     #[id]
//     pub name: FunctionId<'db>,

//     name_span: Span<'db>,

//     #[return_ref]
//     pub args: Vec<VariableId<'db>>,

//     #[return_ref]
//     pub body: Expression<'db>,
// }

// #[derive(Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
// pub struct Expression<'db> {
//     pub span: Span<'db>,
//     pub data: ExpressionData<'db>,
// }

// #[derive(Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
// pub enum ExpressionData<'db> {
//     Number(OrderedFloat<f64>),
//     BinaryOp(Op, Box<Expression<'db>>, Box<Expression<'db>>),
//     Variable(VariableId<'db>),
//     Call(FunctionId<'db>, Vec<Expression<'db>>),
// }

// #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, salsa::DebugWithDb, salsa::Update)]
// pub enum Op {
//     Add,
//     Sub,
//     Mul,
//     Div,
// }

// #[salsa::interned]
// pub struct VariableId<'db> {
//     #[return_ref]
//     pub text: String,
// }

// #[salsa::interned]
// pub struct FunctionId<'db> {
//     #[return_ref]
//     pub text: String,
// }

#[salsa::interned]
pub struct FileId<'db> {
    #[return_ref]
    pub path: Utf8PathBuf,
}
