use lexer::tokens::FileId;
use owo_colors::OwoColorize as _;
use parser::ast;
use salsa::DebugWithDb as _;
use typechecker::typed_ast;

pub mod db;
pub mod diagnostics;
pub mod lexer;
pub mod parser;
pub mod typechecker;

#[salsa::jar(db = Db)]
pub struct Jar(
    diagnostics::Diagnostics,
    SourceProgram,
    compile,
    lexer::lex,
    lexer::tokens::FileId,
    lexer::tokens::Tokens<'_>,
    ast::Program<'_>,
    ast::Function<'_>,
    ast::FunctionId<'_>,
    ast::VariableId<'_>,
    parser::parse,
    typechecker::typecheck,
    typechecker::typecheck_function,
    typed_ast::TypedProgram<'_>,
    typed_ast::Function<'_>,
);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

#[salsa::tracked]
pub fn compile(db: &dyn crate::Db, source_program: SourceProgram) {
    let program = typechecker::typecheck(db, source_program);

    println!("Program: {:?}", program.debug(db).green());
}

#[salsa::input]
pub struct SourceProgram {
    #[return_ref]
    pub text: String,

    pub file_id: FileId,
}
