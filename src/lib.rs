use lexer::tokens::FileId;
use owo_colors::OwoColorize as _;
use parser::ast;
use salsa::DebugWithDb;

pub mod db;
pub mod diagnostics;
pub mod lexer;
pub mod parser;

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
);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

#[salsa::tracked]
pub fn compile(db: &dyn crate::Db, source_program: SourceProgram) {
    let program = parser::parse(db, source_program);

    println!("Program: {:?}", program.debug(db).green());
}

#[salsa::input]
pub struct SourceProgram {
    #[return_ref]
    pub text: String,

    pub file_id: FileId,
}
