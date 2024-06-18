use ir::{FileId, SourceProgram};

mod db;
mod error;
pub mod ir;
mod lexer;
mod tokens;

pub use db::Database;

#[salsa::jar(db = Db)]
pub struct Jar(
    ir::Diagnostics,
    ir::SourceProgram,
    // ir::Program<'_>,
    // ir::Function<'_>,
    // ir::VariableId<'_>,
    // ir::FunctionId<'_>,
    ir::FileId<'_>,
    lexer::lex,
    compile,
);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

#[salsa::tracked]
pub fn compile<'db>(db: &'db dyn crate::Db, source_program: SourceProgram, filename: FileId<'db>) {
    let tokens = lexer::lex(db, source_program, filename);

    println!("Tokens: {tokens:?}");
}
