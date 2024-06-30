use lexer::tokens::FileId;
use parser::ast;
use target_lexicon::Triple;
use typechecker::typed_ast;

pub mod codegen;
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
    typechecker::find_function_untyped,
    typechecker::find_function_typed,
    typechecker::validation::validate_main_function,
    typechecker::validation::validate_unique_function_names,
    typed_ast::TypedProgram<'_>,
    typed_ast::Function<'_>,
);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

#[salsa::tracked]
pub fn compile(
    db: &dyn crate::Db,
    source_program: SourceProgram,
    triple: Triple,
) -> Option<Vec<u8>> {
    let program = typechecker::typecheck(db, source_program)?;

    codegen::codegen(db, program, triple).map(|product| product.emit().unwrap())
}

#[salsa::input]
pub struct SourceProgram {
    #[return_ref]
    pub text: String,

    pub file_id: FileId,
}
