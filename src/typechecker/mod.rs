use crate::{
    parser::{self},
    SourceProgram,
};
use owo_colors::OwoColorize;
use salsa::DebugWithDb;
use typed_ast::TypedProgram;

pub mod typed_ast;

#[salsa::tracked]
pub fn typecheck(db: &dyn crate::Db, source_program: SourceProgram) -> Option<TypedProgram<'_>> {
    let program = parser::parse(db, source_program)?;

    let functions = program
        .functions(db)
        .iter()
        .map(|function| typecheck_function(db, *function))
        .collect();

    Some(TypedProgram::new(db, functions))
}

#[salsa::tracked]
pub fn typecheck_function<'db>(
    db: &'db dyn crate::Db,
    function: parser::ast::Function<'db>,
) -> typed_ast::Function<'db> {
    println!("Typechecking function: {:?}", function.debug(db).yellow());

    todo!()
}
