use lexer::tokens::FileId;
use owo_colors::OwoColorize;
use parser::ast;
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
pub fn compile(db: &dyn crate::Db, source_program: SourceProgram) {
    let program = typechecker::typecheck(db, source_program);

    if let Some(program) = program {
        println!("{}", "typechecking succeeded".green());
        println!(
            "typechecked {} functions",
            program.functions(db).len().yellow()
        );
        for function in program.functions(db) {
            println!(
                "    {} {}{}{}{} {} {}",
                "fn".dimmed(),
                function.name(db).text(db).blue(),
                '('.dimmed(),
                function
                    .params(db)
                    .iter()
                    .map(|param| param.type_.yellow().to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ')'.dimmed(),
                "->".dimmed(),
                function.return_type(db).yellow(),
            );
        }
    }
}

#[salsa::input]
pub struct SourceProgram {
    #[return_ref]
    pub text: String,

    pub file_id: FileId,
}
