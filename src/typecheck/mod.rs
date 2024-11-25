use crate::{diagnostics::error::Error, parser::ast::Ast};
use typed_ast::TypedAst;

pub mod typed_ast;

pub fn typecheck(ast: Ast) -> (TypedAst, Vec<Error>) {
    Typechecker::default().typecheck_ast(ast)
}

#[derive(Default)]
struct Typechecker {
    errors: Vec<Error>,
}

impl Typechecker {
    fn typecheck_ast(self, ast: Ast) -> (TypedAst, Vec<Error>) {
        (TypedAst, self.errors)
    }
}
