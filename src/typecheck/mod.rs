use crate::{
    diagnostics::error::Error,
    parser::ast::{self, Ast},
};
use typed_ast::{TopLevel, TypedAst};

pub mod typed_ast;

pub fn typecheck(ast: Ast) -> (TypedAst, Vec<Error>) {
    Typechecker::default().typecheck_ast(ast)
}

#[derive(Default)]
struct Typechecker {
    errors: ErrorVec,
}

impl Typechecker {
    fn typecheck_ast(mut self, ast: Ast) -> (TypedAst, Vec<Error>) {
        (
            TypedAst(
                ast.0
                    .into_iter()
                    .map(|toplevel| toplevel.map(|toplevel| self.typecheck_toplevel(toplevel)))
                    .collect(),
            ),
            self.errors.0,
        )
    }

    fn typecheck_toplevel(&mut self, toplevel: ast::TopLevel) -> TopLevel {
        todo!()
    }
}

#[derive(Default)]
struct ErrorVec(Vec<Error>);

impl ErrorVec {
    fn push(&mut self, error: Error) {
        self.0.push(error);
    }
}
