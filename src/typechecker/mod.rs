use crate::{diagnostics::error::Error, parser::ast::Ast};
use typed_ast::TypedAst;

pub mod typed_ast;

pub fn typecheck(ast: Ast, errors: &mut Vec<Error>) -> TypedAst {
    Typechecker::new(errors).typecheck(ast)
}

#[derive(Debug)]
struct Typechecker<'a> {
    errors: &'a mut Vec<Error>,
}

impl<'a> Typechecker<'a> {
    fn new(errors: &'a mut Vec<Error>) -> Self {
        Self { errors }
    }

    fn typecheck(&mut self, ast: Ast) -> TypedAst {
        TypedAst(vec![])
    }
}
