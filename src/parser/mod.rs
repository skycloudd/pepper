use crate::{
    diagnostics::{error::Error, Diagnostics},
    lexer::{
        self,
        tokens::{Kw, Punc, Simple, Span, TokenKind},
    },
    SourceProgram,
};
use ast::{Expression, ExpressionData, Function, FunctionId, Program, VariableId};
use chumsky::{input::SpannedInput, prelude::*};

pub mod ast;

#[salsa::tracked]
pub fn parse(db: &dyn crate::Db, source_program: SourceProgram) -> Option<Program<'_>> {
    let tokens = lexer::lex(db, source_program);

    tokens.and_then(|tokens| {
        let eoi = tokens
            .tokens(db)
            .last()
            .map_or_else(|| Span::new(tokens.file_id(db), 0..0), |t| t.1.to_end());

        let tokens = tokens.tokens(db);

        let (program, errors) = parser(db).parse(tokens.spanned(eoi)).into_output_errors();

        for err in errors {
            Diagnostics::push(
                db,
                Error::from(err.clone().map_token(|token| token.to_string())),
            );
        }

        program
    })
}

type ParserInput<'tok> = SpannedInput<TokenKind, Span, &'tok [(TokenKind, Span)]>;

type ParserExtra<'db, 'src, 'tok> = extra::Err<Rich<'tok, TokenKind, Span, &'src str>>;

#[must_use]
fn parser<'db: 'tok, 'src: 'tok, 'tok>(
    db: &'db dyn crate::Db,
) -> impl Parser<'tok, ParserInput<'tok>, Program<'db>, ParserExtra<'db, 'src, 'tok>> {
    function_parser(db)
        .repeated()
        .collect()
        .map(|functions| Program::new(db, functions))
        .boxed()
}

fn function_parser<'db: 'tok, 'src: 'tok, 'tok>(
    db: &'db dyn crate::Db,
) -> impl Parser<'tok, ParserInput<'tok>, Function<'db>, ParserExtra<'db, 'src, 'tok>> {
    let name = function_id_parser(db).map_with(|name, e| (name, e.span()));

    let args = parenthesised(
        variable_id_parser(db)
            .separated_by(just(TokenKind::Simple(Simple::Punc(Punc::Comma))))
            .collect(),
    );

    let body = expr_parser(db);

    just(TokenKind::Simple(Simple::Kw(Kw::Fn)))
        .ignore_then(name)
        .then(args)
        .then_ignore(just(TokenKind::Simple(Simple::Punc(Punc::Equals))))
        .then(body)
        .map(|(((name, name_span), args), body)| Function::new(db, name, name_span, args, body))
        .boxed()
}

fn expr_parser<'db: 'tok, 'src: 'tok, 'tok>(
    db: &'db dyn crate::Db,
) -> impl Parser<'tok, ParserInput<'tok>, Expression<'db>, ParserExtra<'db, 'src, 'tok>> {
    recursive(|expr| {
        let variable = variable_id_parser(db)
            .map_with(|id, e| Expression {
                span: e.span(),
                data: ExpressionData::Variable(id),
            })
            .boxed();

        let integer = select! {
            TokenKind::Simple(Simple::Integer(i)) => i,
        }
        .map_with(|value, e| Expression {
            span: e.span(),
            data: ExpressionData::Integer(value),
        })
        .boxed();

        let float = select! {
            TokenKind::Simple(Simple::Float(f)) => f,
        }
        .map_with(|value, e| Expression {
            span: e.span(),
            data: ExpressionData::Float(value),
        })
        .boxed();

        let parenthesised_expr = parenthesised(expr);

        let atom = choice((variable, integer, float, parenthesised_expr)).boxed();

        atom
    })
}

fn parenthesised<'db: 'tok, 'src: 'tok, 'tok, T>(
    inner: impl Parser<'tok, ParserInput<'tok>, T, ParserExtra<'db, 'src, 'tok>>,
) -> impl Parser<'tok, ParserInput<'tok>, T, ParserExtra<'db, 'src, 'tok>> {
    inner.nested_in(select_ref! {
        TokenKind::Parentheses(tokens) = e => tokens.as_slice().spanned(e.span()),
    })
}

fn function_id_parser<'db: 'tok, 'src: 'tok, 'tok>(
    db: &'db dyn crate::Db,
) -> impl Parser<'tok, ParserInput<'tok>, FunctionId<'db>, ParserExtra<'db, 'src, 'tok>> {
    select! {
        TokenKind::Simple(Simple::Ident(ident)) => FunctionId::new(db, ident),
    }
    .boxed()
}

fn variable_id_parser<'db: 'tok, 'src: 'tok, 'tok>(
    db: &'db dyn crate::Db,
) -> impl Parser<'tok, ParserInput<'tok>, VariableId<'db>, ParserExtra<'db, 'src, 'tok>> {
    select! {
        TokenKind::Simple(Simple::Ident(ident)) => VariableId::new(db, ident),
    }
    .boxed()
}
