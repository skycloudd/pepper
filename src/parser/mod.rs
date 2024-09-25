use crate::{
    lexer::tokens::{FractionalPart, Kw, Punc, SimpleToken, Token},
    span::{Span, Spanned},
    RODEO,
};
use ast::{
    Ast, BinaryOp, Expression, Function, FunctionParam, FunctionType, Identifier, Type, UnaryOp,
};
use chumsky::{extra, input::SpannedInput, prelude::*};
use malachite::{rational_sequences::RationalSequence, Natural, Rational};

pub mod ast;

type ParserInput<'src, 'tok> = SpannedInput<Token<'src>, Span, &'tok [(Token<'src>, Span)]>;

type ParserExtra<'src, 'tok> = extra::Err<Rich<'tok, Token<'src>, Span, &'src str>>;

#[must_use]
pub fn parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Ast, ParserExtra<'src, 'tok>> {
    function_parser()
        .with_span()
        .repeated()
        .collect()
        .map(|functions| Ast { functions })
        .boxed()
}

fn function_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Function, ParserExtra<'src, 'tok>> {
    let name = ident_parser().with_span();

    let params = function_param_parser()
        .with_span()
        .separated_by(just(Token::Simple(SimpleToken::Punc(Punc::Comma))))
        .allow_trailing()
        .collect()
        .parenthesized()
        .with_span();

    let body = expression_parser().with_span();

    just(Token::Simple(SimpleToken::Kw(Kw::Let)))
        .ignore_then(name)
        .then(params)
        .then(
            just(Token::Simple(SimpleToken::Punc(Punc::Arrow)))
                .ignore_then(type_parser().with_span())
                .or_not(),
        )
        .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::Equals))))
        .then(body)
        .map(|(((name, params), return_ty), body)| Function {
            name,
            params,
            return_ty,
            body,
        })
        .boxed()
}

fn function_param_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, FunctionParam, ParserExtra<'src, 'tok>> {
    ident_parser()
        .with_span()
        .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::Colon))))
        .then(type_parser().with_span())
        .map(|(name, ty)| FunctionParam { name, ty })
        .boxed()
}

fn expression_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Expression, ParserExtra<'src, 'tok>> {
    macro_rules! unary_op {
        ($base:expr, $(($punc:expr => $to:expr)),*) => {{
            let ops = choice((
                $(
                    just(Token::Simple(SimpleToken::Punc($punc))).to($to),
                )*
            ))
            .with_span()
            .boxed();

            ops
                .repeated()
                .foldr($base.with_span(), |op, expr| {
                    let span = op.1.union(expr.1);

                    Spanned::new(
                        Expression::UnaryOp {
                            op,
                            expr: expr.boxed(),
                        },
                        span
                    )
                })
                .map(|expr| expr.0)
                .boxed()
        }};
    }

    macro_rules! binary_op {
        ($base:expr, $(($punc:expr => $to:expr)),*) => {{
            let ops = choice((
                $(
                    just(Token::Simple(SimpleToken::Punc($punc))).to($to),
                )*
            ))
            .with_span()
            .boxed();

            $base
                .clone()
                .with_span()
                .foldl(ops.then($base.with_span()).repeated(), |lhs, (op, rhs)| {
                    let span = lhs.1.union(rhs.1);

                    Spanned::new(
                        Expression::BinaryOp {
                            op,
                            lhs: lhs.boxed(),
                            rhs: rhs.boxed(),
                        },
                        span
                    )
                })
                .map(|expr| expr.0)
                .boxed()
        }};
    }

    recursive(|expression| {
        let unit = just(Token::Simple(SimpleToken::Punc(Punc::Hash)))
            .to(Expression::Unit)
            .boxed();

        let number = select! {
            Token::Simple(SimpleToken::Number(int, frac)) => (int, frac),
        }
        .map(|(int, frac)| rational_from_parts_base(int, frac, 10))
        .map(Expression::Number)
        .boxed();

        let bool = select! {
            Token::Simple(SimpleToken::Boolean(bool)) => bool,
        }
        .map(Expression::Bool)
        .boxed();

        let variable = ident_parser().map(Expression::Variable).boxed();

        let call_args = expression
            .clone()
            .with_span()
            .separated_by(just(Token::Simple(SimpleToken::Punc(Punc::Comma))))
            .allow_trailing()
            .collect()
            .parenthesized()
            .with_span()
            .boxed();

        let call = ident_parser()
            .with_span()
            .then(call_args)
            .map(|(name, args)| Expression::Call { name, args })
            .boxed();

        let parenthesized = expression
            .clone()
            .with_span()
            .parenthesized()
            .map(|expr| expr.0)
            .boxed();

        let atom = choice((parenthesized, call, unit, number, bool, variable)).boxed();

        let unary = unary_op!(atom, (Punc::Minus => UnaryOp::Neg)).boxed();

        let factor =
            binary_op!(unary, (Punc::Star => BinaryOp::Mul), (Punc::Slash => BinaryOp::Div))
                .boxed();

        binary_op!(factor, (Punc::Plus => BinaryOp::Add), (Punc::Minus => BinaryOp::Sub)).boxed()
    })
    .boxed()
}

fn ident_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Identifier, ParserExtra<'src, 'tok>> {
    select! {
        Token::Simple(SimpleToken::Identifier(ident)) => Identifier( RODEO.get_or_intern(ident)),
    }
    .boxed()
}

fn type_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Type<Identifier>, ParserExtra<'src, 'tok>> {
    recursive(|type_| {
        let prim = ident_parser().map(Type::Primitive).boxed();

        let unit = just(Token::Simple(SimpleToken::Punc(Punc::Hash)))
            .to(Type::Unit)
            .boxed();

        let never = just(Token::Simple(SimpleToken::Punc(Punc::Bang)))
            .to(Type::Never)
            .boxed();

        let function = {
            let params = type_
                .clone()
                .separated_by(just(Token::Simple(SimpleToken::Punc(Punc::Comma))))
                .allow_trailing()
                .collect()
                .parenthesized()
                .boxed();

            let return_ty = just(Token::Simple(SimpleToken::Punc(Punc::Arrow)))
                .ignore_then(type_)
                .or_not()
                .map(|ty| ty.unwrap_or_else(|| Type::Unit))
                .map(Box::new)
                .boxed();

            params.then(return_ty)
        }
        .map(|(params, return_ty)| FunctionType { params, return_ty })
        .map(Type::Function)
        .boxed();

        choice((prim, unit, never, function)).boxed()
    })
}

trait SpannedExt<'src: 'tok, 'tok, O> {
    fn with_span(
        self,
    ) -> impl Parser<'tok, ParserInput<'src, 'tok>, Spanned<O>, ParserExtra<'src, 'tok>>;

    fn parenthesized(
        self,
    ) -> impl Parser<'tok, ParserInput<'src, 'tok>, O, ParserExtra<'src, 'tok>>;
}

impl<'src: 'tok, 'tok, P, O> SpannedExt<'src, 'tok, O> for P
where
    P: Parser<'tok, ParserInput<'src, 'tok>, O, ParserExtra<'src, 'tok>>,
{
    fn with_span(
        self,
    ) -> impl Parser<'tok, ParserInput<'src, 'tok>, Spanned<O>, ParserExtra<'src, 'tok>> {
        self.map_with(|t, e| Spanned::new(t, e.span()))
    }

    fn parenthesized(
        self,
    ) -> impl Parser<'tok, ParserInput<'src, 'tok>, O, ParserExtra<'src, 'tok>> {
        self.nested_in(select_ref! {
            Token::Parentheses(tokens) = e => tokens.as_slice().spanned(e.span()),
        })
    }
}

fn rational_from_parts_base(int: &str, frac: FractionalPart, radix: u32) -> Rational {
    let before_point = int
        .chars()
        .rev()
        .map(|c| Natural::from(c.to_digit(radix).unwrap()))
        .collect();

    let after_point = match frac {
        FractionalPart::None | FractionalPart::Period => RationalSequence::default(),
        FractionalPart::Full(full) => RationalSequence::from_vec(
            full.chars()
                .map(|c| Natural::from(c.to_digit(radix).unwrap()))
                .collect(),
        ),
    };

    Rational::from_digits(&Natural::from(radix), before_point, after_point)
}
