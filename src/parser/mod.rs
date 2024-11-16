use crate::{
    lexer::tokens::{Identifier, Kw, Punc, SimpleToken, Token},
    span::{Span, Spanned},
};
use ast::{
    Ast, BinaryOp, Expression, Function, FunctionParam, MatchArm, Pattern, PatternType, TopLevel,
    Type, UnaryOp,
};
use chumsky::{extra, input::SpannedInput, prelude::*};

pub mod ast;

type ParserInput<'tok> = SpannedInput<Token, Span, &'tok [(Token, Span)]>;

type ParserExtra<'src, 'tok> = extra::Err<Rich<'tok, Token, Span, &'src str>>;

#[must_use]
pub fn parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Ast, ParserExtra<'src, 'tok>> {
    toplevel_parser()
        .with_span()
        .repeated()
        .collect()
        .map(Ast)
        .boxed()
}

fn toplevel_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, TopLevel, ParserExtra<'src, 'tok>> {
    let function = function_parser()
        .with_span()
        .map(TopLevel::Function)
        .boxed();

    choice((function,)).boxed()
}

fn function_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Function, ParserExtra<'src, 'tok>> {
    let name = ident_parser().with_span();

    let params = function_param_parser()
        .with_span()
        .separated_by(just(Token::Simple(SimpleToken::Punc(Punc::Comma))))
        .allow_trailing()
        .collect()
        .parenthesized()
        .with_span()
        .boxed()
        .labelled("function parameters");

    let return_ty = just(Token::Simple(SimpleToken::Punc(Punc::Arrow)))
        .ignore_then(type_parser().with_span())
        .boxed()
        .labelled("return type");

    let body = expression_parser()
        .with_span()
        .boxed()
        .labelled("function body");

    just(Token::Simple(SimpleToken::Kw(Kw::Func)))
        .ignore_then(name)
        .then(params)
        .then(return_ty)
        .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::Equals))))
        .then(body)
        .map(|(((name, params), return_ty), body)| Function {
            name,
            params,
            return_ty,
            body,
        })
        .boxed()
        .labelled("function")
}

fn function_param_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, FunctionParam, ParserExtra<'src, 'tok>> {
    ident_parser()
        .with_span()
        .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::Colon))))
        .then(type_parser().with_span())
        .map(|(name, ty)| FunctionParam { name, ty })
        .boxed()
        .labelled("function parameter")
}

macro_rules! unary_op {
    ($base:expr, $($punc:expr => $to:expr),* $(,)?) => {{
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
    ($base:expr, $($punc:expr => $to:expr),* $(,)?) => {{
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

fn expression_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Expression, ParserExtra<'src, 'tok>> {
    recursive(|expression| {
        let int = select! {
            Token::Simple(SimpleToken::Int(num)) => num,
        }
        .map(Expression::Int)
        .boxed()
        .labelled("integer");

        let float = select! {
            Token::Simple(SimpleToken::Float(num)) => num,
        }
        .map(Expression::Float)
        .boxed()
        .labelled("float");

        let bool = select! {
            Token::Simple(SimpleToken::Boolean(bool)) => bool,
        }
        .map(Expression::Bool)
        .boxed()
        .labelled("boolean");

        let variable = ident_parser()
            .map(Expression::Variable)
            .boxed()
            .labelled("variable");

        let pattern = choice((
            just(Token::Simple(SimpleToken::Wildcard)).to(PatternType::Wildcard),
            ident_parser().map(PatternType::Variable),
            select! { Token::Simple(SimpleToken::Int(num)) => num }.map(PatternType::Int),
            select! { Token::Simple(SimpleToken::Float(num)) => num }.map(PatternType::Float),
            select! { Token::Simple(SimpleToken::Boolean(bool)) => bool }.map(PatternType::Bool),
        ))
        .with_span()
        .then(
            just(Token::Simple(SimpleToken::Kw(Kw::Where)))
                .ignore_then(expression.clone().with_span())
                .or_not(),
        )
        .map(|(pattern_type, condition)| Pattern {
            pattern_type,
            condition,
        })
        .with_span()
        .boxed()
        .labelled("pattern");

        let match_arm = just(Token::Simple(SimpleToken::Punc(Punc::Pipe)))
            .ignore_then(pattern)
            .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::DoubleArrow))))
            .then(expression.clone().with_span())
            .map(|(pattern, body)| MatchArm { pattern, body })
            .with_span()
            .boxed()
            .labelled("match arm");

        let match_ = just(Token::Simple(SimpleToken::Kw(Kw::Match)))
            .ignore_then(expression.clone().with_span())
            .then(match_arm.repeated().collect().with_span())
            .map(|(expr, arms)| Expression::Match {
                expr: expr.boxed(),
                arms,
            })
            .boxed()
            .labelled("match expression");

        let parenthesized = expression
            .clone()
            .with_span()
            .parenthesized()
            .map(|expr| expr.0)
            .boxed()
            .labelled("parenthesized expression");

        let atom = choice((parenthesized, match_, int, float, bool, variable)).boxed();

        let call_args = expression
            .clone()
            .with_span()
            .separated_by(just(Token::Simple(SimpleToken::Punc(Punc::Comma))))
            .allow_trailing()
            .collect()
            .parenthesized()
            .with_span()
            .boxed()
            .labelled("call arguments");

        let call = atom
            .with_span()
            .foldl(call_args.repeated(), |callee, args| {
                let span = callee.1.union(args.1);

                Spanned::new(
                    Expression::Call {
                        callee: callee.boxed(),
                        args,
                    },
                    span,
                )
            })
            .map(|expr| expr.0)
            .boxed()
            .labelled("function call");

        let unary = unary_op!(
            call,
            Punc::Minus => UnaryOp::Neg,
            Punc::Bang => UnaryOp::Not,
        )
        .boxed()
        .labelled("unary expression");

        let equality = binary_op!(
            unary,
            Punc::DoubleEquals => BinaryOp::Equals,
            Punc::NotEquals => BinaryOp::NotEquals,
        )
        .boxed();

        let relational = binary_op!(
            equality,
            Punc::LessEquals => BinaryOp::LessEquals,
            Punc::GreaterEquals => BinaryOp::GreaterEquals,
            Punc::Less => BinaryOp::Less,
            Punc::Greater => BinaryOp::Greater,
        )
        .boxed();

        let factor = binary_op!(
            relational,
            Punc::Star => BinaryOp::Mul,
            Punc::Slash => BinaryOp::Div,
        )
        .boxed();

        binary_op!(
            factor,
            Punc::Plus => BinaryOp::Add,
            Punc::Minus => BinaryOp::Sub,
        )
        .boxed()
    })
    .boxed()
    .labelled("expression")
}

fn ident_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Identifier, ParserExtra<'src, 'tok>> {
    select! {
        Token::Simple(SimpleToken::Identifier(ident)) => ident
    }
    .boxed()
    .labelled("identifier")
}

fn type_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Type<Identifier>, ParserExtra<'src, 'tok>> {
    recursive(|type_| {
        let prim = ident_parser()
            .map(Type::Primitive)
            .boxed()
            .labelled("primitive type");

        let never = just(Token::Simple(SimpleToken::Punc(Punc::Bang)))
            .to(Type::Never)
            .boxed()
            .labelled("never type");

        let function = {
            let params = type_
                .clone()
                .with_span()
                .separated_by(just(Token::Simple(SimpleToken::Punc(Punc::Comma))))
                .allow_trailing()
                .collect()
                .parenthesized()
                .with_span()
                .boxed();

            let return_ty = just(Token::Simple(SimpleToken::Punc(Punc::Arrow)))
                .ignore_then(type_.with_span())
                .boxed();

            just(Token::Simple(SimpleToken::Kw(Kw::Func)))
                .ignore_then(params)
                .then(return_ty)
                .boxed()
        }
        .map(|(params, return_ty)| Type::Function {
            params,
            return_ty: return_ty.boxed(),
        })
        .boxed()
        .labelled("function type");

        choice((prim, never, function)).boxed().labelled("type")
    })
}

trait SpannedExt<'src: 'tok, 'tok, O> {
    fn with_span(self)
        -> impl Parser<'tok, ParserInput<'tok>, Spanned<O>, ParserExtra<'src, 'tok>>;

    fn parenthesized(self) -> impl Parser<'tok, ParserInput<'tok>, O, ParserExtra<'src, 'tok>>;
}

impl<'src: 'tok, 'tok, P, O> SpannedExt<'src, 'tok, O> for P
where
    P: Parser<'tok, ParserInput<'tok>, O, ParserExtra<'src, 'tok>>,
{
    fn with_span(
        self,
    ) -> impl Parser<'tok, ParserInput<'tok>, Spanned<O>, ParserExtra<'src, 'tok>> {
        self.map_with(|t, e| Spanned::new(t, e.span()))
    }

    fn parenthesized(self) -> impl Parser<'tok, ParserInput<'tok>, O, ParserExtra<'src, 'tok>> {
        self.nested_in(select_ref! {
            Token::Parentheses(tokens) = e => tokens.as_slice().spanned(e.span()),
        })
    }
}
