use crate::{
    lexer::tokens::{Delim, Interned, Kw, Punc, Token, TokenTree},
    span::{Span, Spanned},
};
use ast::{
    Ast, BinaryOp, Expression, Function, FunctionParam, MatchArm, Pattern, PatternType, Statement,
    TopLevel, Type, UnaryOp,
};
use chumsky::{extra, input::SpannedInput, prelude::*};

pub mod ast;

type ParserInput<'tok> = SpannedInput<TokenTree, Span, &'tok [(TokenTree, Span)]>;

type ParserExtra<'src, 'tok> = extra::Err<Rich<'tok, TokenTree, Span, &'src str>>;

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
        .separated_by(just(TokenTree::Token(Token::Punc(Punc::Comma))))
        .allow_trailing()
        .collect()
        .delim(Delim::Paren)
        .with_span()
        .boxed()
        .labelled("function parameters");

    let return_ty = just(TokenTree::Token(Token::Punc(Punc::Arrow)))
        .ignore_then(type_parser().with_span())
        .boxed()
        .labelled("return type");

    let body = block_parser(statement_parser())
        .with_span()
        .labelled("function body");

    just(TokenTree::Token(Token::Kw(Kw::Func)))
        .ignore_then(name)
        .then(params)
        .then(return_ty)
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
        .then_ignore(just(TokenTree::Token(Token::Punc(Punc::Colon))))
        .then(type_parser().with_span())
        .map(|(name, ty)| FunctionParam { name, ty })
        .boxed()
        .labelled("function parameter")
}

fn statement_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Statement, ParserExtra<'src, 'tok>> {
    recursive(|statement| {
        let expr = expression_parser()
            .with_span()
            .then_ignore(just(TokenTree::Token(Token::Punc(Punc::Semicolon))))
            .map(Statement::Expression)
            .boxed();

        let block = block_parser(statement);

        let var_decl = just(TokenTree::Token(Token::Kw(Kw::Var)))
            .ignore_then(ident_parser().with_span())
            .then(
                just(TokenTree::Token(Token::Punc(Punc::Colon)))
                    .ignore_then(type_parser().with_span())
                    .or_not(),
            )
            .then_ignore(just(TokenTree::Token(Token::Punc(Punc::Equals))))
            .then(expression_parser().with_span())
            .then_ignore(just(TokenTree::Token(Token::Punc(Punc::Semicolon))))
            .map(|((name, ty), value)| Statement::VarDecl { name, ty, value })
            .boxed();

        let for_loop = just(TokenTree::Token(Token::Kw(Kw::For)))
            .ignore_then(pattern_type_parser().with_span())
            .then_ignore(just(TokenTree::Token(Token::Kw(Kw::In))))
            .then(expression_parser().with_span())
            .then(block.clone().with_span())
            .map(|((var, iter), body)| Statement::For { var, iter, body });

        choice((expr, block.map(Statement::Block), var_decl, for_loop)).boxed()
    })
}

fn block_parser<'src: 'tok, 'tok>(
    statement: impl Parser<'tok, ParserInput<'tok>, Statement, ParserExtra<'src, 'tok>> + 'tok,
) -> impl Parser<'tok, ParserInput<'tok>, Vec<Spanned<Statement>>, ParserExtra<'src, 'tok>> + Clone
{
    statement
        .with_span()
        .repeated()
        .collect()
        .delim(Delim::Brace)
        .boxed()
}

macro_rules! unary_op {
    ($base:expr, $($punc:expr => $to:expr),* $(,)?) => {{
        let ops = choice((
            $(
                just(TokenTree::Token(Token::Punc($punc))).to($to),
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
                just(TokenTree::Token(Token::Punc($punc))).to($to),
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
        let int = int_parser()
            .map(Expression::Int)
            .boxed()
            .labelled("integer");

        let float = float_parser()
            .map(Expression::Float)
            .boxed()
            .labelled("float");

        let bool = bool_parser()
            .map(Expression::Bool)
            .boxed()
            .labelled("boolean");

        let variable = ident_parser()
            .with_span()
            .map(Expression::Variable)
            .boxed()
            .labelled("variable");

        let list = expression
            .clone()
            .with_span()
            .separated_by(just(TokenTree::Token(Token::Punc(Punc::Comma))))
            .allow_trailing()
            .collect()
            .delim(Delim::Bracket)
            .map(Expression::List)
            .boxed();

        let pattern = pattern_type_parser()
            .with_span()
            .then(
                just(TokenTree::Token(Token::Kw(Kw::Where)))
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

        let match_arm = just(TokenTree::Token(Token::Punc(Punc::Pipe)))
            .ignore_then(pattern)
            .then_ignore(just(TokenTree::Token(Token::Punc(Punc::DoubleArrow))))
            .then(expression.clone().with_span())
            .map(|(pattern, body)| MatchArm { pattern, body })
            .with_span()
            .boxed()
            .labelled("match arm");

        let match_ = just(TokenTree::Token(Token::Kw(Kw::Match)))
            .ignore_then(expression.clone().map(Box::new).with_span())
            .then(match_arm.repeated().collect().with_span())
            .map(|(expr, arms)| Expression::Match { expr, arms })
            .boxed()
            .labelled("match expression");

        let parenthesized = expression
            .clone()
            .with_span()
            .delim(Delim::Paren)
            .map(|expr| expr.0)
            .boxed()
            .labelled("parenthesized expression");

        let atom = choice((parenthesized, list, match_, int, float, bool, variable)).boxed();

        let call_args = expression
            .clone()
            .with_span()
            .separated_by(just(TokenTree::Token(Token::Punc(Punc::Comma))))
            .allow_trailing()
            .collect()
            .delim(Delim::Paren)
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

fn pattern_type_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, PatternType, ParserExtra<'src, 'tok>> {
    choice((
        just(TokenTree::Token(Token::Wildcard)).to(PatternType::Wildcard),
        ident_parser().map(PatternType::Variable),
        int_parser().map(PatternType::Int),
        float_parser().map(PatternType::Float),
        bool_parser().map(PatternType::Bool),
    ))
    .boxed()
}

macro_rules! interned_parser {
    ($name:ident, $token:ident) => {
        fn $name<'src: 'tok, 'tok>(
        ) -> impl Parser<'tok, ParserInput<'tok>, Interned, ParserExtra<'src, 'tok>> {
            select! {
                TokenTree::Token(Token::$token(ident)) => ident
            }
        }
    };
}

interned_parser!(int_parser, Int);
interned_parser!(float_parser, Float);
interned_parser!(bool_parser, Boolean);
interned_parser!(ident_parser, Identifier);

fn type_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Type<Interned>, ParserExtra<'src, 'tok>> {
    recursive(|type_| {
        let prim = ident_parser()
            .with_span()
            .map(Type::Primitive)
            .boxed()
            .labelled("primitive type");

        let never = just(TokenTree::Token(Token::Punc(Punc::Bang)))
            .to(Type::Never)
            .boxed()
            .labelled("never type");

        let function = {
            let params = type_
                .clone()
                .with_span()
                .separated_by(just(TokenTree::Token(Token::Punc(Punc::Comma))))
                .allow_trailing()
                .collect()
                .delim(Delim::Paren)
                .with_span()
                .boxed();

            let return_ty = just(TokenTree::Token(Token::Punc(Punc::Arrow)))
                .ignore_then(type_.with_span())
                .boxed();

            just(TokenTree::Token(Token::Kw(Kw::Func)))
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

    fn delim(
        self,
        delim: Delim,
    ) -> impl Parser<'tok, ParserInput<'tok>, O, ParserExtra<'src, 'tok>>;
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

    fn delim(
        self,
        delim: Delim,
    ) -> impl Parser<'tok, ParserInput<'tok>, O, ParserExtra<'src, 'tok>> {
        self.nested_in(select_ref! {
            TokenTree::Tree(d, tokens) = e if *d == delim => tokens.as_slice().spanned(e.span()),
        })
    }
}
