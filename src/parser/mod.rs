use crate::{
    lexer::tokens::{Delim, Interned, Kw, Punc, Token, TokenTree},
    span::{Span, Spanned},
};
use ast::{
    Ast, AstType, BinaryOp, Enum, EnumVariant, Expression, Function, FunctionParam, Item, MatchArm,
    Module, Path, Pattern, PatternType, Struct, StructField, StructPatternField, Type, UnaryOp,
};
use chumsky::{extra, input::SpannedInput, prelude::*};

pub mod ast;

type ParserInput<'tok> = SpannedInput<TokenTree, Span, &'tok [(TokenTree, Span)]>;

type ParserExtra<'src, 'tok> = extra::Err<Rich<'tok, TokenTree, Span, &'src str>>;

pub fn parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Ast, ParserExtra<'src, 'tok>> {
    recursive(|ast| {
        item_parser(ast)
            .with_span()
            .repeated()
            .collect()
            .map(Ast)
            .boxed()
    })
    .boxed()
}

fn item_parser<'src: 'tok, 'tok>(
    ast: impl Parser<'tok, ParserInput<'tok>, Ast, ParserExtra<'src, 'tok>> + 'tok,
) -> impl Parser<'tok, ParserInput<'tok>, Item, ParserExtra<'src, 'tok>> {
    let function = function_parser().with_span().map(Item::Function).boxed();

    let import = just(TokenTree::Token(Token::Kw(Kw::Import)))
        .ignore_then(path_parser().with_span())
        .then_ignore(just(TokenTree::Token(Token::Punc(Punc::Semicolon))))
        .map(Item::Import)
        .boxed();

    let struct_ = struct_parser().with_span().map(Item::Struct).boxed();

    let enum_ = enum_parser().with_span().map(Item::Enum).boxed();

    let module = {
        let file_module = just(TokenTree::Token(Token::Kw(Kw::Module)))
            .ignore_then(ident_parser().with_span())
            .then_ignore(just(TokenTree::Token(Token::Punc(Punc::Semicolon))))
            .map(|name| Module { name, ast: None })
            .with_span()
            .map(Item::Module)
            .boxed();

        let submodule = just(TokenTree::Token(Token::Kw(Kw::Module)))
            .ignore_then(ident_parser().with_span())
            .then(ast.delim(Delim::Brace))
            .map(|(name, ast)| Module {
                name,
                ast: Some(ast),
            })
            .with_span()
            .map(Item::Module)
            .boxed();

        choice((file_module, submodule)).boxed()
    }
    .boxed();

    choice((function, import, struct_, enum_, module)).boxed()
}

fn function_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Function, ParserExtra<'src, 'tok>> {
    let name = ident_parser().with_span().boxed();

    let generics = comma_separated(ident_parser())
        .delim(Delim::Bracket)
        .with_span()
        .or_not()
        .boxed();

    let params = comma_separated(function_param_parser())
        .delim(Delim::Paren)
        .with_span()
        .boxed();

    let return_ty = just(TokenTree::Token(Token::Punc(Punc::Arrow)))
        .ignore_then(type_parser().with_span())
        .or_not()
        .boxed();

    let body = expression_parser().with_span().boxed();

    just(TokenTree::Token(Token::Kw(Kw::Func)))
        .ignore_then(name)
        .then(generics)
        .then(params)
        .then(return_ty)
        .then_ignore(just(TokenTree::Token(Token::Punc(Punc::Equals))))
        .then(body)
        .map(|((((name, generics), params), return_ty), body)| Function {
            name,
            generics,
            params,
            return_ty,
            body,
        })
        .boxed()
}

fn function_param_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, FunctionParam, ParserExtra<'src, 'tok>> {
    ident_parser()
        .with_span()
        .then(type_parser().with_span())
        .map(|(name, ty)| FunctionParam { name, ty })
        .boxed()
}

fn struct_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Struct, ParserExtra<'src, 'tok>> {
    let name = ident_parser().with_span().boxed();

    let generics = comma_separated(ident_parser())
        .delim(Delim::Bracket)
        .with_span()
        .or_not()
        .boxed();

    let fields = comma_separated(struct_field_parser())
        .delim(Delim::Brace)
        .with_span()
        .boxed();

    just(TokenTree::Token(Token::Kw(Kw::Struct)))
        .ignore_then(name)
        .then(generics)
        .then(fields)
        .map(|((name, generics), fields)| Struct {
            name,
            generics,
            fields,
        })
        .boxed()
}

fn struct_field_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, StructField, ParserExtra<'src, 'tok>> {
    ident_parser()
        .with_span()
        .then(type_parser().with_span())
        .map(|(name, ty)| StructField { name, ty })
        .boxed()
}

fn enum_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Enum, ParserExtra<'src, 'tok>> {
    let name = ident_parser().with_span().boxed();

    let generics = comma_separated(ident_parser())
        .delim(Delim::Bracket)
        .with_span()
        .or_not()
        .boxed();

    let variants = enum_variant_parser()
        .with_span()
        .separated_by(just(TokenTree::Token(Token::Punc(Punc::Pipe))))
        .allow_leading()
        .collect()
        .with_span()
        .boxed();

    just(TokenTree::Token(Token::Kw(Kw::Enum)))
        .ignore_then(name)
        .then(generics)
        .then_ignore(just(TokenTree::Token(Token::Punc(Punc::Equals))))
        .then(variants)
        .map(|((name, generics), variants)| Enum {
            name,
            generics,
            variants,
        })
        .boxed()
}

fn enum_variant_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, EnumVariant, ParserExtra<'src, 'tok>> {
    let unit = ident_parser().with_span().map(EnumVariant::Unit).boxed();

    let tuple = ident_parser()
        .with_span()
        .then(
            comma_separated(type_parser())
                .delim(Delim::Paren)
                .with_span(),
        )
        .map(|(name, fields)| EnumVariant::Tuple { name, fields })
        .boxed();

    let struct_ = ident_parser()
        .with_span()
        .then(
            comma_separated(struct_field_parser())
                .delim(Delim::Brace)
                .with_span(),
        )
        .map(|(name, fields)| EnumVariant::Struct { name, fields })
        .boxed();

    choice((tuple, struct_, unit)).boxed()
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
                let span = op.span().union(expr.span());

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
                let span = lhs.span().union(rhs.span());

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
    }};
}

fn expression_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Expression, ParserExtra<'src, 'tok>> {
    recursive(|expression| {
        let int = int_parser().with_span().map(Expression::Int).boxed();

        let float = float_parser().with_span().map(Expression::Float).boxed();

        let bool = bool_parser().with_span().map(Expression::Bool).boxed();

        let string = string_parser().with_span().map(Expression::String).boxed();

        let name = ident_parser().with_span().map(Expression::Name).boxed();

        let tuple = comma_separated(expression.clone())
            .delim(Delim::Paren)
            .with_span()
            .map(Expression::Tuple)
            .boxed();

        let list = just(TokenTree::Token(Token::Punc(Punc::Hash)))
            .ignore_then(comma_separated(expression.clone()))
            .delim(Delim::Bracket)
            .with_span()
            .map(Expression::List)
            .boxed();

        let pattern = pattern_parser(expression.clone()).with_span().boxed();

        let match_ = {
            let match_arm = just(TokenTree::Token(Token::Punc(Punc::Pipe)))
                .ignore_then(pattern.clone())
                .then_ignore(just(TokenTree::Token(Token::Punc(Punc::DoubleArrow))))
                .then(expression.clone().with_span())
                .map(|(pattern, body)| MatchArm { pattern, body })
                .boxed();

            just(TokenTree::Token(Token::Kw(Kw::Match)))
                .ignore_then(expression.clone().map(Box::new).with_span())
                .then(match_arm.with_span().repeated().collect().with_span())
                .map(|(expr, arms)| Expression::Match { expr, arms })
                .boxed()
        }
        .boxed();

        let binding = {
            let name = ident_parser().with_span().boxed();

            let ty = type_parser().with_span().or_not().boxed();

            let value = just(TokenTree::Token(Token::Punc(Punc::Equals)))
                .ignore_then(expression.clone().map(Box::new).with_span())
                .boxed();

            let body = expression.clone().map(Box::new).with_span().boxed();

            just(TokenTree::Token(Token::Kw(Kw::Var)))
                .ignore_then(name)
                .then(ty)
                .then(value)
                .then_ignore(just(TokenTree::Token(Token::Kw(Kw::In))))
                .then(body)
                .map(|(((name, ty), value), body)| Expression::Binding {
                    name,
                    ty,
                    value,
                    body,
                })
                .boxed()
        }
        .boxed();

        let parenthesized = expression
            .clone()
            .with_span()
            .delim(Delim::Paren)
            .map(|expr| expr.0)
            .boxed();

        let atom = choice((
            parenthesized,
            binding,
            tuple,
            list,
            match_,
            int,
            float,
            bool,
            string,
            name,
        ))
        .boxed();

        let basic = {
            enum BasicOperator {
                Call(Spanned<Vec<Spanned<Expression>>>),
                Dot(Spanned<Interned>),
            }

            let call_args = comma_separated(expression.clone())
                .delim(Delim::Paren)
                .with_span()
                .map(BasicOperator::Call)
                .boxed();

            let dot = just(TokenTree::Token(Token::Punc(Punc::Period)))
                .ignore_then(ident_parser().with_span())
                .map(BasicOperator::Dot)
                .boxed();

            atom.with_span()
                .foldl(choice((call_args, dot)).repeated(), |expr, op| match op {
                    BasicOperator::Call(args) => {
                        let span = expr.span().union(args.span());

                        Spanned::new(
                            Expression::Call {
                                callee: expr.boxed(),
                                args,
                            },
                            span,
                        )
                    }
                    BasicOperator::Dot(field) => {
                        let span = expr.span().union(field.span());

                        Spanned::new(
                            Expression::Dot {
                                expr: expr.boxed(),
                                field,
                            },
                            span,
                        )
                    }
                })
                .map(|expr| expr.0)
                .boxed()
        };

        let unary = unary_op!(
            basic,
            Punc::Plus => UnaryOp::Pos,
            Punc::Minus => UnaryOp::Neg,
            Punc::Bang => UnaryOp::Not,
        )
        .boxed();

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
}

fn pattern_parser<'src: 'tok, 'tok>(
    expression: impl Parser<'tok, ParserInput<'tok>, Expression, ParserExtra<'src, 'tok>> + 'tok,
) -> impl Parser<'tok, ParserInput<'tok>, Pattern, ParserExtra<'src, 'tok>> {
    recursive(|pattern| {
        pattern_type_parser(pattern)
            .with_span()
            .then(
                just(TokenTree::Token(Token::Kw(Kw::Where)))
                    .ignore_then(expression.with_span())
                    .or_not(),
            )
            .map(|(pattern_type, condition)| Pattern {
                pattern_type,
                condition,
            })
            .boxed()
    })
    .boxed()
}

fn pattern_type_parser<'src: 'tok, 'tok>(
    pattern: impl Parser<'tok, ParserInput<'tok>, Pattern, ParserExtra<'src, 'tok>> + Clone + 'tok,
) -> impl Parser<'tok, ParserInput<'tok>, PatternType, ParserExtra<'src, 'tok>> {
    let tuple_pattern = comma_separated(pattern.clone())
        .delim(Delim::Paren)
        .with_span()
        .map(PatternType::Tuple)
        .boxed();

    let tuple_type_pattern = path_parser()
        .with_span()
        .then(
            comma_separated(pattern.clone())
                .delim(Delim::Paren)
                .with_span(),
        )
        .map(|(name, fields)| PatternType::TupleType { name, fields })
        .boxed();

    let struct_type_pattern = {
        let struct_pattern_field = ident_parser()
            .with_span()
            .then(pattern.with_span().or_not())
            .map(|(name, pattern)| StructPatternField { name, pattern })
            .boxed();

        path_parser()
            .with_span()
            .then(
                comma_separated(struct_pattern_field)
                    .delim(Delim::Brace)
                    .with_span(),
            )
            .map(|(name, fields)| PatternType::StructType { name, fields })
            .boxed()
    }
    .boxed();

    choice((
        int_parser().with_span().map(PatternType::Int),
        float_parser().with_span().map(PatternType::Float),
        bool_parser().with_span().map(PatternType::Bool),
        string_parser().with_span().map(PatternType::String),
        tuple_pattern,
        tuple_type_pattern,
        struct_type_pattern,
        path_parser().with_span().map(PatternType::Name),
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
            .boxed()
        }
    };
}

interned_parser!(int_parser, Int);
interned_parser!(float_parser, Float);
interned_parser!(bool_parser, Boolean);
interned_parser!(string_parser, String);
interned_parser!(ident_parser, Identifier);

fn path_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Path, ParserExtra<'src, 'tok>> {
    ident_parser()
        .with_span()
        .separated_by(just(TokenTree::Token(Token::Punc(Punc::Period))))
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|segments| {
            let (base, segments) = segments.split_first().unwrap();

            Path {
                base: *base,
                segments: segments.to_vec(),
            }
        })
        .boxed()
}

fn type_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, AstType, ParserExtra<'src, 'tok>> {
    recursive(|type_| {
        let prim = {
            let generics = comma_separated(type_.clone())
                .delim(Delim::Bracket)
                .with_span()
                .or_not()
                .boxed();

            path_parser()
                .with_span()
                .then(generics)
                .map(|(name, generics)| Type::Primitive { name, generics })
                .boxed()
        };

        let tuple = comma_separated(type_.clone())
            .delim(Delim::Paren)
            .with_span()
            .map(Type::Tuple)
            .boxed();

        let never = just(TokenTree::Token(Token::Punc(Punc::Bang)))
            .to(Type::Never)
            .boxed();

        let function = {
            let params = comma_separated(type_.clone())
                .delim(Delim::Paren)
                .with_span()
                .boxed();

            let return_ty = type_.map(Box::new).with_span().boxed();

            just(TokenTree::Token(Token::Kw(Kw::Func)))
                .ignore_then(params)
                .then_ignore(just(TokenTree::Token(Token::Punc(Punc::Arrow))))
                .then(return_ty)
                .map(|(params, return_ty)| Type::Function { params, return_ty })
                .boxed()
        };

        choice((prim, tuple, never, function)).boxed()
    })
    .boxed()
}

fn comma_separated<'src: 'tok, 'tok, Inner: 'tok>(
    inner: impl Parser<'tok, ParserInput<'tok>, Inner, ParserExtra<'src, 'tok>> + 'tok,
) -> impl Parser<'tok, ParserInput<'tok>, Vec<Spanned<Inner>>, ParserExtra<'src, 'tok>> {
    inner
        .with_span()
        .separated_by(just(TokenTree::Token(Token::Punc(Punc::Comma))))
        .allow_trailing()
        .collect()
        .boxed()
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
