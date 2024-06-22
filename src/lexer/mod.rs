use crate::{
    diagnostics::{error::convert, Diagnostics},
    SourceProgram,
};
use chumsky::{input::WithContext, prelude::*};
use ordered_float::OrderedFloat;
use tokens::{Kw, Punc, Simple, Span, TokenKind, Tokens};

pub mod tokens;

#[salsa::tracked]
pub fn lex(db: &dyn crate::Db, source: SourceProgram) -> Option<Tokens<'_>> {
    let (tokens, errors) = lexer()
        .parse(source.text(db).with_context(source.file_id(db)))
        .into_output_errors();

    for err in errors
        .into_iter()
        .flat_map(|err| convert(&err.map_token(|token| token.to_string())))
    {
        Diagnostics::push(db, err);
    }

    tokens.map(|tokens| Tokens::new(db, tokens, source.file_id(db)))
}

fn lexer<'src>() -> impl Parser<
    'src,
    WithContext<Span, &'src str>,
    Vec<(TokenKind, Span)>,
    extra::Err<Rich<'src, char, Span, &'src str>>,
> {
    recursive(|tokens| {
        let ident = text::ascii::ident()
            .map(|name: &str| Simple::Ident(name.to_owned()))
            .boxed();

        let bool = choice((
            text::keyword("true").to(Simple::Boolean(true)),
            text::keyword("false").to(Simple::Boolean(false)),
        ))
        .boxed();

        let sign = choice((just('+'), just('-'))).or_not().boxed();

        let integer = sign
            .clone()
            .then(text::int(10))
            .to_slice()
            .validate(|n: &str, err, emitter| match n.parse() {
                Ok(n) => n,
                Err(parse_error) => {
                    emitter.emit(Rich::custom(err.span(), parse_error));
                    0
                }
            })
            .map(Simple::Integer)
            .boxed();

        let float = sign
            .then(text::int(10))
            .then_ignore(just('.'))
            .then(text::digits(10))
            .to_slice()
            .validate(|n: &str, err, emitter| match n.parse() {
                Ok(n) => n,
                Err(parse_error) => {
                    emitter.emit(Rich::custom(err.span(), parse_error));
                    0.0
                }
            })
            .map(|value| Simple::Float(OrderedFloat(value)))
            .boxed();

        let keyword = choice((text::keyword("fn").to(Simple::Kw(Kw::Fn)),)).boxed();

        let punctuation = choice((
            just("->").to(Simple::Punc(Punc::Arrow)),
            just("=").to(Simple::Punc(Punc::Equals)),
            just(":").to(Simple::Punc(Punc::Colon)),
            just(",").to(Simple::Punc(Punc::Comma)),
            just('+').to(Simple::Punc(Punc::Plus)),
            just('-').to(Simple::Punc(Punc::Minus)),
            just('*').to(Simple::Punc(Punc::Star)),
            just('/').to(Simple::Punc(Punc::Slash)),
        ))
        .boxed();

        let comment = just("#")
            .then(any().and_is(just('\n').not()).repeated())
            .padded()
            .boxed();

        let simple = choice((keyword, bool, ident, float, integer, punctuation))
            .map(TokenKind::Simple)
            .boxed();

        let parenthesised = tokens
            .clone()
            .delimited_by(just('('), just(')'))
            .recover_with(via_parser(nested_delimiters(
                '(',
                ')',
                [('{', '}')],
                |span| vec![(TokenKind::Error, span)],
            )))
            .map(TokenKind::Parentheses)
            .boxed();

        let curly_braces = tokens
            .delimited_by(just('{'), just('}'))
            .recover_with(via_parser(nested_delimiters(
                '{',
                '}',
                [('(', ')')],
                |span| vec![(TokenKind::Error, span)],
            )))
            .map(TokenKind::CurlyBraces)
            .boxed();

        let token = choice((simple, parenthesised, curly_braces))
            .map_with(|kind, e| (kind, e.span()))
            .padded_by(comment.clone().repeated())
            .padded()
            .boxed();

        token.repeated().collect().padded().boxed()
    })
    .then_ignore(end())
    .boxed()
}
