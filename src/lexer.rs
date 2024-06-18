use crate::{
    ir::{Diagnostics, FileId, SourceProgram},
    tokens::{Kw, Punc, Simple, Span, Token, TokenKind, Tokens},
};
use chumsky::{input::WithContext, prelude::*};
use ordered_float::OrderedFloat;

#[salsa::tracked]
pub fn lex<'db>(
    db: &'db dyn crate::Db,
    source: SourceProgram,
    filename: FileId<'db>,
) -> Option<Tokens<'db>> {
    let (tokens, errors) = lexer()
        .parse(source.text(db).with_context(filename))
        .into_output_errors();

    for err in errors {
        Diagnostics::push(db, err.clone().map_token(|t| t.to_string()).into());
    }

    tokens
}

fn lexer<'src, 'db: 'src>() -> impl Parser<
    'src,
    WithContext<Span<'db>, &'src str>,
    Tokens<'src>,
    extra::Err<Rich<'src, char, Span<'db>, &'src str>>,
> {
    recursive(|tokens| {
        let ident = text::ascii::ident()
            .map(|name: &str| Simple::Ident(name.to_string()))
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
            just("=").to(Simple::Punc(Punc::Equals)),
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
                |span| vec![Token(TokenKind::Error, span)],
            )))
            .map(Tokens)
            .map(TokenKind::Parentheses)
            .boxed();

        let curly_braces = tokens
            .delimited_by(just('{'), just('}'))
            .recover_with(via_parser(nested_delimiters(
                '{',
                '}',
                [('(', ')')],
                |span| vec![Token(TokenKind::Error, span)],
            )))
            .map(Tokens)
            .map(TokenKind::CurlyBraces)
            .boxed();

        let token = choice((simple, parenthesised, curly_braces))
            .map_with(|kind, e| Token(kind, e.span()))
            .padded_by(comment.clone().repeated())
            .padded()
            .boxed();

        token.repeated().collect().padded().boxed()
    })
    .then_ignore(end())
    .map(Tokens)
    .boxed()
}
