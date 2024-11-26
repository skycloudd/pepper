use chumsky::{input::Input as _, span::Span as _, Parser as _};
use diagnostics::error::{convert, Error};
use lasso::ThreadedRodeo;
use parser::ast::Ast;
use span::{FileId, Span};
use std::sync::LazyLock;

pub mod diagnostics;
pub mod lexer;
pub mod parser;
pub mod scopes;
pub mod span;

static RODEO: LazyLock<ThreadedRodeo> = LazyLock::new(ThreadedRodeo::new);

#[must_use]
pub fn parse_file(file_id: FileId, source: impl AsRef<str>) -> (Option<Ast>, Vec<Error>) {
    let (tokens, lexer_errors) = lexer::lexer()
        .parse(source.as_ref().with_context(file_id))
        .into_output_errors();

    let mut errors = lexer_errors
        .iter()
        .flat_map(|error| convert(error))
        .collect::<Vec<_>>();

    let (ast, parser_errors) = tokens.as_ref().map_or_else(
        || (None, vec![]),
        |tokens| {
            let eoi = tokens
                .last()
                .map_or_else(|| Span::zero(file_id), |(_, span)| span.to_end());

            parser::parser()
                .parse(tokens.spanned(eoi))
                .into_output_errors()
        },
    );

    errors.extend(parser_errors.iter().flat_map(|error| convert(error)));

    (ast, errors)
}

#[cfg(test)]
mod tests {
    use crate::{parse_file, span::FileId};
    use owo_colors::OwoColorize as _;
    use std::{fs::read_to_string, io::Write as _, path::PathBuf};

    #[test]
    fn parser_tests() {
        insta::glob!("../tests/parser", "*.pr", |path| {
            let relative_path = path
                .components()
                .skip_while(|c| c.as_os_str() != "tests")
                .collect::<PathBuf>();

            insta::elog!("{} {} ...", "parsing".cyan(), relative_path.display());

            let source = read_to_string(path).map_err(|_| path).unwrap();

            let (ast, errors) = parse_file(FileId::new(0), &source);

            assert!(errors.is_empty(), "errors: {errors:#?}");

            insta::with_settings!({
                description => std::fs::read_to_string(path).unwrap().trim(),
                info => &relative_path,
                snapshot_suffix => path.file_stem().unwrap().to_str().unwrap(),
            }, {
                insta::assert_yaml_snapshot!(ast.unwrap());
            });
        });
    }
}
