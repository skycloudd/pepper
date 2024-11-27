use camino::Utf8PathBuf;
use chumsky::{input::Input as _, span::Span as _, Parser as _};
use codespan_reporting::files::SimpleFiles;
use diagnostics::error::{convert, Error};
use lasso::ThreadedRodeo;
use parser::ast::{Ast, Item, Module};
use span::{FileId, Span};
use std::sync::LazyLock;

pub mod diagnostics;
pub mod lexer;
pub mod parser;
pub mod scopes;
pub mod span;

static RODEO: LazyLock<ThreadedRodeo> = LazyLock::new(ThreadedRodeo::new);

#[must_use]
pub fn parse_file(
    filename: Utf8PathBuf,
    files: &mut SimpleFiles<Utf8PathBuf, String>,
    errors: &mut Vec<Error>,
) -> Option<Ast> {
    let source = std::fs::read_to_string(&filename).unwrap();
    let file_id = FileId::new(files.add(filename, source.clone()));

    let (tokens, lexer_errors) = lexer::lexer()
        .parse(source.with_context(file_id))
        .into_output_errors();

    errors.extend(lexer_errors.iter().flat_map(|error| convert(error)));

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

    ast
}

pub fn insert_explicit_submodules(
    ast: &mut Ast,
    files: &mut SimpleFiles<Utf8PathBuf, String>,
    errors: &mut Vec<Error>,
) {
    for item in &mut ast.0 {
        if let Item::Module(module) = &mut item.0 {
            match &module.0 {
                Module::File(module_name) => {
                    let filename: Utf8PathBuf = format!("{}.pr", module_name.resolve()).into();

                    let ast = parse_file(filename, files, errors);

                    if let Some(ast) = ast {
                        module.0 = Module::Submodule {
                            name: *module_name,
                            ast,
                        };
                    }
                }
                Module::Submodule { .. } => {}
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse_file;
    use camino::Utf8PathBuf;
    use codespan_reporting::files::SimpleFiles;
    use owo_colors::OwoColorize as _;
    use std::{io::Write as _, path::PathBuf};

    #[test]
    fn parser_tests() {
        insta::glob!("../tests/parser", "**/*.pr", |path| {
            let path = Utf8PathBuf::from_path_buf(path.to_path_buf()).unwrap();

            let short_path = path
                .components()
                .skip_while(|c| c.as_os_str() != "tests")
                .collect::<PathBuf>();

            insta::elog!("{} {} ...", "parsing".cyan(), short_path.display());

            let mut files = SimpleFiles::new();
            let mut errors = vec![];
            let ast = parse_file(path.clone(), &mut files, &mut errors);

            assert!(errors.is_empty(), "errors: {errors:#?}");

            let snapshot_suffix = short_path
                .components()
                .map(|c| c.as_os_str().to_string_lossy())
                .collect::<Vec<_>>()
                .join("_");

            insta::with_settings!({
                description => std::fs::read_to_string(path).unwrap().trim(),
                snapshot_suffix => snapshot_suffix,
            }, {
                insta::assert_yaml_snapshot!(ast.unwrap());
            });
        });
    }
}
