use camino::{Utf8Path, Utf8PathBuf};
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
    filename: &Utf8Path,
    files: &mut SimpleFiles<Utf8PathBuf, String>,
    errors: &mut Vec<Error>,
) -> Option<Ast> {
    let source = std::fs::read_to_string(filename).unwrap();
    let file_id = FileId::new(files.add(filename.to_owned(), source.clone()));

    let (tokens, lexer_errors) = lexer::lexer()
        .parse(source.with_context(file_id))
        .into_output_errors();

    errors.extend(lexer_errors.iter().flat_map(|error| convert(error)));

    let (mut ast, parser_errors) = tokens.as_ref().map_or_else(
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

    if let Some(ast) = ast.as_mut() {
        insert_explicit_submodules(ast, filename, files, errors);
    }

    ast
}

pub fn insert_explicit_submodules(
    ast: &mut Ast,
    ast_filename: &Utf8Path,
    files: &mut SimpleFiles<Utf8PathBuf, String>,
    errors: &mut Vec<Error>,
) {
    ast.0
        .iter_mut()
        .filter_map(|item| match &mut item.0 {
            Item::Module(submodule) => Some(submodule),
            _ => None,
        })
        .filter_map(|submodule| match submodule.0 {
            Module::File(module_name) => Some((submodule, module_name)),
            Module::Submodule { .. } => None,
        })
        .for_each(|(submodule, module_name)| {
            let filenames = [
                ast_filename.with_file_name(format!("{}.pr", module_name.resolve())),
                ast_filename
                    .parent()
                    .unwrap()
                    .join(module_name.resolve())
                    .join("mod.pr"),
                ast_filename
                    .parent()
                    .unwrap()
                    .join(module_name.resolve())
                    .join(format!("{}.pr", module_name.resolve())),
            ];

            let mut existing = filenames.iter().filter(|filename| filename.exists());

            let existing_cloned = existing.clone();

            let filename = existing.next().unwrap_or_else(|| todo!());

            if existing.next().is_some() {
                errors.push(Error::AmbiguousModule {
                    module_name: module_name.resolve(),
                    module_name_span: module_name.1,
                    filenames: existing_cloned.map(ToString::to_string).collect(),
                });

                return;
            }

            let ast = parse_file(filename, files, errors);

            if let Some(ast) = ast {
                submodule.0 = Module::Submodule {
                    name: module_name,
                    ast,
                };
            }
        });
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
            let ast = parse_file(&path, &mut files, &mut errors);

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
