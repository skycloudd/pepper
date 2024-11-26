use camino::Utf8PathBuf;
use chumsky::{input::Input as _, span::Span as _, Parser as _};
use clap::Parser;
use codespan_reporting::{
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use pepper::{
    diagnostics::{
        error::{convert, Error},
        report::report,
    },
    lexer,
    parser::{self, ast::Ast},
    span::{FileId, Span},
};
use std::{fs::read_to_string, process::ExitCode};

#[derive(Debug, Parser)]
struct Args {
    filename: Utf8PathBuf,
}

fn main() -> ExitCode {
    let args = Args::parse();

    let mut files = SimpleFiles::new();

    let (ast, errors) = parse_file(args.filename, &mut files);

    eprintln!("ast: {ast:#?}");

    if errors.is_empty() {
        ExitCode::SUCCESS
    } else {
        emit_errors(&errors, &files);
        ExitCode::FAILURE
    }
}

fn parse_file(
    filename: impl Into<Utf8PathBuf>,
    files: &mut SimpleFiles<Utf8PathBuf, String>,
) -> (Option<Ast>, Vec<Error>) {
    let filename = filename.into();

    let source = read_to_string(&filename).unwrap();

    let file_id = FileId::new(files.add(filename, source.clone()));

    let (tokens, lexer_errors) = lexer::lexer()
        .parse(source.with_context(file_id))
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

fn emit_errors(errors: &[Error], files: &SimpleFiles<Utf8PathBuf, String>) {
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let term_config = term::Config::default();

    for error in errors {
        let diagnostic = report(error);

        term::emit(&mut writer.lock(), &term_config, files, &diagnostic).unwrap();
    }
}

#[cfg(test)]
mod tests {
    use crate::parse_file;
    use camino::Utf8PathBuf;
    use codespan_reporting::files::SimpleFiles;

    #[test]
    fn parser_tests() {
        let mut files = SimpleFiles::new();

        for entry in Utf8PathBuf::from("tests/parser").read_dir_utf8().unwrap() {
            let entry = entry.unwrap();

            assert!(entry.path().is_file(), "not a file: {:?}", entry.path());

            assert!(
                entry.path().extension().unwrap() == "pr",
                "not a .pr file: {:?}",
                entry.path()
            );

            let (ast, errors) = parse_file(entry.path(), &mut files);

            assert!(errors.is_empty(), "errors: {errors:#?}");

            insta::with_settings!({
                description => std::fs::read_to_string(entry.path()).unwrap().trim(),
                info => &entry.path().to_string(),
                snapshot_suffix => entry.path().file_stem().unwrap(),
            }, {
                insta::assert_yaml_snapshot!(ast.unwrap());
            });
        }
    }
}
