use camino::{Utf8Path, Utf8PathBuf};
use chumsky::{input::Input as _, span::Span as _, Parser as _};
use clap::Parser;
use codespan_reporting::{
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use diagnostics::{
    error::{convert, Error},
    report::report,
};
use lasso::ThreadedRodeo;
use lower::mir::Mir;
use span::{FileId, Span};
use std::{fs::read_to_string, process::ExitCode, sync::LazyLock};

pub mod diagnostics;
mod lexer;
mod lower;
mod parser;
pub mod scopes;
pub mod span;
mod typecheck;

static RODEO: LazyLock<ThreadedRodeo> = LazyLock::new(ThreadedRodeo::new);

#[derive(Debug, Parser)]
struct Args {
    filename: Utf8PathBuf,
}

fn main() -> ExitCode {
    let args = Args::parse();

    let mut files = SimpleFiles::new();

    match run_mir(&args, &mut files) {
        Ok(mir) => {
            println!("{mir:#?}");
            ExitCode::SUCCESS
        }
        Err(errors) => {
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let term_config = term::Config::default();

            for error in &errors {
                let diagnostic = report(error);

                term::emit(&mut writer.lock(), &term_config, &files, &diagnostic).unwrap();
            }

            ExitCode::FAILURE
        }
    }
}

fn run_mir<'path>(
    args: &'path Args,
    files: &mut SimpleFiles<&'path Utf8Path, String>,
) -> Result<Mir, Vec<Error>> {
    if !args.filename.is_file() {
        return Err(vec![Error::Ice(format!(
            "'{}' is not a file",
            args.filename
        ))]);
    }

    let source =
        read_to_string(&args.filename).map_err(|error| vec![Error::Ice(error.to_string())])?;

    let file_id = FileId::new(files.add(&args.filename, source.to_string()));

    let mut errors = vec![];

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

    let (typed_ast, typecheck_errors) = ast.map_or_else(
        || (None, vec![]),
        |ast| {
            let (typed_ast, errors) = typecheck::typecheck(ast);
            (Some(typed_ast), errors)
        },
    );

    errors.extend(typecheck_errors);

    if errors.is_empty() {
        let mir = lower::lower(typed_ast.unwrap());

        Ok(mir)
    } else {
        Err(errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mir_tests() {
        for file in std::fs::read_dir("tests/mir").unwrap() {
            let file = file.unwrap();

            let args = Args {
                filename: file.path().try_into().unwrap(),
            };

            let mir = run_mir(&args, &mut SimpleFiles::new())
                .map_err(|err| (args, err))
                .unwrap();

            insta::with_settings!({
                description => std::fs::read_to_string(file.path()).unwrap().trim(),
                info => &file.path().to_string_lossy(),
                snapshot_suffix => file.path().file_stem().unwrap().to_string_lossy(),
            }, {
                insta::assert_yaml_snapshot!(mir);
            });
        }
    }
}
