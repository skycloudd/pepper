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
use lexer::tokens::Interned;
use lower::mir::Mir;
use parser::ast::{Ast, Module};
use span::{FileId, Span};
use std::{
    fs::read_to_string,
    path::{Path, PathBuf},
    process::ExitCode,
    sync::LazyLock,
};

mod diagnostics;
mod lexer;
mod lower;
mod parser;
pub mod scopes;
pub mod span;
mod typecheck;

static RODEO: LazyLock<ThreadedRodeo> = LazyLock::new(ThreadedRodeo::new);

#[derive(Debug, Parser)]
struct Args {
    directory: PathBuf,
}

fn main() -> ExitCode {
    let args = Args::parse();

    let mut files = SimpleFiles::new();

    match get_mir(&args.directory, &mut files) {
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

fn get_mir(directory: &Path, files: &mut SimpleFiles<String, String>) -> Result<Mir, Vec<Error>> {
    let (module, mut errors) = get_module_tree(files, directory, true);

    let (typed_ast, type_errors) = typecheck::typecheck(module);

    errors.extend(type_errors);

    let mir = lower::lower(typed_ast);

    if errors.is_empty() {
        Ok(mir)
    } else {
        Err(errors)
    }
}

fn get_module_tree(
    files: &mut SimpleFiles<String, String>,
    directory: &Path,
    is_root: bool,
) -> (Module, Vec<Error>) {
    let mut errors = vec![];

    let mut base_ast = None;
    let mut children = vec![];

    let base_filename = if is_root { "main.pr" } else { "mod.pr" };

    for entry in directory.read_dir().unwrap() {
        let entry = entry.unwrap();
        let file_type = entry.file_type().unwrap();

        if file_type.is_file() {
            let path = entry.path();
            let source = read_to_string(&path).unwrap();

            let file_id = files.add(path.display().to_string(), source.clone());
            let file_id = FileId::new(file_id);

            let (ast, file_errors) = get_ast(file_id, &source);

            errors.extend(file_errors);

            if path.file_name().unwrap() == base_filename {
                base_ast = ast;
            } else if let Some(ast) = ast {
                children.push(Module {
                    name: Interned::new(path.file_stem().unwrap().to_string_lossy()),
                    ast,
                    children: vec![],
                });
            }
        } else if file_type.is_dir() {
            let (module, module_errors) = get_module_tree(files, &entry.path(), false);

            children.push(module);
            errors.extend(module_errors);
        }
    }

    let module = Module {
        name: Interned::new(directory.file_stem().unwrap().to_string_lossy()),
        ast: base_ast.unwrap(),
        children,
    };

    (module, errors)
}

fn get_ast(file_id: FileId, source: &str) -> (Option<Ast>, Vec<Error>) {
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

    (ast, errors)
}
