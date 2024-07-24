use camino::Utf8PathBuf;
use clap::Parser;
use codespan_reporting::{
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use diagnostics::report::report;
use lasso::ThreadedRodeo;
use once_cell::sync::Lazy;
use std::process::ExitCode;

pub mod diagnostics;
pub mod hir;
pub mod lexer;
pub mod parser;
pub mod scopes;
pub mod span;
pub mod typecheck;

static RODEO: Lazy<ThreadedRodeo> = Lazy::new(ThreadedRodeo::new);

#[derive(Debug, Parser)]
struct Args {
    project: Utf8PathBuf,
}

fn main() -> Result<ExitCode, Box<dyn std::error::Error>> {
    let args = Args::parse();

    if !args.project.is_dir() {
        return Err(format!("'{}' is not a directory", args.project).into());
    }

    let mut files = SimpleFiles::new();

    let (module, mut errors) = hir::ModuleTree::new(&mut files).build(args.project);

    let (_typed_module, typecheck_errors) = module.map_or_else(
        || (None, vec![]),
        |module| {
            let (tc_module, tc_errors) = typecheck::Typechecker::new().typecheck(module);

            (Some(tc_module), tc_errors)
        },
    );

    errors.extend(typecheck_errors);

    // eprintln!("{typed_module:#?}");

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let term_config = term::Config::default();

    for error in &errors {
        let diagnostic = report(error);

        term::emit(&mut writer.lock(), &term_config, &files, &diagnostic)?;
    }

    if errors.is_empty() {
        Ok(ExitCode::SUCCESS)
    } else {
        Ok(ExitCode::FAILURE)
    }
}
