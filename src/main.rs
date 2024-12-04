use camino::Utf8PathBuf;
use clap::Parser;
use codespan_reporting::{
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use pepper::{
    diagnostics::{error::Error, report::report},
    typechecker,
};
use std::process::ExitCode;

#[derive(Debug, Parser)]
struct Args {
    filename: Utf8PathBuf,

    #[clap(short, long)]
    print: Vec<String>,
}

fn main() -> ExitCode {
    let args = Args::parse();

    run(&args)
}

fn run(args: &Args) -> ExitCode {
    let mut files = SimpleFiles::new();

    let mut errors = vec![];
    let ast = pepper::parse_file(&args.filename, &mut files, &mut errors);

    if args.print.contains(&"ast".into()) {
        println!("ast: {ast:#?}");
    }

    let typed_ast = ast.map(|ast| typechecker::typecheck(ast, &mut errors));

    if args.print.contains(&"typed_ast".into()) {
        println!("typed_ast: {typed_ast:#?}");
    }

    if errors.is_empty() {
        ExitCode::SUCCESS
    } else {
        emit_errors(&errors, &files);
        ExitCode::FAILURE
    }
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
    use crate::{run, Args};
    use camino::Utf8PathBuf;
    use owo_colors::OwoColorize as _;
    use std::path::PathBuf;
    use std::{io::Write as _, process::ExitCode};

    #[test]
    fn run_tests() {
        insta::glob!("../tests/parser", "**/*.pr", |path| {
            let filename = Utf8PathBuf::from_path_buf(path.to_path_buf()).unwrap();

            let short_filename = filename
                .components()
                .skip_while(|c| c.as_os_str() != "tests")
                .collect::<PathBuf>();

            insta::elog!("{} {} ...", "running".cyan(), short_filename.display());

            assert_eq!(
                run(&Args {
                    filename,
                    print: vec![]
                }),
                ExitCode::SUCCESS
            );
        });
    }
}
