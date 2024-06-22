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
    compile,
    diagnostics::{report::report, Diagnostics},
    lexer::tokens::FileId,
    SourceProgram,
};

#[derive(Parser)]
struct Args {
    filename: Utf8PathBuf,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let db = pepper::db::Database::default();

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

    let mut files = SimpleFiles::new();

    let text = std::fs::read_to_string(&args.filename)?;

    let id = files.add(args.filename, text.clone());
    let file_id = FileId::new(&db, id);

    let source_program = SourceProgram::new(&db, text, file_id);

    let diagnostics = compile::accumulated::<Diagnostics>(&db, source_program);

    for error in diagnostics {
        let diag = report(&db, &error);

        term::emit(&mut writer.lock(), &config, &files, &diag)?;
    }

    Ok(())
}
