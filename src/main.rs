use camino::Utf8PathBuf;
use clap::Parser;
use codespan_reporting::{
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use core::str::FromStr as _;
use owo_colors::OwoColorize as _;
use pepper::{
    compile,
    diagnostics::{report::report, Diagnostics},
    lexer::tokens::FileId,
    SourceProgram,
};
use std::{io::Write as _, os::unix::process::ExitStatusExt as _};
use target_lexicon::Triple;

#[derive(Parser)]
struct Args {
    filename: Utf8PathBuf,

    #[clap(short, long)]
    out: Utf8PathBuf,

    #[clap(short, long)]
    target: Option<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let target_triple = args
        .target
        .as_deref()
        .map_or_else(|| Ok(Triple::host()), Triple::from_str)
        .map_err(|err| format!("invalid target triple: {err}"))?;

    println!("Target: {target_triple}");

    let db = pepper::db::Database::default();

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

    let mut files = SimpleFiles::new();

    let text = std::fs::read_to_string(&args.filename)?;

    let id = files.add(args.filename, text.clone());
    let file_id = FileId::new(&db, id);

    let source_program = SourceProgram::new(&db, text, file_id);

    let diagnostics =
        compile::accumulated::<Diagnostics>(&db, source_program, target_triple.clone());

    for error in &diagnostics {
        let diag = report(&db, error);

        term::emit(&mut writer.lock(), &config, &files, &diag)?;
    }

    if diagnostics.is_empty() {
        let linker = "cc";

        let object_file = args.out.with_extension("o");
        let program_file = args.out;

        let output = compile::get(&db, source_program, target_triple)
            .as_ref()
            .unwrap();

        let mut output_file = std::fs::File::create(&object_file)?;
        output_file.write_all(output)?;

        println!("Compilation successful");
        println!("Output written to `{}`", object_file.green());
        println!(
            "Running `{}` to link the object file",
            format!("{linker} {object_file} -o {program_file}").yellow()
        );

        let mut link_command = std::process::Command::new(linker)
            .arg(object_file)
            .arg("-o")
            .arg(&program_file)
            .spawn()?;

        let status = link_command.wait()?;

        if !status.success() {
            return Err(format!(
                "{linker} exited with {}",
                status.code().map_or_else(
                    || format!("signal {}", status.signal().unwrap()),
                    |code| format!("code {code}")
                )
            )
            .into());
        }

        println!("Linked object file to `{}`", program_file.green());
    }

    Ok(())
}
