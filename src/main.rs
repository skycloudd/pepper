use camino::{Utf8Path, Utf8PathBuf};
use clap::Parser;
use codespan_reporting::{
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use config::{Config, Linker, WithLinker};
use core::{fmt::Write as _, str::FromStr as _};
use owo_colors::OwoColorize as _;
use pepper::{
    compile,
    diagnostics::{report::report, Diagnostics},
    lexer::tokens::FileId,
    SourceProgram,
};
#[cfg(unix)]
use std::os::unix::process::ExitStatusExt as _;
use std::{error::Error, io::Write as _};
use target_lexicon::Triple;

mod config;

#[derive(Parser)]
struct Args {
    filename: Utf8PathBuf,

    #[clap(short, long)]
    out: Option<Utf8PathBuf>,

    #[clap(short, long)]
    target: Option<String>,

    #[clap(short, long)]
    config: Option<Utf8PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let pepper_config: Config = toml::from_str(&std::fs::read_to_string(
        args.config
            .as_deref()
            .unwrap_or_else(|| "pepper_config.toml".into()),
    )?)?;

    let target_triple = args
        .target
        .as_deref()
        .map_or_else(|| Ok(Triple::host()), Triple::from_str)
        .map_err(|err| format!("invalid target triple: {err}"))?;

    let db = pepper::db::Database::default();

    let mut files = SimpleFiles::new();

    let text = std::fs::read_to_string(&args.filename)?;

    let id = files.add(args.filename, text.clone());
    let file_id = FileId::new(&db, id);

    let source_program = SourceProgram::new(&db, text, file_id);

    let diagnostics =
        compile::accumulated::<Diagnostics>(&db, source_program, target_triple.clone());

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let term_config = term::Config::default();

    for error in &diagnostics {
        let diag = report(&db, error);

        term::emit(&mut writer.lock(), &term_config, &files, &diag)?;
    }

    if diagnostics.is_empty() {
        let target_config = match target_triple.to_string().as_str() {
            "aarch64-apple-darwin" => &pepper_config.target.aarch64_apple_darwin as &dyn WithLinker,
            _ => return Err(format!("unsupported target triple: {target_triple}").into()),
        };

        let linker_config = target_config.linker();

        let output_filename = args.out.unwrap_or_else(|| "a.out".into());
        let object_filename = output_filename.with_extension("o");
        let program_filename = output_filename;

        let output = compile::get(&db, source_program, target_triple)
            .as_ref()
            .unwrap();

        let mut output_file = std::fs::File::create(&object_filename)?;
        output_file.write_all(output)?;

        println!("Compilation successful");

        run_linker(linker_config, object_filename, program_filename)?;
    }

    Ok(())
}

fn run_linker<P: AsRef<Utf8Path>>(
    linker_config: &Linker,
    object_filename: P,
    program_filename: P,
) -> Result<(), Box<dyn Error>> {
    let object_filename = object_filename.as_ref();
    let program_filename = program_filename.as_ref();

    println!(
        "Linking with: {}",
        format!(
            "{} {object_filename} -o {program_filename}{}",
            linker_config.command,
            linker_config
                .flags
                .iter()
                .fold(String::new(), |mut init, flag| {
                    write!(init, " {flag}").unwrap();
                    init
                }),
        )
        .green()
    );

    let status = std::process::Command::new(&linker_config.command)
        .arg(object_filename)
        .arg("-o")
        .arg(program_filename)
        .args(&linker_config.flags)
        .status()?;

    if status.success() {
        Ok(())
    } else {
        Err(format!(
            "{} exited with {}",
            linker_config.command,
            status.code().map_or_else(
                #[cfg(unix)]
                || format!("signal {}", status.signal().unwrap()),
                #[cfg(not(unix))]
                || "unknown exit code".to_string(),
                |code| format!("code {code}")
            )
        )
        .into())
    }
}
