use std::process::exit;

use clap::arg;
use std::io;
use std::path::PathBuf;
use tracing_subscriber::{filter::LevelFilter, fmt, prelude::*, EnvFilter};

use seqproc::execute::{compile_geom, interpret};

/// General puprose sequence preprocessor
#[derive(Debug, clap::Parser)]
pub struct Args {
    /// Path to a file containing the EFGDL specification
    #[arg(short, long)]
    geom: PathBuf,

    /// r1 fastq file
    #[arg(short = '1', long)]
    file1: PathBuf,

    /// r2 fastq file
    #[arg(short = '2', long)]
    file2: PathBuf,

    /// r1 out fastq file
    #[arg(short = 'o', long)]
    out1: Option<PathBuf>,

    /// r2 out fastq file
    #[arg(short = 'w', long)]
    out2: Option<PathBuf>,

    /// number of threads to use
    #[arg(short, long, default_value_t = 1)]
    threads: usize,

    #[arg(short, long, value_parser, num_args = 1.., value_delimiter = ' ')]
    additional: Vec<String>,
}

fn main() {
    // set up the logging. Here we will take the
    // logging level from the environment variable if
    // it is set. Otherwise we will set the default
    tracing_subscriber::registry()
        // log level to INFO
        .with(fmt::layer().with_writer(io::stderr))
        .with(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::INFO.into())
                .from_env_lossy(),
        )
        .init();

    let args: Args = <Args as clap::Parser>::parse();

    let geom = std::fs::read_to_string(&args.geom).unwrap();

    let compiled_efgdl = compile_geom(geom.clone());

    let additional_args = args
        .additional
        .iter()
        .map(|a| a.as_str())
        .collect::<Vec<_>>();

    let (out1, out2) = match (args.out1, args.out2) {
        (Some(o1), Some(o2)) => (o1, o2),
        (Some(o1), None) => (o1, PathBuf::new()),
        (None, Some(o2)) => (PathBuf::new(), o2),
        (_, _) => (PathBuf::new(), PathBuf::new()),
    };

    match compiled_efgdl {
        Ok(geom) => interpret(
            &args.file1,
            &args.file2,
            &out1,
            &out2,
            args.threads,
            additional_args,
            geom,
        ),
        Err(_) => exit(1),
    }
}
