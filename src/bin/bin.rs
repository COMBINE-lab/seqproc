use std::process::exit;

use std::fs::File;
use std::io;
use std::path::PathBuf;
use tracing_subscriber::{filter::LevelFilter, fmt, prelude::*, EnvFilter};

use seqproc::{
    demux::DemuxConfig,
    execute::{compile_geom, interpret_with_unassigned, read_pairs_to_file},
};

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
    file2: Option<PathBuf>,

    /// r1 out fastq file
    #[arg(short = 'o', long)]
    out1: Option<PathBuf>,

    /// r2 out fastq file
    #[arg(short = 'w', long)]
    out2: Option<PathBuf>,

    /// number of threads to use
    #[arg(short, long, default_value_t = 1)]
    threads: usize,

    /// Optional path where JSON summary statistics will be written
    #[arg(short = 's', long = "summary")]
    summary: Option<PathBuf>,

    #[arg(short, long, value_parser, num_args = 1.., value_delimiter = ' ')]
    additional: Vec<String>,

    // Demultiplexing options
    /// Path to TSV file mapping barcodes to sample names (enables demultiplexing)
    #[arg(long = "demux-map")]
    demux_map: Option<PathBuf>,

    /// Barcode label to use for demultiplexing (e.g., "seq2.bc1")
    #[arg(long = "demux-label", requires = "demux_map")]
    demux_label: Option<String>,

    /// Output directory for demultiplexed files
    #[arg(long = "demux-out-dir", default_value = "demux_out")]
    demux_out_dir: PathBuf,

    // Unassigned reads output
    /// R1 output file for reads that failed processing (unassigned)
    #[arg(long = "unassigned1")]
    unassigned1: Option<PathBuf>,

    /// R2 output file for reads that failed processing (unassigned)
    #[arg(long = "unassigned2")]
    unassigned2: Option<PathBuf>,
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

    // Build demux config if demux-map is provided
    let demux_config = args.demux_map.as_ref().map(|map_path| {
        let label = args.demux_label.as_deref().unwrap_or("seq2.bc1");
        DemuxConfig::new(map_path.clone(), label).with_output_dir(args.demux_out_dir.clone())
    });

    let (out1, out2) = match (args.out1, args.out2) {
        (Some(o1), Some(o2)) => (o1, o2),
        (Some(o1), None) => (o1, PathBuf::new()),
        (None, Some(o2)) => (PathBuf::new(), o2),
        (_, _) => (PathBuf::new(), PathBuf::new()),
    };

    match compiled_efgdl {
        Ok(geom) => {
            // If no summary file is requested, preserve the existing behavior and
            // just run the transformation without collecting stats.
            if args.summary.is_none() {
                return interpret_with_unassigned(
                    &args.file1,
                    args.file2.as_deref(),
                    &out1,
                    &out2,
                    args.unassigned1.as_deref(),
                    args.unassigned2.as_deref(),
                    args.threads,
                    additional_args,
                    geom,
                    demux_config,
                );
            }

            // When a summary file is requested, run through read_pairs_to_file so
            // that we obtain SeqprocStats, then write them as JSON.
            let summary_path = args.summary.unwrap();

            // For stats collection we need concrete output paths. If the user did
            // not supply any, mirror the behavior of interpret() by discarding
            // output to /dev/null.
            let mut out1_stats = out1.clone();
            let mut out2_stats = out2.clone();
            if out1_stats.as_os_str().is_empty() {
                out1_stats = PathBuf::from("/dev/null");
            }
            if out2_stats.as_os_str().is_empty() {
                out2_stats = PathBuf::from("/dev/null");
            }

            let mut stats = match read_pairs_to_file(
                geom,
                &args.file1,
                args.file2.as_deref(),
                &out1_stats,
                &out2_stats,
                args.threads,
                additional_args,
            ) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("Error while running seqproc: {e}");
                    return;
                }
            };

            let call = std::env::args().collect::<Vec<_>>().join(" ");
            stats.call = Some(call);

            if let Ok(file) = File::create(&summary_path) {
                if let Err(e) = serde_json::to_writer_pretty(file, &stats) {
                    eprintln!("Failed to write summary JSON to {:?}: {}", summary_path, e);
                }
            } else {
                eprintln!("Failed to create summary file at {:?}", summary_path);
            }
        }
        Err(_) => {
            // Errors are already printed by compile_geom via parse_failure
            exit(1);
        }
    }
}
