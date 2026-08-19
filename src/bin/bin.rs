use std::process::exit;

use std::fs::File;
use std::io;
use std::path::PathBuf;
use tracing_subscriber::{filter::LevelFilter, fmt, prelude::*, EnvFilter};

use antisequence::graph::StatisticsLevel;
use seqproc::{
    demux::DemuxConfig,
    execute::{compile_geom, run, RunConfig},
};

#[derive(Debug, Clone, Copy, clap::ValueEnum)]
enum StatisticsLevelArg {
    Basic,
    Detailed,
}

impl From<StatisticsLevelArg> for StatisticsLevel {
    fn from(value: StatisticsLevelArg) -> Self {
        match value {
            StatisticsLevelArg::Basic => Self::Basic,
            StatisticsLevelArg::Detailed => Self::Detailed,
        }
    }
}

/// General puprose sequence preprocessor
#[derive(Debug, clap::Parser)]
#[command(
    name = "seqproc",
    about = "Geometry-driven FASTQ preprocessing",
    args_conflicts_with_subcommands = true
)]
struct Cli {
    #[command(subcommand)]
    command: Option<Command>,

    /// Legacy flag-only invocation; supported for one compatibility cycle.
    #[command(flatten)]
    legacy: RunArgs,
}

#[derive(Debug, clap::Subcommand)]
// Parsed once at startup; boxing the run arguments would add indirection
// without reducing steady-state processing memory.
#[allow(clippy::large_enum_variant)]
enum Command {
    /// Process FASTQ input with an EFGDL geometry.
    Run(RunArgs),
    /// Parse, compile, and semantically validate a geometry without processing reads.
    Validate { geometry: PathBuf },
    /// Print normalized EFGDL and the compiled geometry representation.
    Explain { geometry: PathBuf },
}

#[derive(Debug, Default, clap::Args)]
pub struct RunArgs {
    /// Path to a file containing the EFGDL specification
    #[arg(short, long)]
    geom: Option<PathBuf>,

    /// r1 fastq file
    #[arg(short = '1', long)]
    file1: Option<PathBuf>,

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

    /// Preserve input read order in the output. When set, output reads are
    /// guaranteed to appear in the same order as the input FASTQ. This is
    /// useful when downstream tools expect paired files to be in lock-step
    /// without re-sorting. Transformations remain parallel; a bounded reorder
    /// buffer restores batch order before output.
    #[arg(long)]
    preserve_order: bool,

    /// Use the bounded staged pipeline for unordered output as well. Ordered
    /// output enables it automatically. This can help expensive geometries,
    /// but the legacy worker path is faster for very cheap transformations.
    #[arg(long)]
    staged_pipeline: bool,

    /// Capacity of each pipeline hand-off queue, in batches. By default this
    /// is tuned from the worker count.
    #[arg(long)]
    queue_capacity: Option<usize>,

    /// Maximum batches admitted but not fully written. This is also the hard
    /// memory bound for ordered reassembly.
    #[arg(long)]
    max_in_flight_batches: Option<usize>,

    /// Reads per batch in staged execution.
    #[arg(long)]
    batch_size: Option<usize>,

    /// Gzip compression level for output paths ending in `.gz`. Level 3 is a
    /// fast default; use 6 for the previous size/speed tradeoff.
    #[arg(long, default_value_t = 3, value_parser = clap::value_parser!(u32).range(0..=9))]
    gzip_level: u32,

    /// Compress batches concurrently as concatenated gzip members. This is
    /// faster with multiple workers, but readers must support multi-member gzip.
    #[arg(long)]
    parallel_gzip: bool,

    /// Compress one logical gzip stream using background deflate-block
    /// workers. This preserves dictionary continuity across transform batches.
    #[arg(long, conflicts_with = "parallel_gzip")]
    parallel_gzip_stream: bool,

    /// Compression workers for --parallel-gzip-stream. The measured default is
    /// min(--threads, 4) and is reported separately because it adds workers.
    #[arg(long, requires = "parallel_gzip_stream")]
    gzip_threads: Option<usize>,

    /// Uncompressed bytes per deflate block for --parallel-gzip-stream.
    #[arg(long, default_value_t = 128 * 1024, requires = "parallel_gzip_stream")]
    gzip_block_size: usize,

    /// Decode .gz inputs with rapidgzip-core's adaptive speculative decoder.
    #[arg(long)]
    accelerated_gzip_input: bool,

    /// Adaptive decoder-worker ceiling per gzip input.
    #[arg(long, default_value_t = 1, requires = "accelerated_gzip_input")]
    gzip_input_threads: usize,

    /// Decoded bytes per accelerated input handoff chunk.
    #[arg(
        long,
        default_value_t = 256 * 1024,
        requires = "accelerated_gzip_input"
    )]
    gzip_input_chunk_size: usize,

    /// Optional path where JSON summary statistics will be written
    #[arg(short = 's', long = "summary")]
    summary: Option<PathBuf>,

    /// Statistics detail written by --summary. Basic records run totals with
    /// minimal instrumentation; detailed also records per-stage match-distance
    /// and ambiguity distributions.
    #[arg(long, value_enum, requires = "summary")]
    statistics_level: Option<StatisticsLevelArg>,

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

    let cli = <Cli as clap::Parser>::parse();
    let args = match cli.command {
        Some(Command::Validate { geometry }) => {
            let source = read_geometry(&geometry);
            match compile_geom(source.clone()) {
                Ok(_) => {
                    println!("valid: {}", geometry.display());
                    return;
                }
                Err(errors) => {
                    report_geometry_errors(&source, &errors);
                    exit(1);
                }
            }
        }
        Some(Command::Explain { geometry }) => {
            let source = read_geometry(&geometry);
            match compile_geom(source.clone()) {
                Ok(compiled) => {
                    let representation = format!("{compiled:#?}");
                    let normalized = compiled.get_simplified_description_string();
                    println!(
                        "Normalized EFGDL:\n{normalized}\n\nCompiled geometry:\n{representation}"
                    );
                    return;
                }
                Err(errors) => {
                    report_geometry_errors(&source, &errors);
                    exit(1);
                }
            }
        }
        Some(Command::Run(args)) => args,
        None => {
            eprintln!(
                "warning: the flag-only invocation is deprecated; use `seqproc run ...` instead"
            );
            cli.legacy
        }
    };

    let geom_path = args.geom.unwrap_or_else(|| {
        eprintln!("error: --geom is required for a run");
        exit(2);
    });
    let file1 = args.file1.unwrap_or_else(|| {
        eprintln!("error: --file1 is required for a run");
        exit(2);
    });

    let geom = read_geometry(&geom_path);
    let geometry_digest = format!("md5:{:x}", md5::compute(geom.as_bytes()));

    // Validate input FASTQ paths up front so a missing file surfaces as a clean
    // error instead of a panic from deep inside the read-processing engine.
    for f in std::iter::once(&file1).chain(args.file2.iter()) {
        if !f.exists() {
            eprintln!("error: input FASTQ not found: {:?}", f);
            std::process::exit(1);
        }
    }

    let compiled_efgdl = compile_geom(geom.clone());

    let threads = args.threads;

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

    match compiled_efgdl {
        Ok(geom) => {
            let mut config = RunConfig::new(file1.clone());
            config.input2 = args.file2.clone();
            config.output1 = args.out1.clone();
            config.output2 = args.out2.clone();
            config.unassigned1 = args.unassigned1.clone();
            config.unassigned2 = args.unassigned2.clone();
            config.threads = threads;
            config.preserve_order = args.preserve_order;
            config.staged_pipeline = args.staged_pipeline;
            config.queue_capacity = args.queue_capacity;
            config.max_in_flight_batches = args.max_in_flight_batches;
            config.batch_size = args.batch_size;
            config.gzip_level = args.gzip_level;
            config.parallel_gzip = args.parallel_gzip;
            config.parallel_gzip_stream = args.parallel_gzip_stream;
            config.gzip_threads = args.gzip_threads;
            config.gzip_block_size = args.gzip_block_size;
            config.accelerated_gzip_input = args.accelerated_gzip_input;
            config.gzip_input_threads = args.gzip_input_threads;
            config.gzip_input_chunk_size = args.gzip_input_chunk_size;
            config.additional_args = additional_args.into_iter().map(str::to_owned).collect();
            config.demux = demux_config;
            config.statistics_level = if args.summary.is_some() {
                args.statistics_level
                    .unwrap_or(StatisticsLevelArg::Detailed)
                    .into()
            } else {
                StatisticsLevel::Off
            };
            config.call = Some(std::env::args().collect::<Vec<_>>().join(" "));
            config.geometry_digest = Some(geometry_digest);

            let report = match run(config, geom) {
                Ok(report) => report,
                Err(error) => {
                    eprintln!("error: seqproc execution failed: {error}");
                    exit(1);
                }
            };

            if let Some(summary_path) = args.summary {
                let Some(statistics) = report.statistics else {
                    eprintln!("error: summary statistics were not collected");
                    exit(1);
                };
                let file = File::create(&summary_path).unwrap_or_else(|error| {
                    eprintln!(
                        "error: failed to create summary {:?}: {error}",
                        summary_path
                    );
                    exit(1);
                });
                if let Err(error) = serde_json::to_writer_pretty(file, &statistics) {
                    eprintln!("error: failed to write summary {:?}: {error}", summary_path);
                    exit(1);
                }
            }
        }
        Err(errs) => {
            report_geometry_errors(&geom, &errs);
            exit(1);
        }
    }
}

fn read_geometry(path: &PathBuf) -> String {
    std::fs::read_to_string(path).unwrap_or_else(|error| {
        eprintln!("error: could not read geometry file {:?}: {error}", path);
        exit(1);
    })
}

fn report_geometry_errors(source: &str, errors: &[chumsky::error::Rich<'static, String>]) {
    use ariadne::{Color, Label, Report, ReportKind, Source};
    for error in errors {
        Report::build(ReportKind::Error, ((), error.span().into_range()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(error.to_string())
            .with_label(
                Label::new(((), error.span().into_range()))
                    .with_message(error.reason().to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .print(Source::from(source))
            .unwrap();
    }
}
