use std::process::exit;

use std::fs::File;
use std::io;
use std::path::PathBuf;
use tracing_subscriber::{filter::LevelFilter, fmt, prelude::*, EnvFilter};

use antisequence::graph::{ExecutionMode, PipelineInputMode, StatisticsLevel};
use seqproc::{
    demux::DemuxConfig,
    error::{render_geometry_diagnostics, SeqprocError},
    execute::{compile_geom_typed, run, RunConfig},
    io_config::{InputLane, InputSource, OutputTarget},
    resources::ResourceBindings,
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

#[derive(Debug, Default, Clone, Copy, clap::ValueEnum)]
enum ExecutionModeArg {
    #[default]
    Auto,
    WholeGraph,
    Pipeline,
}

impl From<ExecutionModeArg> for ExecutionMode {
    fn from(value: ExecutionModeArg) -> Self {
        match value {
            ExecutionModeArg::Auto => Self::Auto,
            ExecutionModeArg::WholeGraph => Self::WholeGraph,
            ExecutionModeArg::Pipeline => Self::Pipeline,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, clap::ValueEnum)]
enum PipelineInputModeArg {
    #[default]
    WorkerLocal,
    DedicatedReader,
}

impl From<PipelineInputModeArg> for PipelineInputMode {
    fn from(value: PipelineInputModeArg) -> Self {
        match value {
            PipelineInputModeArg::WorkerLocal => Self::WorkerLocal,
            PipelineInputModeArg::DedicatedReader => Self::DedicatedReader,
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
    #[arg(short = '1', long, conflicts_with = "read1")]
    file1: Option<PathBuf>,

    /// Ordered R1 FASTQ shards; repeat the option or separate paths with commas.
    #[arg(long, value_delimiter = ',', action = clap::ArgAction::Append)]
    read1: Vec<PathBuf>,

    /// r2 fastq file
    #[arg(short = '2', long, conflicts_with = "read2")]
    file2: Option<PathBuf>,

    /// Ordered R2 FASTQ shards; repeat the option or separate paths with commas.
    #[arg(long, value_delimiter = ',', action = clap::ArgAction::Append)]
    read2: Vec<PathBuf>,

    /// Ordered R3 FASTQ shards; repeat the option or separate paths with commas.
    #[arg(long, value_delimiter = ',', action = clap::ArgAction::Append)]
    read3: Vec<PathBuf>,

    /// Ordered FASTQ shards containing interleaved complete fragments. The
    /// geometry determines whether each fragment contains 1, 2, or 3 records.
    #[arg(
        long,
        value_delimiter = ',',
        action = clap::ArgAction::Append,
        conflicts_with_all = ["file1", "read1", "file2", "read2", "read3"]
    )]
    interleaved_input: Vec<PathBuf>,

    /// r1 out fastq file
    #[arg(short = 'o', long)]
    out1: Option<PathBuf>,

    /// r2 out fastq file
    #[arg(short = 'w', long)]
    out2: Option<PathBuf>,

    /// r3 out fastq file
    #[arg(long)]
    out3: Option<PathBuf>,

    /// Gzip-compress a FASTQ output lane directed to stdout (`-`).
    #[arg(long)]
    stdout_gzip: bool,

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

    /// Select execution planning explicitly. `auto` preserves the measured
    /// low-overhead whole-graph default unless ordering requires a pipeline.
    #[arg(long, value_enum, default_value = "auto")]
    execution_mode: ExecutionModeArg,

    /// Disable conservative compile-time graph optimization. Intended for
    /// byte-equivalence tests and controlled performance comparisons.
    #[arg(long)]
    no_graph_optimization: bool,

    /// Disable only the proof-backed dead-label elimination pass.
    #[arg(long)]
    no_dead_label_elimination: bool,

    /// Disable only conservative early placement of selective filters.
    #[arg(long)]
    no_early_filter_placement: bool,

    /// Select where FASTQ parsing occurs when the pipeline backend is used.
    #[arg(long, value_enum, default_value = "worker-local")]
    pipeline_input_mode: PipelineInputModeArg,

    /// Materialize terminal projected reads instead of rendering them directly.
    /// Intended for validation and performance comparisons; direct rendering is
    /// enabled by default whenever the staged planner proves it safe.
    #[arg(long)]
    no_direct_output_rendering: bool,

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

    /// Bind a declared EFGDL 2 resource as NAME=PATH. May be repeated.
    #[arg(long = "bind", value_name = "NAME=PATH")]
    bindings: Vec<String>,

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

    /// R3 output file for reads that failed processing (unassigned)
    #[arg(long = "unassigned3")]
    unassigned3: Option<PathBuf>,
}

fn cli_output_targets(paths: [Option<PathBuf>; 3]) -> Vec<OutputTarget> {
    let Some(last) = paths.iter().rposition(Option::is_some) else {
        return Vec::new();
    };
    paths
        .into_iter()
        .take(last + 1)
        .map(|path| {
            path.map(OutputTarget::from_cli_path)
                .unwrap_or(OutputTarget::Discard)
        })
        .collect()
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
            match compile_geom_typed(&source) {
                Ok(compiled) => {
                    report_warnings(&compiled.warnings);
                    println!("valid: {}", geometry.display());
                    return;
                }
                Err(error) => {
                    report_seqproc_error(Some(&source), &error);
                    exit(error.exit_code());
                }
            }
        }
        Some(Command::Explain { geometry }) => {
            let source = read_geometry(&geometry);
            match compile_geom_typed(&source) {
                Ok(compiled) => {
                    report_warnings(&compiled.warnings);
                    let representation = format!("{compiled:#?}");
                    let normalized = compiled.get_simplified_description_string();
                    println!(
                        "Normalized EFGDL:\n{normalized}\n\nCompiled geometry:\n{representation}"
                    );
                    return;
                }
                Err(error) => {
                    report_seqproc_error(Some(&source), &error);
                    exit(error.exit_code());
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
    let interleaved_input = args.interleaved_input.clone();
    let read1 = if args.read1.is_empty() {
        args.file1.clone().into_iter().collect::<Vec<_>>()
    } else {
        args.read1.clone()
    };
    if read1.is_empty() && interleaved_input.is_empty() {
        eprintln!("error: --read1 (or legacy --file1) is required for a run");
        exit(2);
    }
    let read2 = if args.read2.is_empty() {
        args.file2.clone().into_iter().collect::<Vec<_>>()
    } else {
        args.read2.clone()
    };
    let read3 = args.read3.clone();
    if !read3.is_empty() && read2.is_empty() {
        eprintln!("error: --read3 requires --read2; input lane indices must be contiguous");
        exit(2);
    }
    let Some(file1) = interleaved_input.first().or_else(|| read1.first()).cloned() else {
        eprintln!("error: no FASTQ input remained after CLI validation");
        exit(2);
    };

    let geom = read_geometry(&geom_path);
    let geometry_digest = format!("blake3:{}", blake3::hash(geom.as_bytes()).to_hex());
    let geometry_base = std::fs::canonicalize(&geom_path)
        .ok()
        .and_then(|path| path.parent().map(std::path::Path::to_path_buf))
        .or_else(|| geom_path.parent().map(std::path::Path::to_path_buf));

    // Validate input FASTQ paths up front so a missing file surfaces as a clean
    // error instead of a panic from deep inside the read-processing engine.
    for f in read1
        .iter()
        .chain(&read2)
        .chain(&read3)
        .chain(&interleaved_input)
        .filter(|path| *path != std::path::Path::new("-"))
    {
        if !f.exists() {
            eprintln!("error: input FASTQ not found: {:?}", f);
            std::process::exit(1);
        }
    }

    let compiled_efgdl = compile_geom_typed(&geom);

    let threads = args.threads;

    let additional_args = args
        .additional
        .iter()
        .map(|a| a.as_str())
        .collect::<Vec<_>>();
    let mut resource_bindings = ResourceBindings::new();
    for binding in &args.bindings {
        let Some((name, path)) = binding.split_once('=') else {
            eprintln!("error: malformed --bind `{binding}`; expected NAME=PATH");
            exit(2);
        };
        if name.is_empty() || path.is_empty() {
            eprintln!("error: malformed --bind `{binding}`; expected NAME=PATH");
            exit(2);
        }
        if let Err(error) = resource_bindings.insert(name, path) {
            eprintln!("error: {error}");
            exit(2);
        }
    }

    // Build demux config if demux-map is provided
    let demux_config = args.demux_map.as_ref().map(|map_path| {
        let label = args.demux_label.as_deref().unwrap_or("seq2.bc1");
        DemuxConfig::new(map_path.clone(), label).with_output_dir(args.demux_out_dir.clone())
    });

    match compiled_efgdl {
        Ok(geom) => {
            report_warnings(&geom.warnings);
            let mut config = RunConfig::new(file1.clone());
            config.input2 = read2.first().cloned();
            if interleaved_input.is_empty() {
                let mut input_lanes = vec![InputLane::new(
                    read1.iter().cloned().map(InputSource::from_cli_path),
                )];
                if !read2.is_empty() {
                    input_lanes.push(InputLane::new(
                        read2.iter().cloned().map(InputSource::from_cli_path),
                    ));
                }
                if !read3.is_empty() {
                    input_lanes.push(InputLane::new(
                        read3.iter().cloned().map(InputSource::from_cli_path),
                    ));
                }
                config.input_lanes = Some(input_lanes);
            } else {
                config.interleaved_input = Some(
                    interleaved_input
                        .iter()
                        .cloned()
                        .map(InputSource::from_cli_path)
                        .collect(),
                );
            }
            config.output1 = args.out1.clone();
            config.output2 = args.out2.clone();
            config.unassigned1 = args.unassigned1.clone();
            config.unassigned2 = args.unassigned2.clone();
            if args.out3.is_some()
                || args.out1.as_deref() == Some(std::path::Path::new("-"))
                || args.out2.as_deref() == Some(std::path::Path::new("-"))
            {
                config.outputs = Some(cli_output_targets([
                    args.out1.clone(),
                    args.out2.clone(),
                    args.out3.clone(),
                ]));
            }
            if args.unassigned3.is_some()
                || args.unassigned1.as_deref() == Some(std::path::Path::new("-"))
                || args.unassigned2.as_deref() == Some(std::path::Path::new("-"))
            {
                config.unassigned_outputs = Some(cli_output_targets([
                    args.unassigned1.clone(),
                    args.unassigned2.clone(),
                    args.unassigned3.clone(),
                ]));
            }
            config.stdout_gzip = args.stdout_gzip;
            config.threads = threads;
            config.preserve_order = args.preserve_order;
            config.staged_pipeline = args.staged_pipeline;
            config.execution_mode = args.execution_mode.into();
            config.graph_optimization = !args.no_graph_optimization;
            config.graph_optimization_passes.dead_label_elimination =
                !args.no_dead_label_elimination;
            config
                .graph_optimization_passes
                .early_selective_filter_placement = !args.no_early_filter_placement;
            config.pipeline_input_mode = args.pipeline_input_mode.into();
            config.direct_output_rendering = !args.no_direct_output_rendering;
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
            config.resource_bindings = resource_bindings;
            config.geometry_base = geometry_base;
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
                    report_seqproc_error(None, &error);
                    exit(error.exit_code());
                }
            };

            if let Some(summary_path) = args.summary {
                let Some(statistics) = report.statistics else {
                    eprintln!("error: summary statistics were not collected");
                    exit(1);
                };
                let result = if summary_path == std::path::Path::new("-") {
                    serde_json::to_writer_pretty(io::stderr().lock(), &statistics)
                } else {
                    let file = File::create(&summary_path).unwrap_or_else(|error| {
                        eprintln!(
                            "error: failed to create summary {:?}: {error}",
                            summary_path
                        );
                        exit(1);
                    });
                    serde_json::to_writer_pretty(file, &statistics)
                };
                if let Err(error) = result {
                    eprintln!("error: failed to write summary {:?}: {error}", summary_path);
                    exit(1);
                }
            }
        }
        Err(error) => {
            report_seqproc_error(Some(&geom), &error);
            exit(error.exit_code());
        }
    }
}

fn read_geometry(path: &PathBuf) -> String {
    std::fs::read_to_string(path).unwrap_or_else(|error| {
        eprintln!("error: could not read geometry file {:?}: {error}", path);
        exit(1);
    })
}

fn report_seqproc_error(source: Option<&str>, error: &SeqprocError) {
    if let (Some(source), Some((_, diagnostics))) = (source, error.geometry_diagnostics()) {
        if let Err(render_error) = render_geometry_diagnostics(source, diagnostics) {
            eprintln!("error: could not render geometry diagnostics: {render_error}");
        }
    } else {
        eprintln!("error: {error}");
    }
}

fn report_warnings(warnings: &[String]) {
    for warning in warnings {
        eprintln!("warning: {warning}");
    }
}
