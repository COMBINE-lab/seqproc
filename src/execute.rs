use std::{
    fs::File,
    io::{self, BufWriter, Write},
    path::{Path, PathBuf},
    thread,
};

use antisequence::expr::fmt_expr;
use antisequence::graph::TryOp;
use antisequence::graph::*;
use anyhow::{anyhow, bail, Result as AnyResult};
use chumsky::{error::Rich, input::Input, Parser};
use flate2::{write::GzEncoder, Compression};
use nix::sys::stat;
use nix::unistd;
use serde::Serialize;
use tempfile::tempdir;
use tracing::info;

use crate::{
    compile::{compile, CompiledData},
    demux::DemuxConfig,
    error::{
        ExecutionConfigError, GeometryDiagnostic, GeometryStage, InputTopologyError,
        OutputTopologyError, SeqprocError, SeqprocResult,
    },
    io_config::{InputLane, InputSource, OutputTarget, MAX_INPUT_LANES},
    lexer,
    parser::parser,
    resources::{ResourceBindings, ResourceResolutionReport},
};

const MIN_PARALLEL_GZIP_BLOCK_SIZE: usize = 32 * 1024;

fn graph_error_contains(
    error: &antisequence::errors::Error,
    predicate: &impl Fn(&antisequence::errors::Error) -> bool,
) -> bool {
    predicate(error)
        || match error {
            antisequence::errors::Error::WorkerFailures { errors, .. } => errors
                .iter()
                .any(|error| graph_error_contains(error, predicate)),
            _ => false,
        }
}

fn is_fastq_input_error(error: &antisequence::errors::Error) -> bool {
    graph_error_contains(error, &|error| {
        matches!(
            error,
            antisequence::errors::Error::ParseRecord { .. }
                | antisequence::errors::Error::UnpairedRead(_)
                | antisequence::errors::Error::ShardCountMismatch { .. }
                | antisequence::errors::Error::ShardRecordCountMismatch { .. }
                | antisequence::errors::Error::IncompleteInterleavedFragment { .. }
        )
    })
}

fn is_broken_pipe_error(error: &antisequence::errors::Error) -> bool {
    graph_error_contains(error, &|error| {
        let antisequence::errors::Error::BytesIo(source) = error else {
            return false;
        };
        source
            .downcast_ref::<io::Error>()
            .is_some_and(|error| error.kind() == io::ErrorKind::BrokenPipe)
    })
}

fn configure_fastq_output(
    output: OutputFastqFileOp,
    config: &RunConfig,
    gzip_threads: usize,
) -> SeqprocResult<OutputFastqFileOp> {
    let output = output
        .try_with_gzip_level(config.gzip_level)
        .map_err(|source| SeqprocError::FastqOutput { source })?;
    if config.parallel_gzip_stream {
        output
            .try_with_parallel_gzip_stream(gzip_threads, config.gzip_block_size)
            .map_err(|source| SeqprocError::FastqOutput { source })
    } else {
        Ok(output.with_parallel_gzip_members(config.parallel_gzip))
    }
}

fn output_targets_from_legacy(config: &RunConfig, output_arity: usize) -> Vec<OutputTarget> {
    let mut targets = vec![config
        .output1
        .clone()
        .map(OutputTarget::Path)
        .unwrap_or(OutputTarget::Discard)];
    if output_arity > 1 {
        if let Some(path) = &config.output2 {
            targets.push(OutputTarget::Path(path.clone()));
        }
    }
    targets
}

fn unassigned_targets_from_legacy(config: &RunConfig, input_arity: usize) -> Vec<OutputTarget> {
    let Some(last) = [config.unassigned1.as_ref(), config.unassigned2.as_ref()]
        .into_iter()
        .take(input_arity)
        .rposition(|path| path.is_some())
    else {
        return Vec::new();
    };
    [config.unassigned1.as_ref(), config.unassigned2.as_ref()]
        .into_iter()
        .take(last + 1)
        .map(|path| {
            path.cloned()
                .map(OutputTarget::Path)
                .unwrap_or(OutputTarget::Discard)
        })
        .collect()
}

fn target_file_name(target: &OutputTarget) -> Option<String> {
    match target {
        OutputTarget::Path(path) => Some(path.to_string_lossy().into_owned()),
        OutputTarget::Discard => Some("/dev/null".to_owned()),
        OutputTarget::Stdout => None,
    }
}

fn writer_for_target(
    target: &OutputTarget,
    stdout_gzip: bool,
    gzip_level: u32,
) -> SeqprocResult<Box<dyn Write + Send>> {
    match target {
        OutputTarget::Path(path) => {
            let file = File::create(path).map_err(|source| SeqprocError::Io {
                operation: "create FASTQ output",
                target: path.clone(),
                source,
            })?;
            let writer = BufWriter::new(file);
            if path.to_string_lossy().ends_with(".gz") {
                Ok(Box::new(GzEncoder::new(
                    writer,
                    Compression::new(gzip_level),
                )))
            } else {
                Ok(Box::new(writer))
            }
        }
        OutputTarget::Stdout => {
            let writer = BufWriter::new(io::stdout());
            if stdout_gzip {
                Ok(Box::new(GzEncoder::new(
                    writer,
                    Compression::new(gzip_level),
                )))
            } else {
                Ok(Box::new(writer))
            }
        }
        OutputTarget::Discard => Ok(Box::new(io::sink())),
    }
}

fn add_fastq_targets(
    graph: &mut Graph,
    targets: &[OutputTarget],
    config: &RunConfig,
    gzip_threads: usize,
) -> SeqprocResult<()> {
    if targets.is_empty() {
        return Ok(());
    }
    if targets
        .iter()
        .all(|target| !matches!(target, OutputTarget::Stdout))
    {
        let files = targets
            .iter()
            .filter_map(target_file_name)
            .collect::<Vec<_>>();
        graph.add(configure_fastq_output(
            OutputFastqFileOp::from_files(files),
            config,
            gzip_threads,
        )?);
        return Ok(());
    }
    if config.parallel_gzip || config.parallel_gzip_stream {
        return Err(OutputTopologyError::ParallelGzipStdout.into());
    }
    let writers = targets
        .iter()
        .map(|target| writer_for_target(target, config.stdout_gzip, config.gzip_level))
        .collect::<SeqprocResult<Vec<_>>>()?;
    graph.add(OutputFastqOp::from_writers(writers));
    Ok(())
}

#[derive(Debug)]
pub struct FifoSeqprocData {
    pub r1_fifo: PathBuf,
    pub r2_fifo: PathBuf,
    pub join_handle: thread::JoinHandle<AnyResult<SeqprocStats>>,
}

#[derive(Debug, Clone)]
pub struct RunConfig {
    pub input1: PathBuf,
    pub input2: Option<PathBuf>,
    /// Primary grouped input representation. `None` uses the legacy
    /// `input1`/`input2` fields.
    pub input_lanes: Option<Vec<InputLane>>,
    /// Ordered shards containing complete interleaved fragments. Geometry
    /// input arity determines the records per fragment.
    pub interleaved_input: Option<Vec<InputSource>>,
    pub output1: Option<PathBuf>,
    pub output2: Option<PathBuf>,
    /// Primary output representation. `None` uses `output1`/`output2`.
    pub outputs: Option<Vec<OutputTarget>>,
    pub unassigned1: Option<PathBuf>,
    pub unassigned2: Option<PathBuf>,
    /// Unassigned output representation. `None` uses the legacy fields.
    pub unassigned_outputs: Option<Vec<OutputTarget>>,
    /// Compress the stdout FASTQ stream. Required because stdout has no suffix.
    pub stdout_gzip: bool,
    pub threads: usize,
    pub preserve_order: bool,
    pub staged_pipeline: bool,
    /// Automatic, whole-graph, or bounded-pipeline execution selection.
    pub execution_mode: ExecutionMode,
    /// Apply conservative compile-time graph optimization passes.
    pub graph_optimization: bool,
    /// Select whether pipeline workers parse their own batches or receive
    /// batches from a dedicated reader thread.
    pub pipeline_input_mode: PipelineInputMode,
    /// Render a safe terminal FASTQ projection directly into output buffers
    /// when staged execution can prove that intermediate records are dead.
    pub direct_output_rendering: bool,
    pub queue_capacity: Option<usize>,
    pub max_in_flight_batches: Option<usize>,
    pub batch_size: Option<usize>,
    /// Gzip compression level for output paths ending in `.gz`.
    pub gzip_level: u32,
    /// Compress independent batches into concatenated gzip members.
    pub parallel_gzip: bool,
    /// Compress one logical gzip stream with a bounded deflate-block pool.
    pub parallel_gzip_stream: bool,
    /// Compression workers for the single-stream backend. `None` uses the
    /// measured `min(transform threads, 4)` default.
    pub gzip_threads: Option<usize>,
    /// Uncompressed bytes accumulated into each parallel deflate block.
    pub gzip_block_size: usize,
    /// Decode `.gz` inputs through rapidgzip-core's adaptive parallel reader.
    pub accelerated_gzip_input: bool,
    /// Adaptive decoder-worker ceiling per gzip input.
    pub gzip_input_threads: usize,
    /// Decoded bytes assigned to each accelerated input handoff chunk.
    pub gzip_input_chunk_size: usize,
    pub additional_args: Vec<String>,
    /// Named EFGDL 2 resource bindings supplied by the caller.
    pub resource_bindings: ResourceBindings,
    /// Base directory used for relative EFGDL 2 literal/default resources.
    pub geometry_base: Option<PathBuf>,
    pub demux: Option<DemuxConfig>,
    /// Runtime instrumentation level. `Off` leaves data-dependent statistics
    /// collection disabled; `Basic` collects run totals; `Detailed` also
    /// collects per-match distance and ambiguity distributions.
    pub statistics_level: StatisticsLevel,
    /// Backward-compatible switch. When true and `statistics_level` is `Off`,
    /// detailed statistics are collected.
    pub collect_statistics: bool,
    pub call: Option<String>,
    pub geometry_digest: Option<String>,
}

impl RunConfig {
    pub fn new(input1: impl Into<PathBuf>) -> Self {
        Self {
            input1: input1.into(),
            input2: None,
            input_lanes: None,
            interleaved_input: None,
            output1: None,
            output2: None,
            outputs: None,
            unassigned1: None,
            unassigned2: None,
            unassigned_outputs: None,
            stdout_gzip: false,
            threads: 1,
            preserve_order: false,
            staged_pipeline: false,
            execution_mode: ExecutionMode::Auto,
            graph_optimization: true,
            pipeline_input_mode: PipelineInputMode::WorkerLocal,
            direct_output_rendering: true,
            queue_capacity: None,
            max_in_flight_batches: None,
            batch_size: None,
            // Development profiling selected level 3 as the speed/size
            // default; users can request the traditional level 6 explicitly.
            gzip_level: 3,
            parallel_gzip: false,
            parallel_gzip_stream: false,
            gzip_threads: None,
            gzip_block_size: 128 * 1024,
            accelerated_gzip_input: false,
            gzip_input_threads: 1,
            gzip_input_chunk_size: 256 * 1024,
            additional_args: Vec::new(),
            resource_bindings: ResourceBindings::new(),
            geometry_base: None,
            demux: None,
            statistics_level: StatisticsLevel::Off,
            collect_statistics: false,
            call: None,
            geometry_digest: None,
        }
    }

    fn effective_statistics_level(&self) -> StatisticsLevel {
        if self.statistics_level.is_enabled() {
            self.statistics_level
        } else if self.collect_statistics {
            StatisticsLevel::Detailed
        } else {
            StatisticsLevel::Off
        }
    }

    pub fn with_input_lanes(mut self, lanes: impl IntoIterator<Item = InputLane>) -> Self {
        self.input_lanes = Some(lanes.into_iter().collect());
        self
    }

    pub fn with_outputs(mut self, outputs: impl IntoIterator<Item = OutputTarget>) -> Self {
        self.outputs = Some(outputs.into_iter().collect());
        self
    }

    pub fn with_interleaved_input(
        mut self,
        sources: impl IntoIterator<Item = impl Into<InputSource>>,
    ) -> Self {
        self.interleaved_input = Some(sources.into_iter().map(Into::into).collect());
        self
    }

    fn effective_input_lanes(&self) -> SeqprocResult<Vec<InputLane>> {
        let lanes = self.input_lanes.clone().unwrap_or_else(|| {
            let mut lanes = vec![InputLane::single(self.input1.clone())];
            if let Some(input2) = &self.input2 {
                lanes.push(InputLane::single(input2.clone()));
            }
            lanes
        });
        if lanes.is_empty() {
            return Err(InputTopologyError::MissingLanes.into());
        }
        if lanes.len() > MAX_INPUT_LANES {
            return Err(InputTopologyError::TooManyLanes {
                maximum: MAX_INPUT_LANES,
                observed: lanes.len(),
            }
            .into());
        }
        let expected_shards = lanes[0].shards.len();
        if expected_shards == 0 {
            return Err(InputTopologyError::EmptyLane { lane: 1 }.into());
        }
        for (lane, input) in lanes.iter().enumerate() {
            if input.shards.is_empty() {
                return Err(InputTopologyError::EmptyLane { lane: lane + 1 }.into());
            }
            if input.shards.len() != expected_shards {
                return Err(InputTopologyError::ShardCountMismatch {
                    lane: lane + 1,
                    expected: expected_shards,
                    observed: input.shards.len(),
                }
                .into());
            }
        }
        Ok(lanes)
    }
}

#[derive(Debug, Serialize)]
pub struct RunReport {
    pub effective_threads: usize,
    pub ordered_output: bool,
    pub statistics_level: StatisticsLevel,
    pub gzip_compression_level: u32,
    pub parallel_gzip_members: bool,
    pub parallel_gzip_stream: bool,
    pub gzip_compression_threads: usize,
    pub gzip_block_size: usize,
    pub gzip_input_backend: String,
    pub gzip_input_threads: usize,
    pub gzip_input_chunk_size: usize,
    pub graph_optimization: GraphOptimizationReport,
    pub execution_plan: ExecutionPlan,
    pub resources: ResourceResolutionReport,
    pub input_topology: Vec<Vec<String>>,
    pub input_layout: String,
    pub input_arity: usize,
    pub output_arity: usize,
    pub output_topology: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pipeline: Option<PipelineReport>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub statistics: Option<SeqprocStats>,
}

#[derive(Debug, Serialize)]
pub struct SeqprocStats {
    pub schema_version: String,
    pub seqproc_version: String,
    pub statistics_level: StatisticsLevel,
    pub call: Option<String>,
    pub geometry_digest: Option<String>,
    pub ordering_mode: String,
    pub effective_threads: usize,
    pub gzip_compression_level: u32,
    pub parallel_gzip_members: bool,
    pub parallel_gzip_stream: bool,
    pub gzip_compression_threads: usize,
    pub gzip_block_size: usize,
    pub gzip_input_backend: String,
    pub gzip_input_threads: usize,
    pub gzip_input_chunk_size: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub graph_optimization: Option<GraphOptimizationReport>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub execution_plan: Option<ExecutionPlan>,
    pub resources: ResourceResolutionReport,
    pub input_topology: Vec<Vec<String>>,
    pub input_layout: String,
    pub input_arity: usize,
    pub output_arity: usize,
    pub output_topology: Vec<String>,

    pub n_fastqs: u32,
    pub n_processed: u64,
    pub n_reads_max: u64,

    pub total_fragments: u64,
    pub accepted_fragments: u64,
    pub rejected_fragments: u64,
    pub failed_parsing: u64,
    pub rejection_reasons: Vec<RejectionReasonCount>,
    pub read_length_mean: Vec<f64>,
    pub read_length_min: Vec<u64>,
    pub read_length_max: Vec<u64>,
    pub shard_read_counts: Vec<Vec<u64>>,
    pub match_distance_stats: Vec<MatchDistanceStats>,
}

#[derive(Debug, Serialize)]
pub struct MatchDistanceStats {
    pub stage_index: usize,
    pub label: String,
    pub attempted: u64,
    pub matched: u64,
    pub unmatched: u64,
    pub distance_histogram: Vec<DistanceBin>,
    pub ambiguity: AmbiguityStats,
}

#[derive(Debug, Serialize)]
pub struct AmbiguityStats {
    pub total: u64,
    pub accepted: u64,
    pub dropped: u64,
    pub resolved_first: u64,
    pub resolved_random: u64,
    pub resolved_quality: u64,
    pub position_total: u64,
    pub position_dropped: u64,
    pub position_resolved_leftmost: u64,
    pub position_resolved_rightmost: u64,
    pub position_resolved_quality: u64,
}

#[derive(Debug, Serialize)]
pub struct DistanceBin {
    pub distance: usize,
    pub count: u64,
}

#[derive(Debug, Serialize)]
pub struct RejectionReasonCount {
    pub reason: String,
    pub count: u64,
}

/// Execute a compiled geometry through one pipeline for normal, summary,
/// demultiplexed, and unassigned-read runs.
pub fn run(config: RunConfig, compiled_data: CompiledData) -> SeqprocResult<RunReport> {
    if config.threads == 0 {
        return Err(ExecutionConfigError::ThreadCount(config.threads).into());
    }
    if config.gzip_level > 9 {
        return Err(ExecutionConfigError::GzipLevel(config.gzip_level).into());
    }
    if config.parallel_gzip && config.parallel_gzip_stream {
        return Err(ExecutionConfigError::ConflictingGzipModes.into());
    }
    if config.staged_pipeline && config.execution_mode == ExecutionMode::WholeGraph {
        return Err(ExecutionConfigError::StagedWholeGraphConflict.into());
    }
    if config.preserve_order
        && config.execution_mode == ExecutionMode::WholeGraph
        && config.threads > 1
    {
        return Err(ExecutionConfigError::OrderedWholeGraph.into());
    }
    // The real-data crossover sweep found that letting the compression pool
    // grow to every transform worker oversubscribed short-read workloads.
    // Four is the best balanced default; explicit settings remain available.
    let gzip_threads = config.gzip_threads.unwrap_or(config.threads.min(4));
    if config.parallel_gzip_stream && gzip_threads == 0 {
        return Err(ExecutionConfigError::GzipThreadCount.into());
    }
    if config.accelerated_gzip_input && config.gzip_input_threads == 0 {
        return Err(ExecutionConfigError::GzipInputThreadCount.into());
    }
    if config.accelerated_gzip_input && config.gzip_input_chunk_size == 0 {
        return Err(ExecutionConfigError::GzipInputChunkSize.into());
    }
    if config.parallel_gzip_stream && config.gzip_block_size < MIN_PARALLEL_GZIP_BLOCK_SIZE {
        return Err(ExecutionConfigError::GzipBlockSize {
            minimum: MIN_PARALLEL_GZIP_BLOCK_SIZE,
            observed: config.gzip_block_size,
        }
        .into());
    }
    if config.parallel_gzip_stream
        && (config.demux.is_some()
            || config.unassigned_outputs.is_some()
            || config.unassigned1.is_some()
            || config.unassigned2.is_some())
    {
        return Err(OutputTopologyError::ParallelStreamVariableOutputs.into());
    }
    let effective_gzip_threads = if config.parallel_gzip_stream {
        gzip_threads
    } else if config.parallel_gzip {
        config.threads
    } else {
        1
    };

    if config.interleaved_input.is_some() && config.input_lanes.is_some() {
        return Err(InputTopologyError::ConflictingLayouts.into());
    }
    let interleaved_sources = config.interleaved_input.clone();
    let input_lanes = if interleaved_sources.is_some() {
        Vec::new()
    } else {
        config.effective_input_lanes()?
    };
    let input_lane_count = if interleaved_sources.is_some() {
        compiled_data.geometry.len()
    } else {
        input_lanes.len()
    };
    if interleaved_sources.is_none() && compiled_data.geometry.len() != input_lane_count {
        return Err(InputTopologyError::GeometryArityMismatch {
            required: compiled_data.geometry.len(),
            supplied: input_lane_count,
        }
        .into());
    }
    if let Some(sources) = &interleaved_sources {
        if sources.is_empty() {
            return Err(InputTopologyError::EmptyInterleavedInput.into());
        }
        if !(1..=MAX_INPUT_LANES).contains(&input_lane_count) {
            return Err(InputTopologyError::UnsupportedInterleavedArity {
                maximum: MAX_INPUT_LANES,
                observed: input_lane_count,
            }
            .into());
        }
    }

    let output_arity = compiled_data
        .transformation
        .as_ref()
        .map_or(input_lane_count, Vec::len);
    if config.demux.is_none() {
        if let Some(transformations) = &compiled_data.transformation {
            if config.outputs.is_none()
                && transformations.len() == 2
                && (config.output1.is_none() || config.output2.is_none())
            {
                return Err(OutputTopologyError::LegacyPairedOutputRequired.into());
            }
        }
    }
    let primary_targets = config
        .outputs
        .clone()
        .unwrap_or_else(|| output_targets_from_legacy(&config, output_arity));
    if config.demux.is_none() && primary_targets.len() != output_arity {
        return Err(OutputTopologyError::ArityMismatch {
            required: output_arity,
            supplied: primary_targets.len(),
        }
        .into());
    }
    let unassigned_targets = config
        .unassigned_outputs
        .clone()
        .unwrap_or_else(|| unassigned_targets_from_legacy(&config, input_lane_count));
    if unassigned_targets.len() > input_lane_count {
        return Err(OutputTopologyError::TooManyUnassigned {
            supplied: unassigned_targets.len(),
            input_arity: input_lane_count,
        }
        .into());
    }
    let stdout_targets = primary_targets
        .iter()
        .chain(&unassigned_targets)
        .filter(|target| matches!(target, OutputTarget::Stdout))
        .count();
    if stdout_targets > 1 {
        return Err(OutputTopologyError::MultipleStdout.into());
    }
    if config.stdout_gzip && stdout_targets == 0 {
        return Err(OutputTopologyError::StdoutGzipWithoutStdout.into());
    }

    let stdin_sources = input_lanes
        .iter()
        .flat_map(|lane| lane.shards.iter())
        .chain(interleaved_sources.iter().flatten())
        .filter(|source| matches!(source, InputSource::Stdin))
        .count();
    if stdin_sources > 1 {
        return Err(InputTopologyError::MultipleStdin.into());
    }
    if stdin_sources > 0 && config.accelerated_gzip_input {
        return Err(InputTopologyError::AcceleratedGzipStdin.into());
    }

    let resolved_resources = compiled_data.resolve_resources(
        &config.additional_args,
        &config.resource_bindings,
        config.geometry_base.as_deref(),
    )?;
    let mut graph = Graph::new();
    let input_layout = if interleaved_sources.is_some() {
        "interleaved"
    } else {
        "separate"
    };
    let input_topology = if let Some(sources) = &interleaved_sources {
        vec![sources
            .iter()
            .map(|source| source.kind().to_owned())
            .collect::<Vec<_>>()]
    } else {
        input_lanes
            .iter()
            .map(|lane| {
                lane.shards
                    .iter()
                    .map(|source| source.kind().to_owned())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>()
    };
    let grouped_files = input_lanes
        .iter()
        .map(|lane| {
            lane.shards
                .iter()
                .filter_map(|source| source.path())
                .map(|path| path.to_string_lossy().into_owned())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    if let Some(sources) = &interleaved_sources {
        if stdin_sources > 0 {
            if sources.len() != 1 {
                return Err(InputTopologyError::InterleavedStdinWithShards.into());
            }
            graph.add(
                InputFastqOp::from_interleaved_reader(io::stdin(), input_lane_count).map_err(
                    |source| SeqprocError::FastqInput {
                        context: "interleaved stdin",
                        source,
                    },
                )?,
            );
        } else {
            let files = sources
                .iter()
                .filter_map(InputSource::path)
                .map(|path| path.to_string_lossy().into_owned())
                .collect::<Vec<_>>();
            let input = if config.accelerated_gzip_input {
                GroupedInputFastqOp::from_interleaved_files_accelerated_gzip(
                    files,
                    input_lane_count,
                    config.gzip_input_threads,
                    config.gzip_input_chunk_size,
                )
            } else {
                GroupedInputFastqOp::from_interleaved_files(files, input_lane_count)
            };
            graph.add(input.map_err(|source| SeqprocError::FastqInput {
                context: "interleaved",
                source,
            })?);
        }
    } else if stdin_sources > 0 {
        if input_lanes.iter().any(|lane| lane.shards.len() != 1) {
            return Err(InputTopologyError::StdinWithShards.into());
        }
        let readers = input_lanes
            .iter()
            .map(|lane| match &lane.shards[0] {
                InputSource::Path(path) => File::open(path)
                    .map(|file| Box::new(file) as Box<dyn io::Read + Send>)
                    .map_err(|source| SeqprocError::Io {
                        operation: "open FASTQ input",
                        target: path.clone(),
                        source,
                    }),
                InputSource::Stdin => Ok(Box::new(io::stdin()) as Box<dyn io::Read + Send>),
            })
            .collect::<SeqprocResult<Vec<_>>>()?;
        graph.add(InputFastqOp::from_readers(readers).map_err(|source| {
            SeqprocError::FastqInput {
                context: "streamed",
                source,
            }
        })?);
    } else if grouped_files.iter().all(|lane| lane.len() == 1) {
        let files = grouped_files
            .iter()
            .map(|lane| lane[0].clone())
            .collect::<Vec<_>>();
        let input = if config.accelerated_gzip_input {
            InputFastqOp::from_files_accelerated_gzip(
                files,
                config.gzip_input_threads,
                config.gzip_input_chunk_size,
            )
        } else {
            InputFastqOp::from_files(files)
        };
        graph.add(input.map_err(|source| SeqprocError::FastqInput {
            context: "file-backed",
            source,
        })?);
    } else {
        let input = if config.accelerated_gzip_input {
            GroupedInputFastqOp::from_files_accelerated_gzip(
                grouped_files,
                config.gzip_input_threads,
                config.gzip_input_chunk_size,
            )
        } else {
            GroupedInputFastqOp::from_files(grouped_files)
        };
        graph.add(input.map_err(|source| SeqprocError::FastqInput {
            context: "grouped",
            source,
        })?);
    }

    let has_unassigned = !unassigned_targets.is_empty();
    if has_unassigned {
        let mut try_graph = Graph::new();
        compiled_data.interpret_with_resources(&mut try_graph, &resolved_resources)?;

        let mut catch_graph = Graph::new();
        add_fastq_targets(&mut catch_graph, &unassigned_targets, &config, gzip_threads)?;
        graph.add(TryOp::new(try_graph, catch_graph));
    } else {
        compiled_data.interpret_with_resources(&mut graph, &resolved_resources)?;
    }

    if let Some(demux) = &config.demux {
        demux.add_lookup_op(&mut graph)?;
        std::fs::create_dir_all(&demux.output_dir).map_err(|source| SeqprocError::Io {
            operation: "create demultiplexing output directory",
            target: demux.output_dir.clone(),
            source,
        })?;

        let out_dir = demux.output_dir.to_string_lossy();
        let sample_attr_path = format!("{}.{}", demux.barcode_label, demux.sample_attr);
        let expressions = (1..=output_arity)
            .map(|lane| {
                fmt_expr(format!(
                    "{}/{{{}}}_R{}.fastq",
                    out_dir, sample_attr_path, lane
                ))
            })
            .collect::<Vec<_>>();
        graph.add(configure_fastq_output(
            OutputFastqFileOp::from_files(expressions),
            &config,
            effective_gzip_threads,
        )?);
    } else {
        add_fastq_targets(&mut graph, &primary_targets, &config, gzip_threads)?;
    }

    // Geometry compilation currently relies on the historical conditional
    // skip behavior. Make that compatibility choice explicit before freezing
    // the graph; future EFGDL validation can select stricter policies.
    graph.set_missing_input_policy(MissingInputPolicy::Skip);
    let statistics_level = config.effective_statistics_level();
    graph.set_statistics_level(statistics_level);
    let graph = graph
        .compile_with(GraphOptimizationConfig {
            enabled: config.graph_optimization,
        })
        .map_err(|source| SeqprocError::GraphCompilation { source })?;
    let optimization = graph.optimization_report().clone();
    let mut execution_request = ExecutionRequest::new(config.threads);
    execution_request.mode =
        if config.staged_pipeline && config.execution_mode == ExecutionMode::Auto {
            ExecutionMode::Pipeline
        } else {
            config.execution_mode
        };
    execution_request.pipeline.preserve_order = config.preserve_order;
    execution_request.pipeline.direct_output_rendering = config.direct_output_rendering;
    execution_request.pipeline.input_mode = config.pipeline_input_mode;
    if let Some(queue_capacity) = config.queue_capacity {
        execution_request.pipeline.queue_capacity = queue_capacity;
    }
    if let Some(max_in_flight_batches) = config.max_in_flight_batches {
        execution_request.pipeline.max_in_flight_batches = max_in_flight_batches;
    }
    if let Some(batch_size) = config.batch_size {
        execution_request.pipeline.batch_size = batch_size;
    }
    let planned = graph.try_run_planned(execution_request).map_err(|source| {
        if is_fastq_input_error(&source) {
            return SeqprocError::FastqInput {
                context: "runtime",
                source,
            };
        }
        if matches!(
            &source,
            antisequence::errors::Error::InvalidPipelineConfig(_)
                | antisequence::errors::Error::InvalidPipelineGraph(_)
        ) {
            return SeqprocError::ExecutionPlanning { source };
        }
        if is_broken_pipe_error(&source) {
            return SeqprocError::BrokenPipe {
                operation: "FASTQ output",
            };
        }
        SeqprocError::GraphExecution { source }
    })?;
    let execution_plan = planned.plan;
    let pipeline = planned.pipeline;

    let statistics = statistics_level.is_enabled().then(|| {
        statistics_from_graph(
            &graph,
            statistics_level,
            config.call.clone(),
            config.geometry_digest.clone(),
            RuntimeProvenance {
                config: &config,
                gzip_compression_threads: effective_gzip_threads,
                optimization: &optimization,
                execution_plan: &execution_plan,
                resources: resolved_resources.report(),
                input_topology: &input_topology,
                input_layout,
                input_arity: input_lane_count,
                output_arity,
                output_topology: &primary_targets,
            },
        )
    });
    Ok(RunReport {
        effective_threads: config.threads,
        ordered_output: config.preserve_order || config.threads == 1,
        statistics_level,
        gzip_compression_level: config.gzip_level,
        parallel_gzip_members: config.parallel_gzip,
        parallel_gzip_stream: config.parallel_gzip_stream,
        gzip_compression_threads: effective_gzip_threads,
        gzip_block_size: config.gzip_block_size,
        gzip_input_backend: if config.accelerated_gzip_input {
            "rapidgzip-core".to_owned()
        } else {
            "needletail-auto".to_owned()
        },
        gzip_input_threads: if config.accelerated_gzip_input {
            config.gzip_input_threads
        } else {
            1
        },
        gzip_input_chunk_size: if config.accelerated_gzip_input {
            config.gzip_input_chunk_size
        } else {
            0
        },
        graph_optimization: optimization,
        execution_plan,
        resources: resolved_resources.report().clone(),
        input_topology,
        input_layout: input_layout.to_owned(),
        input_arity: input_lane_count,
        output_arity,
        output_topology: primary_targets
            .iter()
            .map(|target| target.kind().to_owned())
            .collect(),
        pipeline,
        statistics,
    })
}

struct RuntimeProvenance<'a> {
    config: &'a RunConfig,
    gzip_compression_threads: usize,
    optimization: &'a GraphOptimizationReport,
    execution_plan: &'a ExecutionPlan,
    resources: &'a ResourceResolutionReport,
    input_topology: &'a [Vec<String>],
    input_layout: &'a str,
    input_arity: usize,
    output_arity: usize,
    output_topology: &'a [OutputTarget],
}

fn statistics_from_graph(
    graph: &Graph,
    statistics_level: StatisticsLevel,
    call: Option<String>,
    geometry_digest: Option<String>,
    provenance: RuntimeProvenance<'_>,
) -> SeqprocStats {
    let RuntimeProvenance {
        config,
        gzip_compression_threads,
        optimization,
        execution_plan,
        resources,
        input_topology,
        input_layout,
        input_arity,
        output_arity,
        output_topology,
    } = provenance;
    let input_stats = graph.input_stats();
    let (
        n_fastqs,
        n_processed,
        n_reads_max,
        read_length_min,
        read_length_max,
        read_length_mean,
        shard_read_counts,
    ) = if let Some(stats) = input_stats {
        let mut read_length_min = Vec::with_capacity(stats.n_fastqs);
        let mut read_length_max = Vec::with_capacity(stats.n_fastqs);
        let mut read_length_mean = Vec::with_capacity(stats.n_fastqs);
        let mut max_count = 0usize;

        for index in 0..stats.n_fastqs {
            let count = *stats.read_counts.get(index).unwrap_or(&0);
            max_count = max_count.max(count);
            if stats.lengths_collected {
                let min = *stats.read_length_min.get(index).unwrap_or(&0);
                let max = *stats.read_length_max.get(index).unwrap_or(&0);
                let sum = *stats.read_length_sum.get(index).unwrap_or(&0);
                read_length_min.push(min as u64);
                read_length_max.push(max as u64);
                read_length_mean.push(if count == 0 {
                    0.0
                } else {
                    sum as f64 / count as f64
                });
            }
        }

        let shard_read_counts = if stats.shard_read_counts.is_empty() {
            stats
                .read_counts
                .iter()
                .map(|count| vec![*count as u64])
                .collect()
        } else {
            stats
                .shard_read_counts
                .iter()
                .map(|lane| lane.iter().map(|count| *count as u64).collect())
                .collect()
        };
        (
            stats.n_fastqs as u32,
            max_count as u64,
            max_count as u64,
            read_length_min,
            read_length_max,
            read_length_mean,
            shard_read_counts,
        )
    } else {
        (0, 0, 0, Vec::new(), Vec::new(), Vec::new(), Vec::new())
    };

    let match_distance_stats = graph
        .match_distance_counts()
        .into_iter()
        .enumerate()
        .map(|(stage_index, counts)| {
            let matched_total = counts.counts.iter().map(|&count| count as u64).sum::<u64>();
            let distance_histogram = counts
                .counts
                .into_iter()
                .enumerate()
                .filter(|(_, count)| *count > 0)
                .map(|(distance, count)| DistanceBin {
                    distance,
                    count: count as u64,
                })
                .collect();
            MatchDistanceStats {
                stage_index,
                label: counts.label,
                attempted: counts.total as u64,
                matched: matched_total,
                unmatched: (counts.total as u64).saturating_sub(matched_total),
                distance_histogram,
                ambiguity: AmbiguityStats {
                    total: counts.ambiguity.total as u64,
                    accepted: counts.ambiguity.accepted as u64,
                    dropped: counts.ambiguity.dropped as u64,
                    resolved_first: counts.ambiguity.resolved_first as u64,
                    resolved_random: counts.ambiguity.resolved_random as u64,
                    resolved_quality: counts.ambiguity.resolved_quality as u64,
                    position_total: counts.ambiguity.position_total as u64,
                    position_dropped: counts.ambiguity.position_dropped as u64,
                    position_resolved_leftmost: counts.ambiguity.position_resolved_leftmost as u64,
                    position_resolved_rightmost: counts.ambiguity.position_resolved_rightmost
                        as u64,
                    position_resolved_quality: counts.ambiguity.position_resolved_quality as u64,
                },
            }
        })
        .collect();
    let failed_parsing = graph.failed_reads() as u64;
    let accepted_fragments = graph
        .final_output_reads()
        .map(|count| count as u64)
        .unwrap_or_else(|| n_processed.saturating_sub(failed_parsing));
    let rejected_fragments = n_processed.saturating_sub(accepted_fragments);
    let mut rejection_reasons = Vec::new();
    if failed_parsing > 0 {
        rejection_reasons.push(RejectionReasonCount {
            reason: "missing_required_label_or_attribute".to_owned(),
            count: failed_parsing,
        });
    }
    let other_rejected = rejected_fragments.saturating_sub(failed_parsing);
    if other_rejected > 0 {
        rejection_reasons.push(RejectionReasonCount {
            reason: "not_emitted_by_primary_output".to_owned(),
            count: other_rejected,
        });
    }

    SeqprocStats {
        schema_version: "1.11.0".to_owned(),
        seqproc_version: env!("CARGO_PKG_VERSION").to_owned(),
        statistics_level,
        call,
        geometry_digest,
        ordering_mode: if config.preserve_order || config.threads == 1 {
            "input-order".to_owned()
        } else {
            "unordered".to_owned()
        },
        effective_threads: config.threads,
        gzip_compression_level: config.gzip_level,
        parallel_gzip_members: config.parallel_gzip,
        parallel_gzip_stream: config.parallel_gzip_stream,
        gzip_compression_threads,
        gzip_block_size: config.gzip_block_size,
        gzip_input_backend: if config.accelerated_gzip_input {
            "rapidgzip-core".to_owned()
        } else {
            "needletail-auto".to_owned()
        },
        gzip_input_threads: if config.accelerated_gzip_input {
            config.gzip_input_threads
        } else {
            1
        },
        gzip_input_chunk_size: if config.accelerated_gzip_input {
            config.gzip_input_chunk_size
        } else {
            0
        },
        graph_optimization: Some(optimization.clone()),
        execution_plan: Some(execution_plan.clone()),
        resources: resources.clone(),
        input_topology: input_topology.to_vec(),
        input_layout: input_layout.to_owned(),
        input_arity,
        output_arity,
        output_topology: output_topology
            .iter()
            .map(|target| target.kind().to_owned())
            .collect(),
        n_fastqs,
        n_processed,
        n_reads_max,
        total_fragments: n_processed,
        accepted_fragments,
        rejected_fragments,
        failed_parsing,
        rejection_reasons,
        read_length_mean,
        read_length_min,
        read_length_max,
        shard_read_counts,
        match_distance_stats,
    }
}

#[deprecated(note = "use run(RunConfig, CompiledData) for structured errors")]
#[allow(deprecated)]
pub fn interpret(
    file1: &Path,
    file2: Option<&Path>,
    out1: &Path,
    out2: &Path,
    threads: usize,
    additional_args: Vec<&str>,
    compiled_data: CompiledData,
) {
    interpret_with_unassigned(
        file1,
        file2,
        out1,
        out2,
        None,
        None,
        threads,
        additional_args,
        compiled_data,
        None,
    );
}

/// Interpret geometry with optional unassigned output and demultiplexing support.
#[allow(clippy::too_many_arguments)]
#[deprecated(note = "use run(RunConfig, CompiledData) for structured errors")]
pub fn interpret_with_unassigned(
    file1: &Path,
    file2: Option<&Path>,
    out1: &Path,
    out2: &Path,
    unassigned1: Option<&Path>,
    unassigned2: Option<&Path>,
    threads: usize,
    additional_args: Vec<&str>,
    compiled_data: CompiledData,
    demux_config: Option<DemuxConfig>,
) {
    let additional_args = additional_args.into_iter().collect::<Vec<_>>();

    // Skip output check when demux is enabled (demux handles its own output routing)
    if demux_config.is_none() {
        if let Some(transformations) = &compiled_data.transformation {
            if transformations.len() == 2
                && (out1.as_os_str().is_empty() || out2.as_os_str().is_empty())
            {
                tracing::error!(
                    "You defined a transformation into two files - you must provide two outputs"
                );
                return;
            }
        }
    }

    // Build main processing graph
    let mut main_graph = antisequence::graph::Graph::new();
    if let Err(error) = compiled_data.try_interpret(&mut main_graph, &additional_args) {
        tracing::error!("Failed to resolve geometry resources: {error}");
        return;
    }

    // If unassigned output is requested, wrap in TryOp
    let has_unassigned = unassigned1.is_some() || unassigned2.is_some();

    let mut graph = antisequence::graph::Graph::new();
    let file1_str = file1.to_str().unwrap_or("");

    let mut input_files = vec![file1_str];
    if let Some(f2) = file2 {
        input_files.push(f2.to_str().unwrap_or(""));
    }

    let input = match antisequence::graph::InputFastqOp::from_files(input_files) {
        Ok(input) => input,
        Err(error) => {
            tracing::error!("Failed to configure FASTQ input: {error}");
            return;
        }
    };
    graph.add(input);

    if has_unassigned {
        // Build catch graph for unassigned reads
        let mut catch_graph = antisequence::graph::Graph::new();
        let unassigned1_str = unassigned1
            .map(|p| p.to_str().unwrap_or(""))
            .unwrap_or("/dev/null");
        let unassigned2_str = unassigned2
            .map(|p| p.to_str().unwrap_or(""))
            .unwrap_or("/dev/null");

        let mut unassigned_files = vec![unassigned1_str.to_owned()];
        if file2.is_some() && !unassigned2_str.is_empty() && unassigned2_str != "/dev/null" {
            unassigned_files.push(unassigned2_str.to_owned());
        }

        if !unassigned1_str.is_empty() && unassigned1_str != "/dev/null" {
            catch_graph.add(OutputFastqFileOp::from_files(unassigned_files));
        }

        // Use TryOp to route failed reads to catch graph
        graph.add(TryOp::new(main_graph, catch_graph));
    } else {
        // No unassigned output - just add main graph nodes
        if let Err(error) = compiled_data.try_interpret(&mut graph, &additional_args) {
            tracing::error!("Failed to resolve geometry resources: {error}");
            return;
        }
    }

    // Add LookupOp for demultiplexing if configured
    if let Some(ref config) = demux_config {
        if let Err(e) = config.add_lookup_op(&mut graph) {
            tracing::error!("Failed to add demux LookupOp: {}", e);
            return;
        }
        tracing::info!(
            "Demultiplexing enabled with label: {}",
            config.barcode_label
        );
    }

    let out1_str = out1.to_str().unwrap_or("");
    let out2_str = out2.to_str().unwrap_or("");

    // When demux is enabled, use expression-based output routing
    if let Some(ref config) = demux_config {
        // Create output directory if it doesn't exist
        if let Err(e) = std::fs::create_dir_all(&config.output_dir) {
            tracing::error!("Failed to create demux output directory: {}", e);
            return;
        }

        let out_dir = config.output_dir.to_string_lossy();
        let sample_attr_path = format!("{}.{}", config.barcode_label, config.sample_attr);

        let out1_expr = format!("{}/{{{}}}_R1.fastq", out_dir, sample_attr_path);
        let mut out_exprs = vec![fmt_expr(out1_expr.clone())];

        if file2.is_some() {
            let out2_expr = format!("{}/{{{}}}_R2.fastq", out_dir, sample_attr_path);
            tracing::info!("Demux output: {} and {}", out1_expr, out2_expr);
            out_exprs.push(fmt_expr(out2_expr));
        } else {
            tracing::info!("Demux output: {}", out1_expr);
        }

        graph.add(OutputFastqFileOp::from_files(out_exprs));
    } else {
        // Standard output (no demux)
        match (out1_str, out2_str) {
            ("", "") => {
                graph.add(OutputFastqFileOp::from_file("/dev/null"));
            }
            (out1_str, "") => {
                graph.add(OutputFastqFileOp::from_file(out1_str.to_owned()));
            }
            (out1_str, out2_str) => {
                graph.add(OutputFastqFileOp::from_files([
                    out1_str.to_owned(),
                    out2_str.to_owned(),
                ]));
            }
        }
    }

    graph.set_missing_input_policy(MissingInputPolicy::Skip);
    let graph = match graph.compile() {
        Ok(graph) => graph,
        Err(error) => {
            tracing::error!("Failed to compile processing graph: {}", error);
            return;
        }
    };
    graph.run_with_threads(threads);
}

/// Interpret geometry with optional demultiplexing support.
#[allow(clippy::too_many_arguments)]
#[deprecated(note = "use run(RunConfig, CompiledData) for structured errors")]
pub fn interpret_with_demux(
    file1: &Path,
    file2: Option<&Path>,
    out1: &Path,
    out2: &Path,
    threads: usize,
    additional_args: Vec<&str>,
    compiled_data: CompiledData,
    demux_config: Option<DemuxConfig>,
) {
    let additional_args = additional_args.into_iter().collect::<Vec<_>>();

    // Skip output check when demux is enabled (demux handles its own output routing)
    if demux_config.is_none() {
        if let Some(transformations) = &compiled_data.transformation {
            if transformations.len() == 2
                && (out1.as_os_str().is_empty() || out2.as_os_str().is_empty())
            {
                tracing::error!(
                    "You defined a transformation into two files - you must provide two outputs"
                );
                return;
            }
        }
    }

    let mut graph = antisequence::graph::Graph::new();
    let file1_str = file1.to_str().unwrap_or("");

    let mut input_files = vec![file1_str];
    if let Some(f2) = file2 {
        input_files.push(f2.to_str().unwrap_or(""));
    }

    let input = match antisequence::graph::InputFastqOp::from_files(input_files) {
        Ok(input) => input,
        Err(error) => {
            tracing::error!("Failed to configure FASTQ input: {error}");
            return;
        }
    };
    graph.add(input);

    if let Err(error) = compiled_data.try_interpret(&mut graph, &additional_args) {
        tracing::error!("Failed to resolve geometry resources: {error}");
        return;
    }

    // Add LookupOp for demultiplexing if configured
    if let Some(ref config) = demux_config {
        if let Err(e) = config.add_lookup_op(&mut graph) {
            tracing::error!("Failed to add demux LookupOp: {}", e);
            return;
        }
        tracing::info!(
            "Demultiplexing enabled with label: {}",
            config.barcode_label
        );
    }

    let out1_str = out1.to_str().unwrap_or("");
    let out2_str = out2.to_str().unwrap_or("");

    // When demux is enabled, use expression-based output routing
    if let Some(ref config) = demux_config {
        // Create output directory if it doesn't exist
        if let Err(e) = std::fs::create_dir_all(&config.output_dir) {
            tracing::error!("Failed to create demux output directory: {}", e);
            return;
        }

        // Build format expressions for dynamic file routing based on sample attribute
        // Format: {output_dir}/{sample}_R1.fastq and {output_dir}/{sample}_R2.fastq
        let out_dir = config.output_dir.to_string_lossy();

        // The sample attribute is set on the barcode label (e.g., seq2.bc1.sample)
        let sample_attr_path = format!("{}.{}", config.barcode_label, config.sample_attr);

        let out1_expr = format!("{}/{{{}}}_R1.fastq", out_dir, sample_attr_path);
        let mut out_exprs = vec![fmt_expr(out1_expr.clone())];

        if file2.is_some() {
            let out2_expr = format!("{}/{{{}}}_R2.fastq", out_dir, sample_attr_path);
            tracing::info!("Demux output: {} and {}", out1_expr, out2_expr);
            out_exprs.push(fmt_expr(out2_expr));
        } else {
            tracing::info!("Demux output: {}", out1_expr);
        }

        graph.add(OutputFastqFileOp::from_files(out_exprs));
    } else {
        // Standard output (no demux)
        match (out1_str, out2_str) {
            ("", "") => {
                graph.add(OutputFastqFileOp::from_file("/dev/null"));
            }
            (out1_str, "") => {
                graph.add(OutputFastqFileOp::from_file(out1_str.to_owned()));
            }
            (out1_str, out2_str) => {
                graph.add(OutputFastqFileOp::from_files([
                    out1_str.to_owned(),
                    out2_str.to_owned(),
                ]));
            }
        }
    }

    graph.set_missing_input_policy(MissingInputPolicy::Skip);
    let graph = match graph.compile() {
        Ok(graph) => graph,
        Err(error) => {
            tracing::error!("Failed to compile processing graph: {}", error);
            return;
        }
    };
    graph.run_with_threads(threads);
}

fn interpret_to_pipes(
    files1: Vec<String>,
    files2: Vec<String>,
    out1: PathBuf,
    out2: PathBuf,
    threads: usize,
    additional_args: Vec<&str>,
    compiled_data: CompiledData,
) -> AnyResult<SeqprocStats> {
    let f1 = File::create(out1)?;

    // Handle second output stream optionally if files2 is present?
    // But this function return signature doesn't change easily.
    // And it is used by read_pairs_to_fifo which has r1_fifo and r2_fifo.
    // We should probably keep assuming paired output if called via this path,
    // OR we assume that if files2 is empty, we don't write to out2?
    // But out2 is passed as PathBuf.
    // Let's create f2 only if needed?
    // But InputFastqOp needs readers.

    let mut readers = files1
        .iter()
        .map(File::open)
        .collect::<std::io::Result<Vec<_>>>()?;

    for f in &files2 {
        readers.push(File::open(f)?);
    }

    let additional_args = additional_args.into_iter().collect::<Vec<_>>();

    let mut graph = antisequence::graph::Graph::new();
    graph.add(
        antisequence::graph::InputFastqOp::from_readers(readers)
            .map_err(|error| anyhow!(error.to_string()))?,
    );

    compiled_data.try_interpret(&mut graph, &additional_args)?;

    let stream1 = BufWriter::new(f1);

    if !files2.is_empty() {
        let f2 = File::create(out2)?;
        let stream2 = BufWriter::new(f2);
        graph.add(OutputFastqOp::from_writers([stream1, stream2]));
    } else {
        graph.add(OutputFastqOp::from_writer(stream1));
    }

    // This is the reporting path. Normal execution leaves statistics disabled
    // in antisequence so it avoids per-read counters and histogram locks.
    graph.set_missing_input_policy(MissingInputPolicy::Skip);
    graph.set_statistics_level(StatisticsLevel::Detailed);
    let graph = graph
        .compile()
        .map_err(|error| anyhow!(error.to_string()))?;
    graph
        .try_run_with_threads(threads)
        .map_err(|error| anyhow!(error.to_string()))?;

    let input_stats = graph.input_stats();

    let (
        n_fastqs,
        n_processed,
        n_reads_max,
        read_length_min,
        read_length_max,
        read_length_mean,
        shard_read_counts,
    ) = if let Some(s) = input_stats {
        let n_fastqs = s.n_fastqs as u32;

        let mut read_length_min_u64 = Vec::with_capacity(s.n_fastqs);
        let mut read_length_max_u64 = Vec::with_capacity(s.n_fastqs);
        let mut read_length_mean_f64 = Vec::with_capacity(s.n_fastqs);

        let mut max_count = 0usize;

        for i in 0..s.n_fastqs {
            let count = *s.read_counts.get(i).unwrap_or(&0);
            if count > max_count {
                max_count = count;
            }

            let min = *s.read_length_min.get(i).unwrap_or(&0);
            let max = *s.read_length_max.get(i).unwrap_or(&0);
            let sum = *s.read_length_sum.get(i).unwrap_or(&0);

            read_length_min_u64.push(min as u64);
            read_length_max_u64.push(max as u64);

            let mean = if count > 0 {
                sum as f64 / (count as f64)
            } else {
                0.0
            };
            read_length_mean_f64.push(mean);
        }

        let n_processed = max_count as u64;
        let n_reads_max = n_processed;
        let shard_read_counts = if s.shard_read_counts.is_empty() {
            s.read_counts
                .iter()
                .map(|count| vec![*count as u64])
                .collect()
        } else {
            s.shard_read_counts
                .iter()
                .map(|lane| lane.iter().map(|count| *count as u64).collect())
                .collect()
        };

        (
            n_fastqs,
            n_processed,
            n_reads_max,
            read_length_min_u64,
            read_length_max_u64,
            read_length_mean_f64,
            shard_read_counts,
        )
    } else {
        (0, 0, 0, Vec::new(), Vec::new(), Vec::new(), Vec::new())
    };

    let match_distance_stats = graph
        .match_distance_counts()
        .into_iter()
        .enumerate()
        .map(|(stage_index, c)| {
            let matched_total: u64 = c.counts.iter().map(|&x| x as u64).sum();
            let unmatched = (c.total as u64).saturating_sub(matched_total);

            let distance_histogram = c
                .counts
                .into_iter()
                .enumerate()
                .filter_map(|(distance, count)| {
                    if count == 0 {
                        None
                    } else {
                        Some(DistanceBin {
                            distance,
                            count: count as u64,
                        })
                    }
                })
                .collect();

            MatchDistanceStats {
                stage_index,
                label: c.label,
                attempted: c.total as u64,
                matched: matched_total,
                unmatched,
                distance_histogram,
                ambiguity: AmbiguityStats {
                    total: c.ambiguity.total as u64,
                    accepted: c.ambiguity.accepted as u64,
                    dropped: c.ambiguity.dropped as u64,
                    resolved_first: c.ambiguity.resolved_first as u64,
                    resolved_random: c.ambiguity.resolved_random as u64,
                    resolved_quality: c.ambiguity.resolved_quality as u64,
                    position_total: c.ambiguity.position_total as u64,
                    position_dropped: c.ambiguity.position_dropped as u64,
                    position_resolved_leftmost: c.ambiguity.position_resolved_leftmost as u64,
                    position_resolved_rightmost: c.ambiguity.position_resolved_rightmost as u64,
                    position_resolved_quality: c.ambiguity.position_resolved_quality as u64,
                },
            }
        })
        .collect();

    let failed_parsing = graph.failed_reads() as u64;
    let accepted_fragments = graph
        .final_output_reads()
        .map(|count| count as u64)
        .unwrap_or_else(|| n_processed.saturating_sub(failed_parsing));
    let rejected_fragments = n_processed.saturating_sub(accepted_fragments);
    let mut rejection_reasons = Vec::new();
    if failed_parsing > 0 {
        rejection_reasons.push(RejectionReasonCount {
            reason: "missing_required_label_or_attribute".to_owned(),
            count: failed_parsing,
        });
    }
    let other_rejected = rejected_fragments.saturating_sub(failed_parsing);
    if other_rejected > 0 {
        rejection_reasons.push(RejectionReasonCount {
            reason: "not_emitted_by_primary_output".to_owned(),
            count: other_rejected,
        });
    }

    Ok(SeqprocStats {
        schema_version: "1.11.0".to_string(),
        seqproc_version: env!("CARGO_PKG_VERSION").to_string(),
        statistics_level: StatisticsLevel::Detailed,
        call: None,
        geometry_digest: None,
        ordering_mode: if threads == 1 {
            "input-order".to_string()
        } else {
            "unordered".to_string()
        },
        effective_threads: threads,
        gzip_compression_level: OutputFastqFileOp::DEFAULT_GZIP_LEVEL,
        parallel_gzip_members: false,
        parallel_gzip_stream: false,
        gzip_compression_threads: 1,
        gzip_block_size: 128 * 1024,
        gzip_input_backend: "needletail-auto".to_owned(),
        gzip_input_threads: 1,
        gzip_input_chunk_size: 0,
        graph_optimization: None,
        execution_plan: None,
        resources: ResourceResolutionReport::default(),
        input_topology: vec![vec!["path".to_owned()]; n_fastqs as usize],
        input_layout: "separate".to_owned(),
        input_arity: n_fastqs as usize,
        output_arity: n_fastqs as usize,
        output_topology: vec!["path".to_owned(); n_fastqs as usize],

        n_fastqs,
        n_processed,
        n_reads_max,

        total_fragments: n_processed,
        accepted_fragments,
        rejected_fragments,
        failed_parsing,
        rejection_reasons,
        read_length_mean,
        read_length_min,
        read_length_max,
        shard_read_counts,
        match_distance_stats,
    })
}

fn diagnostic_from_rich(error: Rich<'_, impl std::fmt::Display>) -> GeometryDiagnostic {
    GeometryDiagnostic {
        message: error.to_string(),
        reason: error.reason().to_string(),
        span: error.span().into_range(),
        contexts: error
            .contexts()
            .map(|(label, span)| (format!("while parsing this {label}"), span.into_range()))
            .collect(),
    }
}

/// Parse and semantically compile EFGDL with structured, stage-specific
/// diagnostics. This is the primary library compilation API.
pub fn compile_geom_typed(geom: impl AsRef<str>) -> SeqprocResult<CompiledData> {
    let geom = geom.as_ref();
    let tokens =
        lexer::lexer()
            .parse(geom)
            .into_result()
            .map_err(|errors| SeqprocError::Geometry {
                stage: GeometryStage::Lexing,
                diagnostics: errors.into_iter().map(diagnostic_from_rich).collect(),
            })?;

    let tokens = tokens
        .into_iter()
        .map(|(tok, span)| chumsky::span::Spanned { inner: tok, span })
        .collect::<Vec<_>>();
    let input = tokens[..].split_spanned((0..geom.len()).into());

    let description =
        parser()
            .parse(input)
            .into_result()
            .map_err(|errors| SeqprocError::Geometry {
                stage: GeometryStage::Parsing,
                diagnostics: errors.into_iter().map(diagnostic_from_rich).collect(),
            })?;

    compile(description).map_err(|error| SeqprocError::Geometry {
        stage: GeometryStage::SemanticCompilation,
        diagnostics: vec![GeometryDiagnostic {
            message: error.msg.clone(),
            reason: error.msg,
            span: error.span.into_range(),
            contexts: Vec::new(),
        }],
    })
}

/// Compatibility API returning Chumsky diagnostics. New callers should use
/// [`compile_geom_typed`].
pub fn compile_geom(geom: String) -> std::result::Result<CompiledData, Vec<Rich<'static, String>>> {
    let compiled = compile_geom_typed(&geom).map_err(|error| {
        let Some((_, diagnostics)) = error.geometry_diagnostics() else {
            unreachable!("geometry compilation only returns geometry diagnostics")
        };
        diagnostics
            .iter()
            .map(|diagnostic| {
                Rich::<String>::custom(diagnostic.span.clone().into(), diagnostic.message.clone())
                    .into_owned()
            })
            .collect::<Vec<_>>()
    })?;

    // LANG-DEPRECATE: Preserve compatibility diagnostics on the old API.
    for warning in &compiled.warnings {
        eprintln!("Warning: {}", warning);
    }

    Ok(compiled)
}

pub fn read_pairs_to_file(
    compiled_data: CompiledData,
    in1: &Path,
    in2: Option<&Path>,
    out1: &Path,
    out2: &Path,
    threads: usize,
    additional_args: Vec<&str>,
) -> AnyResult<SeqprocStats> {
    let files1 = vec![in1.to_str().unwrap_or("").to_owned()];
    let files2 = if let Some(i2) = in2 {
        vec![i2.to_str().unwrap_or("").to_owned()]
    } else {
        vec![]
    };

    let stats = interpret_to_pipes(
        files1,
        files2,
        out1.to_path_buf(),
        out2.to_path_buf(),
        threads,
        additional_args,
        compiled_data,
    )?;

    Ok(stats)
}

pub fn read_pairs_to_fifo<'a: 'static>(
    compiled_data: CompiledData,
    r1: Vec<String>,
    r2: Vec<String>,
    additional_args: Vec<&'a str>,
) -> AnyResult<FifoSeqprocData> {
    if !r2.is_empty() && r1.len() != r2.len() {
        bail!(
            "The number of R1 files ({}) must match the number of R2 files ({})",
            r1.len(),
            r2.len()
        );
    }

    let tmp_dir = tempdir()?;
    let r1_fifo = tmp_dir.path().join("r1.pipe");
    let r2_fifo = tmp_dir.path().join("r2.pipe");

    // create the fifos
    // create new fifo and give read, write and execute rights to the owner
    match unistd::mkfifo(&r1_fifo, stat::Mode::S_IRWXU) {
        Ok(_) => {
            info!("created {:?}", r1_fifo);
            if !std::path::Path::new(&r1_fifo).exists() {
                bail!("read 1 fifo was not created at {:?}", r1_fifo);
            }
        }
        Err(err) => bail!("Error creating read 1 fifo: {}", err),
    }
    // create new fifo and give read, write and execute rights to the owner
    match unistd::mkfifo(&r2_fifo, stat::Mode::S_IRWXU) {
        Ok(_) => {
            info!("created {:?}", r2_fifo);
            if !std::path::Path::new(&r2_fifo).exists() {
                bail!("read 2 fifo was not created at {:?}", r2_fifo);
            }
        }
        Err(err) => bail!("Error creating read 2 fifo: {}", err),
    }

    // we clone this here because we want to move these into
    // the thread that will do the transformation but we need
    // to retain a copy to pass to the FifoXFormData that we
    // will return.
    let r1_fifo_clone = r1_fifo.clone();
    let r2_fifo_clone = r2_fifo.clone();

    let join_handle: thread::JoinHandle<AnyResult<SeqprocStats>> = thread::spawn(move || {
        let seqproc_stats = interpret_to_pipes(
            r1,
            r2,
            r1_fifo_clone,
            r2_fifo_clone,
            6, // default to 6 threads
            additional_args,
            compiled_data,
        )?;

        // Explicitly check for and propagate any errors encountered in the
        // closing and deleting of the temporary directory.  The directory
        // will be deleted when the handle goes out of scope, but without
        // calling this method, any encountered errors will be silently
        // ignored.
        // see: https://docs.rs/tempfile/latest/tempfile/struct.TempDir.html#method.close
        match tmp_dir.close() {
            Ok(_) => Ok(seqproc_stats),
            Err(e) => {
                bail!("When closing (deleting) the temp directory, the following error was encountered {:?}", e);
            }
        }
    });

    Ok(FifoSeqprocData {
        r1_fifo,
        r2_fifo,
        join_handle,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_geom_simple_barcode_read() {
        let result = compile_geom("1{b[16]u[10]r:}".to_string());
        assert!(result.is_ok());
        let data = result.unwrap();
        assert_eq!(data.geometry.len(), 1);
        assert_eq!(data.geometry[0].len(), 3);
        assert!(data.transformation.is_none());
    }

    #[test]
    fn test_compile_geom_returns_lex_and_parse_errors() {
        assert!(compile_geom("1{b[16]r:@}".to_string()).is_err());
        assert!(compile_geom("1{b[16]".to_string()).is_err());
    }

    #[test]
    fn test_compile_geom_two_reads() {
        let result = compile_geom("1{b[16]u[10]r:}2{r:}".to_string());
        assert!(result.is_ok());
        let data = result.unwrap();
        assert_eq!(data.geometry.len(), 2);
    }

    #[test]
    fn test_compile_geom_discard() {
        let result = compile_geom("1{x[5]b[16]r:}".to_string());
        assert!(result.is_ok());
        let data = result.unwrap();
        assert_eq!(data.geometry[0].len(), 3);
    }

    #[test]
    fn test_compile_geom_fixed_seq() {
        let result = compile_geom("1{f[ACGT]r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_multiple_barcodes() {
        let result = compile_geom("1{b[16]b[8]r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_with_rev() {
        let result = compile_geom("1{rev(b[16])r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_with_revcomp() {
        let result = compile_geom("1{revcomp(b[16])r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_with_trunc() {
        let result = compile_geom("1{trunc(b[16], 2)r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_with_trunc_to() {
        let result = compile_geom("1{trunc_to(b[16], 10)r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_with_remove() {
        let result = compile_geom("1{remove(f[ACGT])r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_with_pad() {
        let result = compile_geom("1{pad(b[16], 4, A)r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_with_pad_to() {
        let result = compile_geom("1{pad_to(b[16], 20, A)r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_with_hamming() {
        let result = compile_geom("1{hamming(f[ACGT], 1)r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_with_edit() {
        let result = compile_geom("1{edit(f[ACGT], 1)r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_with_remove_hamming() {
        let result = compile_geom("1{remove(hamming(f[CAG], 1))r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_with_labels() {
        let result = compile_geom("1{b<bc1>[16]u<umi>[10]r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_complex_two_read() {
        let result = compile_geom("1{b[16]u[12]r:}2{x[10]b[8]r:}".to_string());
        assert!(result.is_ok());
        let data = result.unwrap();
        assert_eq!(data.geometry.len(), 2);
        assert_eq!(data.geometry[0].len(), 3);
        assert_eq!(data.geometry[1].len(), 3);
    }

    #[test]
    fn test_compile_geom_with_transformation() {
        let result = compile_geom(
            "1{b<bc>[16]u<umi>[10]r<read>:}2{r<read2>:}->1{<bc><umi>}2{<read2>}".to_string(),
        );
        assert!(result.is_ok());
        let data = result.unwrap();
        assert!(data.transformation.is_some());
    }

    #[test]
    fn test_compile_geom_is_complex() {
        let data = compile_geom("1{b[16]r[100]}".to_string()).unwrap();
        assert!(!data.is_complex_geometry());

        // FixedSeq makes geometry complex
        let data = compile_geom("1{f[ACGT]r[100]}".to_string()).unwrap();
        assert!(data.is_complex_geometry());
    }

    #[test]
    fn test_compile_geom_simplified_description() {
        let data = compile_geom("1{b[16]u[10]r:}".to_string()).unwrap();
        let desc = data.get_simplified_description_string();
        assert!(desc.contains("b[16]"));
        assert!(desc.contains("u[10]"));
        assert!(desc.contains("r:"));
    }

    #[test]
    fn test_compile_geom_with_definitions() {
        let result = compile_geom("bc1 = b[16]\n1{<bc1>r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_multiple_umi() {
        let result = compile_geom("1{b[16]u[10]u[8]r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_trunc_left() {
        let result = compile_geom("1{trunc_left(b[16], 2)r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_trunc_to_left() {
        let result = compile_geom("1{trunc_to_left(b[16], 10)r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_pad_left() {
        let result = compile_geom("1{pad_left(b[16], 4, T)r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_geom_pad_to_left() {
        let result = compile_geom("1{pad_to_left(b[16], 20, G)r:}".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_seqproc_stats_serialization() {
        let stats = SeqprocStats {
            schema_version: "1.11.0".to_string(),
            seqproc_version: "0.1.0".to_string(),
            statistics_level: StatisticsLevel::Detailed,
            call: Some("test".to_string()),
            geometry_digest: None,
            ordering_mode: "input-order".to_string(),
            effective_threads: 1,
            gzip_compression_level: 3,
            parallel_gzip_members: false,
            parallel_gzip_stream: false,
            gzip_compression_threads: 1,
            gzip_block_size: 128 * 1024,
            gzip_input_backend: "needletail-auto".to_owned(),
            gzip_input_threads: 1,
            gzip_input_chunk_size: 0,
            graph_optimization: None,
            execution_plan: None,
            resources: ResourceResolutionReport::default(),
            input_topology: vec![vec!["path".to_owned()], vec!["path".to_owned()]],
            input_layout: "separate".to_owned(),
            input_arity: 2,
            output_arity: 2,
            output_topology: vec!["path".to_owned(), "path".to_owned()],
            n_fastqs: 2,
            n_processed: 100,
            n_reads_max: 1000,
            total_fragments: 100,
            accepted_fragments: 95,
            rejected_fragments: 5,
            failed_parsing: 5,
            rejection_reasons: vec![RejectionReasonCount {
                reason: "test".to_string(),
                count: 5,
            }],
            read_length_mean: vec![150.0, 150.0],
            read_length_min: vec![100, 100],
            read_length_max: vec![200, 200],
            shard_read_counts: vec![vec![100], vec![100]],
            match_distance_stats: vec![],
        };
        let json = serde_json::to_string(&stats).unwrap();
        assert!(json.contains("seqproc_version"));
        assert!(json.contains("0.1.0"));
    }

    #[test]
    fn test_match_distance_stats_serialization() {
        let stats = MatchDistanceStats {
            stage_index: 0,
            label: "test".to_string(),
            attempted: 105,
            matched: 95,
            unmatched: 10,
            distance_histogram: vec![
                DistanceBin {
                    distance: 0,
                    count: 90,
                },
                DistanceBin {
                    distance: 1,
                    count: 5,
                },
            ],
            ambiguity: AmbiguityStats {
                total: 2,
                accepted: 1,
                dropped: 1,
                resolved_first: 1,
                resolved_random: 0,
                resolved_quality: 0,
                position_total: 0,
                position_dropped: 0,
                position_resolved_leftmost: 0,
                position_resolved_rightmost: 0,
                position_resolved_quality: 0,
            },
        };
        let json = serde_json::to_string(&stats).unwrap();
        assert!(json.contains("\"label\":\"test\""));
        assert!(json.contains("\"position_total\":0"));
    }

    #[test]
    fn test_compile_geom_simplified_with_transform() {
        let data =
            compile_geom("1{b<bc>[16]u<umi>[10]r:}2{r:}->1{<bc><umi>}2{<bc>}".to_string()).unwrap();
        let desc = data.get_simplified_description_string();
        assert!(!desc.is_empty());
    }

    #[test]
    fn test_compile_geom_composition() {
        let result = compile_geom("1{trunc_to(rev(b[16]), 10)r:}".to_string());
        assert!(result.is_ok());
    }
}
