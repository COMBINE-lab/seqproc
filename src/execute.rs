use std::{
    fs::File,
    io::BufWriter,
    panic,
    path::{Path, PathBuf},
    thread,
};

use antisequence::expr::fmt_expr;
use antisequence::graph::TryOp;
use antisequence::graph::*;
use anyhow::{anyhow, bail, Result};
use chumsky::{error::Rich, input::Input, Parser};
use nix::sys::stat;
use nix::unistd;
use serde::Serialize;
use tempfile::tempdir;
use tracing::info;

use crate::{
    compile::{compile, CompiledData},
    demux::DemuxConfig,
    lexer,
    parser::parser,
};

const MIN_PARALLEL_GZIP_BLOCK_SIZE: usize = 32 * 1024;

fn configure_fastq_output(
    output: OutputFastqFileOp,
    config: &RunConfig,
    gzip_threads: usize,
) -> Result<OutputFastqFileOp> {
    let output = output.try_with_gzip_level(config.gzip_level)?;
    if config.parallel_gzip_stream {
        Ok(output.try_with_parallel_gzip_stream(gzip_threads, config.gzip_block_size)?)
    } else {
        Ok(output.with_parallel_gzip_members(config.parallel_gzip))
    }
}

#[derive(Debug)]
pub struct FifoSeqprocData {
    pub r1_fifo: PathBuf,
    pub r2_fifo: PathBuf,
    pub join_handle: thread::JoinHandle<Result<SeqprocStats>>,
}

#[derive(Debug, Clone)]
pub struct RunConfig {
    pub input1: PathBuf,
    pub input2: Option<PathBuf>,
    pub output1: Option<PathBuf>,
    pub output2: Option<PathBuf>,
    pub unassigned1: Option<PathBuf>,
    pub unassigned2: Option<PathBuf>,
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
            output1: None,
            output2: None,
            unassigned1: None,
            unassigned2: None,
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
pub fn run(config: RunConfig, compiled_data: CompiledData) -> Result<RunReport> {
    if config.threads == 0 {
        bail!("number of threads must be greater than zero");
    }
    if config.gzip_level > 9 {
        bail!(
            "gzip compression level must be between 0 and 9, got {}",
            config.gzip_level
        );
    }
    if config.parallel_gzip && config.parallel_gzip_stream {
        bail!("--parallel-gzip and --parallel-gzip-stream are mutually exclusive");
    }
    if config.staged_pipeline && config.execution_mode == ExecutionMode::WholeGraph {
        bail!("staged pipeline execution conflicts with forced whole-graph execution");
    }
    if config.preserve_order
        && config.execution_mode == ExecutionMode::WholeGraph
        && config.threads > 1
    {
        bail!("parallel input-order output requires automatic or pipeline execution");
    }
    // The real-data crossover sweep found that letting the compression pool
    // grow to every transform worker oversubscribed short-read workloads.
    // Four is the best balanced default; explicit settings remain available.
    let gzip_threads = config.gzip_threads.unwrap_or(config.threads.min(4));
    if config.parallel_gzip_stream && gzip_threads == 0 {
        bail!("number of gzip compression threads must be greater than zero");
    }
    if config.accelerated_gzip_input && config.gzip_input_threads == 0 {
        bail!("number of gzip input threads must be greater than zero");
    }
    if config.accelerated_gzip_input && config.gzip_input_chunk_size == 0 {
        bail!("gzip input chunk size must be greater than zero");
    }
    if config.parallel_gzip_stream && config.gzip_block_size < MIN_PARALLEL_GZIP_BLOCK_SIZE {
        bail!(
            "parallel gzip block size must be at least {}, got {}",
            MIN_PARALLEL_GZIP_BLOCK_SIZE,
            config.gzip_block_size
        );
    }
    if config.parallel_gzip_stream
        && (config.demux.is_some() || config.unassigned1.is_some() || config.unassigned2.is_some())
    {
        bail!(
            "parallel single-stream gzip currently supports fixed primary outputs only; demultiplexed and unassigned outputs would create an unbounded number of compression pools"
        );
    }
    let effective_gzip_threads = if config.parallel_gzip_stream {
        gzip_threads
    } else if config.parallel_gzip {
        config.threads
    } else {
        1
    };

    if config.demux.is_none() {
        if let Some(transformations) = &compiled_data.transformation {
            if transformations.len() == 2 && (config.output1.is_none() || config.output2.is_none())
            {
                bail!("geometry transforms into two reads; both output1 and output2 are required");
            }
        }
    }

    let additional_args = config
        .additional_args
        .iter()
        .map(String::as_str)
        .collect::<Vec<_>>();
    let mut graph = Graph::new();
    let mut input_files = vec![config.input1.to_string_lossy().into_owned()];
    if let Some(input2) = &config.input2 {
        input_files.push(input2.to_string_lossy().into_owned());
    }
    let input = if config.accelerated_gzip_input {
        InputFastqOp::from_files_accelerated_gzip(
            input_files,
            config.gzip_input_threads,
            config.gzip_input_chunk_size,
        )
    } else {
        InputFastqOp::from_files(input_files)
    };
    graph.add(input.map_err(|error| anyhow!("failed to open input FASTQ: {error}"))?);

    let has_unassigned = config.unassigned1.is_some() || config.unassigned2.is_some();
    if has_unassigned {
        let mut try_graph = Graph::new();
        compiled_data.interpret(&mut try_graph, &additional_args);

        let mut catch_graph = Graph::new();
        let mut unassigned_files = Vec::new();
        if let Some(path) = &config.unassigned1 {
            unassigned_files.push(path.to_string_lossy().into_owned());
        }
        if config.input2.is_some() {
            if let Some(path) = &config.unassigned2 {
                unassigned_files.push(path.to_string_lossy().into_owned());
            }
        }
        if !unassigned_files.is_empty() {
            catch_graph.add(configure_fastq_output(
                OutputFastqFileOp::from_files(unassigned_files),
                &config,
                gzip_threads,
            )?);
        }
        graph.add(TryOp::new(try_graph, catch_graph));
    } else {
        compiled_data.interpret(&mut graph, &additional_args);
    }

    if let Some(demux) = &config.demux {
        demux
            .add_lookup_op(&mut graph)
            .map_err(|error| anyhow!(error))?;
        std::fs::create_dir_all(&demux.output_dir)?;

        let out_dir = demux.output_dir.to_string_lossy();
        let sample_attr_path = format!("{}.{}", demux.barcode_label, demux.sample_attr);
        let mut expressions = vec![fmt_expr(format!(
            "{}/{{{}}}_R1.fastq",
            out_dir, sample_attr_path
        ))];
        if config.input2.is_some() {
            expressions.push(fmt_expr(format!(
                "{}/{{{}}}_R2.fastq",
                out_dir, sample_attr_path
            )));
        }
        graph.add(configure_fastq_output(
            OutputFastqFileOp::from_files(expressions),
            &config,
            effective_gzip_threads,
        )?);
    } else {
        let output1 = config
            .output1
            .as_deref()
            .unwrap_or_else(|| Path::new("/dev/null"))
            .to_string_lossy()
            .into_owned();
        match (&config.input2, &config.output2) {
            (Some(_), Some(output2)) => {
                graph.add(configure_fastq_output(
                    OutputFastqFileOp::from_files([
                        output1,
                        output2.to_string_lossy().into_owned(),
                    ]),
                    &config,
                    gzip_threads,
                )?);
            }
            _ => {
                graph.add(configure_fastq_output(
                    OutputFastqFileOp::from_file(output1),
                    &config,
                    gzip_threads,
                )?);
            }
        }
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
        .map_err(|error| anyhow!("failed to compile processing graph: {error}"))?;
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
    let planned = graph
        .try_run_planned(execution_request)
        .map_err(|error| anyhow!(error.to_string()))?;
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
        pipeline,
        statistics,
    })
}

struct RuntimeProvenance<'a> {
    config: &'a RunConfig,
    gzip_compression_threads: usize,
    optimization: &'a GraphOptimizationReport,
    execution_plan: &'a ExecutionPlan,
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
    } = provenance;
    let input_stats = graph.input_stats();
    let (n_fastqs, n_processed, n_reads_max, read_length_min, read_length_max, read_length_mean) =
        if let Some(stats) = input_stats {
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

            (
                stats.n_fastqs as u32,
                max_count as u64,
                max_count as u64,
                read_length_min,
                read_length_max,
                read_length_mean,
            )
        } else {
            (0, 0, 0, Vec::new(), Vec::new(), Vec::new())
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
        schema_version: "1.6.0".to_owned(),
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
        match_distance_stats,
    }
}

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
    compiled_data.interpret(&mut main_graph, &additional_args);

    // If unassigned output is requested, wrap in TryOp
    let has_unassigned = unassigned1.is_some() || unassigned2.is_some();

    let mut graph = antisequence::graph::Graph::new();
    let file1_str = file1.to_str().unwrap_or("");

    let mut input_files = vec![file1_str];
    if let Some(f2) = file2 {
        input_files.push(f2.to_str().unwrap_or(""));
    }

    graph.add(
        antisequence::graph::InputFastqOp::from_files(input_files)
            .unwrap_or_else(|e| panic!("{e}")),
    );

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
        compiled_data.interpret(&mut graph, &additional_args);
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

    graph.add(
        antisequence::graph::InputFastqOp::from_files(input_files)
            .unwrap_or_else(|e| panic!("{e}")),
    );

    compiled_data.interpret(&mut graph, &additional_args);

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
) -> Result<SeqprocStats> {
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

    compiled_data.interpret(&mut graph, &additional_args);

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

    let (n_fastqs, n_processed, n_reads_max, read_length_min, read_length_max, read_length_mean) =
        if let Some(s) = input_stats {
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

            (
                n_fastqs,
                n_processed,
                n_reads_max,
                read_length_min_u64,
                read_length_max_u64,
                read_length_mean_f64,
            )
        } else {
            (0, 0, 0, Vec::new(), Vec::new(), Vec::new())
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
        schema_version: "1.6.0".to_string(),
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
        match_distance_stats,
    })
}

pub fn compile_geom(geom: String) -> Result<CompiledData, Vec<Rich<'static, String>>> {
    // lex input
    let tokens = lexer::lexer()
        .parse(&geom)
        .into_result()
        .map_err(|errors| {
            errors
                .into_iter()
                .map(|error| Rich::<String>::custom(*error.span(), error.to_string()).into_owned())
                .collect::<Vec<_>>()
        })?;

    let tokens = tokens
        .into_iter()
        .map(|(tok, span)| chumsky::span::Spanned { inner: tok, span })
        .collect::<Vec<_>>();
    let input = tokens[..].split_spanned((0..geom.len()).into());

    // parse token
    let description = parser().parse(input).into_result().map_err(|errors| {
        errors
            .into_iter()
            .map(|error| Rich::<String>::custom(*error.span(), error.to_string()).into_owned())
            .collect::<Vec<_>>()
    })?;

    // compile ast
    let compiled = compile(description).map_err(|e| {
        let rich = Rich::<String>::custom(e.span, e.msg);
        vec![rich.into_owned()]
    })?;

    // LANG-DEPRECATE: Print deprecation warnings to stderr.
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
) -> Result<SeqprocStats> {
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
) -> Result<FifoSeqprocData> {
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
            assert!(std::path::Path::new(&r1_fifo).exists());
        }
        Err(err) => bail!("Error creating read 1 fifo: {}", err),
    }
    // create new fifo and give read, write and execute rights to the owner
    match unistd::mkfifo(&r2_fifo, stat::Mode::S_IRWXU) {
        Ok(_) => {
            info!("created {:?}", r2_fifo);
            assert!(std::path::Path::new(&r2_fifo).exists());
        }
        Err(err) => bail!("Error creating read 2 fifo: {}", err),
    }

    // we clone this here because we want to move these into
    // the thread that will do the transformation but we need
    // to retain a copy to pass to the FifoXFormData that we
    // will return.
    let r1_fifo_clone = r1_fifo.clone();
    let r2_fifo_clone = r2_fifo.clone();

    let join_handle: thread::JoinHandle<Result<SeqprocStats>> = thread::spawn(move || {
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
            schema_version: "1.6.0".to_string(),
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
