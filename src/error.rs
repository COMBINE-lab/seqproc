//! Public, structured errors for geometry compilation and execution.

use std::{fmt, io, ops::Range, path::PathBuf};

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::prelude::*;
use serde::Serialize;
use thiserror::Error;

use crate::{
    demux::DemuxError, processors::ProcessorError, resources::ResourceError,
    seqspec_import::SeqspecImportError,
};

pub type SeqprocResult<T> = std::result::Result<T, SeqprocError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum GeometryStage {
    Lexing,
    Parsing,
    SemanticCompilation,
}

impl fmt::Display for GeometryStage {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::Lexing => "lexing",
            Self::Parsing => "parsing",
            Self::SemanticCompilation => "semantic compilation",
        })
    }
}

/// Owned, source-located geometry diagnostic suitable for API consumers.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct GeometryDiagnostic {
    pub message: String,
    pub reason: String,
    pub span: Range<usize>,
    pub contexts: Vec<(String, Range<usize>)>,
}

#[derive(Debug, Error)]
pub enum InputTopologyError {
    #[error("at least one FASTQ input lane is required")]
    MissingLanes,
    #[error("this release supports at most {maximum} FASTQ input lanes; got {observed}")]
    TooManyLanes { maximum: usize, observed: usize },
    #[error("FASTQ input lane {lane} has no shards")]
    EmptyLane { lane: usize },
    #[error("FASTQ input lane {lane} has {observed} shards; expected {expected}")]
    ShardCountMismatch {
        lane: usize,
        expected: usize,
        observed: usize,
    },
    #[error("geometry requires {required} input lanes, but {supplied} were supplied")]
    GeometryArityMismatch { required: usize, supplied: usize },
    #[error("interleaved input and separate input lanes are mutually exclusive")]
    ConflictingLayouts,
    #[error("interleaved input requires at least one FASTQ source")]
    EmptyInterleavedInput,
    #[error("interleaved input supports geometry arities 1 through {maximum}; got {observed}")]
    UnsupportedInterleavedArity { maximum: usize, observed: usize },
    #[error("at most one FASTQ input source may use stdin")]
    MultipleStdin,
    #[error("stdin must be the only shard in its logical input lane")]
    StdinWithShards,
    #[error("interleaved stdin must be the only shard in its input stream")]
    InterleavedStdinWithShards,
    #[error("accelerated gzip input is not available for stdin; gzip stdin is auto-detected")]
    AcceleratedGzipStdin,
}

#[derive(Debug, Error)]
pub enum OutputTopologyError {
    #[error("at least one primary FASTQ output target must not be discard")]
    MissingPrimaryOutput,
    #[error("geometry transforms into two reads; both output1 and output2 are required")]
    LegacyPairedOutputRequired,
    #[error(
        "{supplied} primary output targets were supplied, but the geometry emits {required} reads"
    )]
    ArityMismatch { required: usize, supplied: usize },
    #[error("unassigned output arity is {supplied}, but input arity is {input_arity}")]
    UnassignedArityMismatch { supplied: usize, input_arity: usize },
    #[error("primary FASTQ outputs cannot be combined with demultiplexing; use --demux-out-dir")]
    PrimaryOutputsWithDemultiplexing,
    #[error("at most one FASTQ output target may use stdout")]
    MultipleStdout,
    #[error("stdout gzip compression was requested, but no output target uses stdout")]
    StdoutGzipWithoutStdout,
    #[error("parallel gzip output is not supported when a lane targets stdout")]
    ParallelGzipStdout,
    #[error("parallel single-stream gzip currently supports fixed primary outputs only")]
    ParallelStreamVariableOutputs,
}

#[derive(Debug, Error)]
pub enum ExecutionConfigError {
    #[error("number of threads must be greater than zero (got {0})")]
    ThreadCount(usize),
    #[error("gzip compression level must be between 0 and 9, got {0}")]
    GzipLevel(u32),
    #[error("parallel member gzip and parallel stream gzip are mutually exclusive")]
    ConflictingGzipModes,
    #[error("staged pipeline execution conflicts with forced whole-graph execution")]
    StagedWholeGraphConflict,
    #[error("parallel input-order output requires automatic or pipeline execution")]
    OrderedWholeGraph,
    #[error("number of gzip compression threads must be greater than zero")]
    GzipThreadCount,
    #[error("number of gzip input threads must be greater than zero")]
    GzipInputThreadCount,
    #[error("gzip input chunk size must be greater than zero")]
    GzipInputChunkSize,
    #[error("parallel gzip block size must be at least {minimum}, got {observed}")]
    GzipBlockSize { minimum: usize, observed: usize },
    #[error("dynamic batch planning requires a nonzero memory budget")]
    BatchMemoryBudget,
}

#[derive(Debug, Error)]
pub enum SeqprocError {
    #[error("geometry {stage} failed with {} diagnostic(s)", diagnostics.len())]
    Geometry {
        stage: GeometryStage,
        diagnostics: Vec<GeometryDiagnostic>,
    },
    #[error(transparent)]
    Resource(#[from] ResourceError),
    #[error(transparent)]
    InputTopology(#[from] InputTopologyError),
    #[error("failed to configure {context} FASTQ input: {source}")]
    FastqInput {
        context: &'static str,
        #[source]
        source: antisequence::errors::Error,
    },
    #[error(transparent)]
    OutputTopology(#[from] OutputTopologyError),
    #[error("failed to configure FASTQ output: {source}")]
    FastqOutput {
        #[source]
        source: io::Error,
    },
    #[error(transparent)]
    InvalidExecutionConfiguration(#[from] ExecutionConfigError),
    #[error(transparent)]
    GraphConstruction(#[from] ProcessorError),
    #[error("failed to compile processing graph: {source}")]
    GraphCompilation {
        #[source]
        source: antisequence::errors::Error,
    },
    #[error("invalid execution plan: {source}")]
    ExecutionPlanning {
        #[source]
        source: antisequence::errors::Error,
    },
    #[error("processing graph execution failed: {source}")]
    GraphExecution {
        #[source]
        source: antisequence::errors::Error,
    },
    #[error(transparent)]
    Demultiplex(#[from] DemuxError),
    #[error(transparent)]
    SeqspecImport(#[from] SeqspecImportError),
    #[error("{operation} failed for `{target}`: {source}")]
    Io {
        operation: &'static str,
        target: PathBuf,
        #[source]
        source: io::Error,
    },
    #[error("{operation} stopped because the stdout consumer closed its pipe")]
    StdoutBrokenPipe { operation: &'static str },
    #[error("unsupported feature combination: {0}")]
    Unsupported(String),
}

impl SeqprocError {
    /// Broad, stable CLI status classes: 2 configuration, 3 malformed/runtime
    /// input, 1 graph/output execution, and 0 for normal stdout early closure.
    pub fn exit_code(&self) -> i32 {
        match self {
            Self::Geometry { .. }
            | Self::Resource(_)
            | Self::InputTopology(_)
            | Self::OutputTopology(_)
            | Self::InvalidExecutionConfiguration(_)
            | Self::ExecutionPlanning { .. }
            | Self::Demultiplex(_)
            | Self::SeqspecImport(_)
            | Self::Unsupported(_) => 2,
            Self::FastqInput { .. } => 3,
            Self::StdoutBrokenPipe { .. } => 0,
            Self::FastqOutput { .. }
            | Self::GraphConstruction(_)
            | Self::GraphCompilation { .. }
            | Self::GraphExecution { .. }
            | Self::Io { .. } => 1,
        }
    }

    pub fn geometry_diagnostics(&self) -> Option<(GeometryStage, &[GeometryDiagnostic])> {
        match self {
            Self::Geometry { stage, diagnostics } => Some((*stage, diagnostics)),
            _ => None,
        }
    }
}

pub fn render_geometry_diagnostics(
    source: &str,
    diagnostics: &[GeometryDiagnostic],
) -> io::Result<()> {
    for diagnostic in diagnostics {
        Report::build(ReportKind::Error, ((), diagnostic.span.clone()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(&diagnostic.message)
            .with_label(
                Label::new(((), diagnostic.span.clone()))
                    .with_message(&diagnostic.reason)
                    .with_color(Color::Red),
            )
            .with_labels(diagnostic.contexts.iter().map(|(message, span)| {
                Label::new(((), span.clone()))
                    .with_message(message)
                    .with_color(Color::Yellow)
            }))
            .finish()
            .write(Source::from(source), io::stderr())?;
    }
    Ok(())
}

/// Compatibility helper retained for downstream code using the original API.
pub fn failure(
    msg: String,
    label: (String, SimpleSpan),
    extra_labels: impl IntoIterator<Item = (String, SimpleSpan)>,
    source: String,
) -> ! {
    let diagnostic = GeometryDiagnostic {
        message: msg,
        reason: label.0,
        span: label.1.into_range(),
        contexts: extra_labels
            .into_iter()
            .map(|(message, span)| (message, span.into_range()))
            .collect(),
    };
    if let Err(error) = render_geometry_diagnostics(&source, &[diagnostic]) {
        eprintln!("error: failed to render geometry diagnostic: {error}");
    }
    std::process::exit(1)
}

pub fn parse_failure(err: &Rich<'_, impl fmt::Display>, src: String) -> ! {
    failure(
        err.to_string(),
        (err.reason().to_string(), *err.span()),
        err.contexts()
            .map(|(label, span)| (format!("while parsing this {label}"), *span)),
        src,
    )
}
