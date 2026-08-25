//! Conservative seqspec-to-EFGDL import for seqproc.
//!
//! This crate intentionally models only the semantics required to construct a
//! seqproc input matcher. It does not infer output layouts, barcode correction,
//! or fuzzy matching policy from seqspec metadata.

mod diagnostic;
mod model;
mod projection;
mod render;

pub use diagnostic::{DiagnosticCategory, ImportDiagnostic, ImportStatus, Severity};
pub use projection::{
    assess, GeneratedGeometry, ImportAssessment, ImportOptions, InputFile, InputLane, OnlistPolicy,
    PatternOrientation, PatternProjection, ResourceRequirement, SEQSPEC_IMPORT_REPORT_VERSION,
};

/// Version of the importer implementation that produced an assessment or
/// compatibility report.
pub const IMPORTER_VERSION: &str = env!("CARGO_PKG_VERSION");

use thiserror::Error;

#[derive(Debug, Error)]
pub enum ImportError {
    #[error("seqspec YAML could not be parsed: {message}")]
    Yaml {
        message: String,
        line: Option<usize>,
        column: Option<usize>,
    },
}
