//! # seqproc
//!
//! A high-performance sequence processing tool for single-cell genomics data.
//!
//! seqproc parses geometry specification files (.geom) to extract barcodes, UMIs,
//! and other sequence elements from FASTQ files. It supports:
//!
//! - Flexible geometry definitions with anchoring via linker sequences
//! - Hamming distance tolerance for error-tolerant matching
//! - Demultiplexing with sample barcode lookup
//! - Multi-threaded processing for large datasets
//!
//! ## Modules
//!
//! - `demux`: Sample demultiplexing functionality
//! - `error`: Error types and handling
//! - `execute`: Main execution pipeline
//! - `geometry`: Geometry file parsing and compilation
//! - `processors`: Read processing operations

pub mod demux;
pub mod error;
pub mod execute;
mod geometry;
mod processors;

pub use crate::geometry::*;
pub use crate::processors::*;
