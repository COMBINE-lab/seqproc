//! Demultiplexing configuration and utilities for seqproc.
//!
//! This module provides functionality to demultiplex sequencing reads based on
//! barcode-to-sample mappings. It uses ANTISEQUENCE's LookupOp to map barcodes
//! to sample identifiers, then routes reads to per-sample output files.

use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;

use antisequence::expr::label;
use antisequence::graph::{Graph, LookupOp};
use antisequence::trace::NoTrace;
use rustc_hash::FxHashMap;

/// Configuration for demultiplexing.
#[derive(Debug, Clone)]
pub struct DemuxConfig {
    /// Path to TSV file mapping barcodes to sample names
    pub sample_map_path: PathBuf,
    /// Label of the barcode to use for demux (e.g., "seq2.bc1")
    pub barcode_label: String,
    /// Attribute name to store the sample name (default: "sample")
    pub sample_attr: String,
    /// Output directory for demultiplexed files
    pub output_dir: PathBuf,
    /// Value for reads that don't match any barcode
    pub unassigned_name: String,
}

impl Default for DemuxConfig {
    fn default() -> Self {
        Self {
            sample_map_path: PathBuf::new(),
            barcode_label: String::new(),
            sample_attr: "sample".to_string(),
            output_dir: PathBuf::from("demux_out"),
            unassigned_name: "unassigned".to_string(),
        }
    }
}

impl DemuxConfig {
    /// Create a new DemuxConfig with the given sample map file and barcode label.
    pub fn new(sample_map_path: impl Into<PathBuf>, barcode_label: impl Into<String>) -> Self {
        Self {
            sample_map_path: sample_map_path.into(),
            barcode_label: barcode_label.into(),
            ..Default::default()
        }
    }

    /// Set the output directory.
    pub fn with_output_dir(mut self, dir: impl Into<PathBuf>) -> Self {
        self.output_dir = dir.into();
        self
    }

    /// Set the unassigned sample name.
    pub fn with_unassigned_name(mut self, name: impl Into<String>) -> Self {
        self.unassigned_name = name.into();
        self
    }

    /// Load the sample mapping from the TSV file.
    pub fn load_sample_map(&self) -> Result<HashMap<Vec<u8>, Vec<u8>>, std::io::Error> {
        let file = File::open(&self.sample_map_path)?;
        let reader = BufReader::new(file);
        let mut map = HashMap::new();

        for line in reader.lines() {
            let line = line?;
            let line = line.trim();

            // Skip empty lines and comments
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            let parts: Vec<&str> = line.split('\t').collect();
            if parts.len() < 2 {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!(
                        "malformed sample-map line (expected TAB-separated `barcode<TAB>sample`): {line:?}"
                    ),
                ));
            }
            map.insert(parts[0].as_bytes().to_vec(), parts[1].as_bytes().to_vec());
        }

        Ok(map)
    }

    /// Add the LookupOp to the graph for demultiplexing.
    ///
    /// The barcode_label should be in the format "seqN.label" (e.g., "seq2.bc1").
    pub fn add_lookup_op(&self, graph: &mut Graph<NoTrace>) -> Result<(), String> {
        let sample_map = self
            .load_sample_map()
            .map_err(|e| format!("Failed to load sample map: {}", e))?;

        // Convert HashMap to rustc_hash::FxHashMap which LookupOp expects
        let fx_map: FxHashMap<Vec<u8>, Vec<u8>> = sample_map.into_iter().collect();

        // Parse the label (e.g., "seq2.bc1") - label() panics on invalid input
        let input_label = label(&self.barcode_label);

        let lookup_op = LookupOp::new(
            input_label,
            &self.sample_attr,
            fx_map,
            self.unassigned_name.as_bytes(), // Stick in "unassigned" as bytes as the default if the barcode is not found
        );

        graph.add(lookup_op);
        Ok(())
    }

    /// Generate an output file path expression for demux routing.
    ///
    /// Returns an expression string like "demux_out/{sample}_R1.fastq.gz"
    pub fn output_path_expr(&self, read_num: usize) -> String {
        let dir = self.output_dir.to_string_lossy();
        format!(
            "{}/{{seq2.{}.{}}}_R{}.fastq.gz",
            dir,
            self.barcode_label.split('.').next_back().unwrap_or("bc"),
            self.sample_attr,
            read_num
        )
    }
}

/// Summary statistics for demultiplexing.
#[derive(Debug, Clone, Default)]
pub struct DemuxStats {
    /// Number of reads assigned to each sample
    pub sample_counts: HashMap<String, u64>,
    /// Number of unassigned reads
    pub unassigned_count: u64,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_load_sample_map() {
        // Create a temp file with sample mappings
        let mut file = NamedTempFile::new().unwrap();
        writeln!(file, "AACGTGAT\tsample_A").unwrap();
        writeln!(file, "TGGTGGTA\tsample_B").unwrap();
        writeln!(file, "# This is a comment").unwrap();
        writeln!(file, "AACAACCA\tsample_C").unwrap();
        file.flush().unwrap();

        let config = DemuxConfig::new(file.path(), "seq2.bc1");
        let map = config.load_sample_map().unwrap();

        assert_eq!(map.len(), 3);
        assert_eq!(map.get(b"AACGTGAT".as_slice()), Some(&b"sample_A".to_vec()));
        assert_eq!(map.get(b"TGGTGGTA".as_slice()), Some(&b"sample_B".to_vec()));
        assert_eq!(map.get(b"AACAACCA".as_slice()), Some(&b"sample_C".to_vec()));
    }

    #[test]
    fn test_output_path_expr() {
        let config = DemuxConfig::new("/path/to/map.tsv", "seq2.bc1").with_output_dir("my_output");

        assert_eq!(
            config.output_path_expr(1),
            "my_output/{seq2.bc1.sample}_R1.fastq.gz"
        );
        assert_eq!(
            config.output_path_expr(2),
            "my_output/{seq2.bc1.sample}_R2.fastq.gz"
        );
    }

    #[test]
    fn malformed_sample_map_line_is_not_silently_dropped() {
        // A data line that is neither blank nor a comment, but is missing the
        // TAB-separated sample column (here a common mistake: spaces, not a tab).
        // Silently skipping it means every read carrying barcode TGGTGGTA is
        // routed to "unassigned" with no error -- silent data loss. The loader
        // must instead surface the malformed line.
        let mut file = NamedTempFile::new().unwrap();
        writeln!(file, "AACGTGAT\tsample_A").unwrap(); // valid
        writeln!(file, "TGGTGGTA sample_B").unwrap(); // MALFORMED: space, not tab
        file.flush().unwrap();

        let config = DemuxConfig::new(file.path(), "seq2.bc1");
        let result = config.load_sample_map();

        assert!(
            result.is_err(),
            "a malformed (non-comment) sample-map line was silently accepted/skipped \
             instead of raising an error; got Ok({:?})",
            result.ok().map(|m| m.len())
        );
    }
}
