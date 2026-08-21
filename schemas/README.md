# seqproc report schemas

`seqproc-summary-1.4.0.schema.json` is the current versioned schema emitted by
`seqproc run --summary`; the 1.0.0 and 1.1.0 files remain immutable for existing
consumers. Schema versions are independent of the seqproc binary version so
consumers can negotiate report compatibility explicitly.

For schema 1.0.0:

- `total_fragments = accepted_fragments + rejected_fragments`;
- `rejected_fragments = failed_parsing` while the legacy field name is retained;
- rejection-reason counts sum to `rejected_fragments`;
- `ordering_mode = input-order` only when the effective thread count is one;
- `geometry_digest` includes its algorithm prefix. Current releases emit
  `blake3:`; early reports emitted `md5:`. It is a content/provenance
  identifier rather than a substitute for archiving the geometry;
- read-length vectors contain one entry per FASTQ input.

Schema 1.1.0 adds the required `gzip_compression_level` and
`parallel_gzip_members` provenance fields.

Schema 1.2.0 adds compression provenance (`parallel_gzip_stream`,
`gzip_compression_threads`, and `gzip_block_size`) plus input-decoder
provenance (`gzip_input_backend`, `gzip_input_threads`, and
`gzip_input_chunk_size`). The compression thread count is the number of workers
that actually perform compression: one for serial gzip, the transform-thread
count for multi-member gzip, or the configured deflate-pool size for
single-stream gzip. The input chunk size is zero when the default needletail
auto-decoder is selected.

Schema 1.3.0 adds the required `statistics_level` field and makes match-stage
attrition explicit with `stage_index`, `attempted`, `matched`, and `unmatched`.
Detailed reports also include counts of ambiguous equal-best matches and the
outcome of the configured ambiguity policy. Basic reports retain run-level
input, output, and rejection totals while leaving `match_distance_stats` and
the three read-length vectors empty.

Schema 1.4.0 adds the conservative graph-optimization report and the effective
execution plan. These identify the requested mode and selected execution
backend, bounded-pipeline parameters, graph cost classes, planner reason codes,
and the compile-time passes that changed the graph. They are optional only for
legacy Rust API paths that cannot reconstruct the frozen graph decision;
ordinary `seqproc run --summary` reports include both objects.

Additive fields require a schema minor version. Removing fields, changing their
meaning, or changing types requires a schema major version and a new file.
