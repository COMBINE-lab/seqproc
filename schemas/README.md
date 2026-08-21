# seqproc report schemas

`seqproc-summary-1.11.0.schema.json` is the current versioned schema emitted by
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

Schema 1.5.0 separates equal-best pattern ambiguity from equal-best positional
ambiguity. It adds position totals, drops, and leftmost/rightmost resolution
counts to every detailed match-stage ambiguity object.

Schema 1.6.0 adds `position_resolved_quality`, which counts equal-best
placements selected by the position-quality policy.

Schema 1.7.0 adds resolved EFGDL resources, their binding source and BLAKE3
content digest, plus declared resources that were not used by the geometry.

Schema 1.8.0 adds per-lane, per-shard FASTQ record counts.

Schema 1.9.0 adds typed `input_topology` and `output_topology` provenance so
streamed stdin/stdout runs are distinguishable from path-backed and discarded
lanes without exposing path names.

Schema 1.10.0 adds `input_layout`, distinguishing separate logical lanes from
one physical interleaved input stream.

Schema 1.11.0 adds the validated `input_arity` and `output_arity` (currently
bounded to one, two, or three).

Additive fields require a schema minor version. Removing fields, changing their
meaning, or changing types requires a schema major version and a new file.
