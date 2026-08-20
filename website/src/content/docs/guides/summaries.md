---
title: Run summaries
description: Versioned JSON reports, statistics levels, and provenance.
---

Add `--summary` to write a machine-readable JSON report:

```console
seqproc run ... --summary report.json --statistics-level basic
```

Statistics are disabled in ordinary runs. Requesting a summary enables them;
without an explicit level, summaries default to `detailed`.

## Basic statistics

`--statistics-level basic` records run-level information with minimal
data-dependent instrumentation, including:

- input, accepted, and rejected fragment totals;
- parse failures and categorized rejection reasons;
- effective transform thread count and ordering mode;
- gzip input/output backend parameters;
- seqproc version, command, and geometry digest when available.

## Detailed statistics

The default detailed report adds:

- input read-length minima, maxima, and means;
- ordered match-stage attempts, matches, and attrition;
- match-distance distributions;
- equal-best ambiguity counts and policy outcomes.

Detailed collection uses per-thread counters and aggregates at the end of the
run, but it is still instrumentation. Benchmark statistics-disabled execution
for a headline performance result and measure summary overhead separately.

## Schema versioning

The current source tree emits summary schema 1.3.0. Schemas are versioned
independently from the binary and are committed under
[`schemas/`](https://github.com/COMBINE-lab/seqproc/tree/main/schemas).

Consumers should inspect `schema_version` instead of assuming that all seqproc
revisions emit the same fields. Additive fields require a schema minor version;
removing fields or changing their meaning requires a major version.

The geometry digest is a content/provenance identifier, not a security
primitive. Current releases use the algorithm-tagged form `blake3:<hex>` (early
reports used `md5:<hex>`). Archive the geometry itself and all referenced maps
or whitelists in addition to the report.
