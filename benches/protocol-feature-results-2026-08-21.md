# EFGDL 2 protocol-feature microbenchmarks (2026-08-21)

## Scope

These Criterion measurements isolate the new complex-protocol features. Each
condition processes 10,000 generated single-end FASTQ records with one worker,
null output, 3 seconds of warm-up, and 10 measurement samples. They are not a
replacement for the end-to-end paper benchmarks.

Build inputs were seqproc `dev` with ANTISEQUENCE pinned to
`07a6b2227fe4da960759cefe1583b54589b394ef`, release/LTO profile, and the
repository lockfile. Run with:

```text
SEQPROC_FEATURE_BENCH_READS=10000 \
  cargo bench --bench protocol_feature_benches --offline -- --noplot
```

## Results

| Feature condition | Criterion estimate | Approx. reads/s | Interpretation |
| --- | ---: | ---: | --- |
| Choice succeeds in first arm | 4.794 ms | 2.086 M | Preferred alternative cost |
| Choice succeeds in third arm | 8.282 ms | 1.207 M | 1.73x first-arm time after two rejected attempts |
| All three arms reject | 8.656 ms | 1.155 M | 1.81x first-arm time; preserves isolated fallbacks |
| Headerless EFGDL 1 control | 2.677 ms | 3.736 M | Legacy reference |
| EFGDL 2, no new runtime feature | 2.673 ms | 3.741 M | 0.13% faster; performance-neutral |
| Two manually named captures | 2.873 ms | 3.480 M | Expanded reference |
| Two statically indexed captures | 2.871 ms | 3.483 M | 0.08% faster; lowering is runtime-neutral |
| Exact anchor set, 8 entries | 4.585 ms | 2.181 M | Small-set reference |
| Exact anchor set, 1,024 entries | 5.614 ms | 1.781 M | 22.4% more time, not 128x; indexed matcher scales well |

The feature-free and indexed-capture comparisons are within measurement noise,
as expected: the document header is compile-time metadata, and indexed
captures lower to ordinary physical interval labels. Choice cost depends on
how many isolated alternatives actually execute, so source order should place
the common successful layout first. Large anchor-set cost grows modestly
because matching uses an indexed backend rather than scanning every anchor.

Criterion's raw estimates and HTML reports are under
`target/criterion/{layout_choice_paths,legacy_feature_neutrality,indexed_capture_lowering,anchor_set_scale}`.
