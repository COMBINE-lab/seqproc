# seqproc benches

This directory contains Criterion benchmarks for the `seqproc` API on top of the local `ANTISEQUENCE` graph engine.

## Benchmarks

- `antisequence_10x_trivial`
  - Synthetic in-memory FASTQs.
  - Geometry: `1{b[16]u[10]}2{r:}`.

- `antisequence_sci_rna_seq3`
  - Synthetic in-memory FASTQs.
  - Geometry:
    ```
    anchor = f[CAGAGC]
    brc1  = b[9-10]
    1{<brc1><anchor>u[8]b[10]}2{r:}
    ```

- `antisequence_sci_rna_seq3_tolerant`
  - Synthetic in-memory FASTQs.
  - Geometry adds normalization and anchor tolerance:
    ```
    anchor = f[CAGAGC]
    brc1  = norm(b[9-10])
    1{<brc1> hamming(<anchor>, 1) u[8] b[10]}2{r:}
    ```

- `antisequence_sci_rna_seq3_disk`
  - On-disk gzipped FASTQs (ENA subsets).
  - Uses env vars `SCI3_R1` and `SCI3_R2` if set; otherwise defaults to `data/sci3/SRR7827206_{1,2}_10k.fastq.gz` under `CARGO_MANIFEST_DIR`.

- `antisequence_sci_rna_seq3_disk` (multi)
  - Aggregates multiple ENA runs in one benchmark iteration.
  - Reads runs from `SCI3_RUNS` (comma-separated, e.g. `SRR7827206,SRR7827207`),
    else defaults to `SRR7827206..SRR7827215`.
  - Optional root directory override via `SCI3_DIR` (defaults to `data/sci3` under crate).

## Quick commands

- Run all benches:
  ```bash
  cargo bench
  ```

- Run only these benches:
  ```bash
  cargo bench --bench antisequence_benches
  ```

- Filter to specific groups/functions:
  ```bash
  cargo bench --bench antisequence_benches -- 10x_
  cargo bench --bench antisequence_benches -- sci3
  cargo bench --bench antisequence_benches -- sci3_ENA_10k
  cargo bench --bench antisequence_benches -- sci3_ENA_multi
  ```

- Save a baseline before optimizations:
  ```bash
  cargo bench --bench antisequence_benches -- --save-baseline preopt
  ```

- Compare against baseline after changes and save new baseline:
  ```bash
  cargo bench --bench antisequence_benches -- --baseline preopt --save-baseline postopt
  ```

## Data fetch helper

Use the helper to fetch ENA FASTQs and create 10k subsets:

```bash
chmod +x ./scripts/fetch_sci3.sh
./scripts/fetch_sci3.sh             # defaults to SRR7827206 and 10k
# or specify accession and N reads
./scripts/fetch_sci3.sh SRR7827206 20000

# Optionally export to override defaults used by the on-disk bench
export SCI3_R1=seqproc/data/sci3/SRR7827206_1_10k.fastq.gz
export SCI3_R2=seqproc/data/sci3/SRR7827206_2_10k.fastq.gz
```

## Reports

- Open the top-level Criterion HTML report:
  ```bash
  open target/criterion/report/index.html
  ```
- Or open a specific benchmark group’s report:
  ```bash
  open target/criterion/antisequence_sci_rna_seq3/report/index.html
  open target/criterion/antisequence_sci_rna_seq3_tolerant/report/index.html
  open target/criterion/antisequence_sci_rna_seq3_disk/report/index.html
  ```

- Raw estimates (JSON) live under each group/function, e.g.:
  ```
  target/criterion/antisequence_sci_rna_seq3_disk/sci3_ENA_10k/new/estimates.json
  ```


---
# How to Check whether your optimization sped up or slowed down

Compare to the saved “preopt” baseline (runs all benches)
From seqproc/:
```bash
cargo bench --bench antisequence_benches -- --baseline preopt
```
Only the on-disk ENA 10k bench
```bash
cargo bench --bench antisequence_benches -- sci3_ENA_10k -- --baseline preopt
```
Only the multi-run aggregate
```bash
cargo bench --bench antisequence_benches -- sci3_ENA_multi -- --baseline preopt
```
Optional: save a new “postopt” baseline (so you can compare runs later)
```bash
cargo bench --bench antisequence_benches -- --baseline preopt --save-baseline postopt
```
Open the HTML report
```bash
open target/criterion/report/index.html
```
Fetch additional SRR accessions and run the multi-run bench
From the repo root, fetch 10k-read subsets for SRR7827206–SRR7827215:
```bash
for acc in SRR7827{206..215}; do ./seqproc/scripts/fetch_sci3.sh "$acc" 10000; done
```
Run the aggregate multi-run bench (it will auto-detect which runs exist in seqproc/data/sci3/)
```bash
cd seqproc
cargo bench --bench antisequence_benches -- sci3_ENA_multi
```
Optional: explicitly point to the data dir and run list (not required if you used the defaults above)
```bash
export SCI3_DIR="$PWD/data/sci3"
export SCI3_RUNS="SRR7827206,SRR7827207,SRR7827208,SRR7827209,SRR7827210,SRR7827211,SRR7827212,SRR7827213,SRR7827214,SRR7827215"
cargo bench --bench antisequence_benches -- sci3_ENA_multi
```

Notes

The fetch script downloads both mates, creates 10k subsets, and places them in seqproc/data/sci3/.
You can run individual per-run benches too, e.g.:
cargo bench --bench antisequence_benches -- sci3_ENA_SRR7827206
Criterion will print % change vs baseline and also generate the HTML report under target/criterion/.