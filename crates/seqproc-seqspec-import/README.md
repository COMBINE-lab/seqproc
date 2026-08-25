# seqproc-seqspec-import

This crate is the typed, deterministic seqspec-to-EFGDL import implementation
used by [`seqproc`](https://github.com/COMBINE-lab/seqproc). Its API is scoped
to seqproc import workflows; it is not intended to be a general replacement
for the official seqspec tools.

The importer is deliberately conservative. It preserves the source document,
projects declared reads onto library regions, and emits input-matching EFGDL 2
without inventing barcode correction, ambiguity, or output-transformation
policies that are absent from seqspec.
