#!/usr/bin/env bash
set -euo pipefail

repo_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
work_dir="$(mktemp -d "${TMPDIR:-/tmp}/seqproc-simd-equivalence.XXXXXX")"
trap 'rm -rf "$work_dir"' EXIT

build_target="${CARGO_TARGET_DIR:-$repo_dir/target}"
baseline_binary="$work_dir/seqproc-baseline"
release_binary="$work_dir/seqproc-release"

echo "Building generic SSE2/NEON control"
RUSTFLAGS="-D warnings" CARGO_TARGET_DIR="$build_target" \
    cargo build --manifest-path "$repo_dir/Cargo.toml" --locked \
    --no-default-features --features baseline-simd
cp "$build_target/debug/seqproc" "$baseline_binary"

echo "Building x86-64-v3/AVX2 candidate"
RUSTFLAGS="-C target-cpu=x86-64-v3 -D warnings" CARGO_TARGET_DIR="$build_target" \
    cargo build --manifest-path "$repo_dir/Cargo.toml" --locked
cp "$build_target/debug/seqproc" "$release_binary"

"$baseline_binary" --version --verbose | grep -F "SIMD backend: x86-sse2"
"$release_binary" --version --verbose | grep -F "compiler CPU target: x86-64-v3"
"$release_binary" --version --verbose | grep -F "SIMD backend: x86-avx2"

while IFS= read -r input_dir; do
    case_name="$(basename "$input_dir")"
    geometry="$repo_dir/tests/fgdl/$case_name.geom"
    input1="$input_dir/${case_name}_l.fastq"
    input2="$input_dir/${case_name}_r.fastq"
    baseline_output="$work_dir/${case_name}.baseline.fastq"
    release_output="$work_dir/${case_name}.release.fastq"
    expected="$repo_dir/tests/test_data/expected_out/${case_name}.fastq"
    additional=()
    while IFS= read -r resource; do
        additional+=(--additional "$resource")
    done < <(
        find "$input_dir" -maxdepth 1 -type f \
            ! -name "${case_name}_l.fastq" \
            ! -name "${case_name}_r.fastq" -print | sort
    )

    common_args=(
        run --geom "$geometry" --read1 "$input1" --read2 "$input2"
        --threads 2 --preserve-order
    )
    "$baseline_binary" "${common_args[@]}" \
        --out1 "$baseline_output" "${additional[@]}"
    "$release_binary" "${common_args[@]}" \
        --out1 "$release_output" "${additional[@]}"

    cmp "$baseline_output" "$release_output"
    cmp "$release_output" "$expected"
    echo "byte-identical: $case_name"
done < <(find "$repo_dir/tests/test_data/in" -mindepth 1 -maxdepth 1 -type d -print | sort)

echo "All baseline and release-SIMD fixtures are byte-identical."
