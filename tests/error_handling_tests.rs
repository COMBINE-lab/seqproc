//! Integration tests: bad user inputs must produce a clean error and a
//! non-zero exit, NOT a Rust panic + backtrace.
//!
//! Regression tests for the code-review findings that seqproc panicked on
//! ordinary user mistakes:
//!   * a missing geometry file -> `read_to_string(&args.geom).unwrap()` in bin.rs
//!   * a missing input FASTQ    -> the input-FASTQ op panicking on open failure
//!
//! A panic surfaces to users as `thread 'main' panicked at ...` (exit 101),
//! which is a poor experience for what are routine "file not found" errors.

use assert_cmd::Command;

fn stderr_on_failure(args: &[&str]) -> String {
    let assert = Command::cargo_bin("seqproc")
        .unwrap()
        .args(args)
        .assert()
        .failure(); // must exit non-zero either way
    String::from_utf8_lossy(&assert.get_output().stderr).into_owned()
}

#[test]
fn missing_geometry_file_errors_cleanly_not_panic() {
    let stderr = stderr_on_failure(&[
        "-g",
        "/no/such/dir/missing.geom",
        "-1",
        "/no/such/dir/r1.fastq",
        "-o",
        "/tmp/seqproc_eh_geom.fastq",
    ]);
    assert!(
        !stderr.to_lowercase().contains("panicked"),
        "seqproc panicked on a missing geometry file instead of a clean error:\n{stderr}"
    );
}

#[test]
fn missing_input_fastq_errors_cleanly_not_panic() {
    // A valid, parseable geometry so we get past parsing/compilation and reach
    // the point where the input FASTQ is opened.
    let geom = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fgdl/match.geom");
    let stderr = stderr_on_failure(&[
        "-g",
        geom,
        "-1",
        "/no/such/dir/missing_r1.fastq",
        "-2",
        "/no/such/dir/missing_r2.fastq",
        "-o",
        "/tmp/seqproc_eh_o1.fastq",
        "-w",
        "/tmp/seqproc_eh_o2.fastq",
    ]);
    assert!(
        !stderr.to_lowercase().contains("panicked"),
        "seqproc panicked on a missing input FASTQ instead of a clean error:\n{stderr}"
    );
}
