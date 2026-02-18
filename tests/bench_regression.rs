use std::fs::{self, File};
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};

use seqproc::demux::DemuxConfig;
use seqproc::execute::{compile_geom, read_pairs_to_file};

fn nuc(i: usize) -> u8 {
    const N: [u8; 4] = [b'A', b'C', b'G', b'T'];
    N[i & 3]
}

fn seq_count_if_exists(path: &Path) -> usize {
    if path.exists() {
        parse_fastq_seq_lengths(path).len()
    } else {
        0
    }
}

fn parse_fastq_sequences(path: &Path) -> Vec<String> {
    let f = File::open(path).unwrap();
    let reader = BufReader::new(f);
    let mut seqs = Vec::new();
    for (line_idx, line) in reader.lines().enumerate() {
        let line = line.unwrap();
        if line_idx % 4 == 1 {
            seqs.push(line);
        }
    }
    seqs
}

fn write_fastq_pair_sci3_mismatch_anchor(dir: &Path, n: usize) -> (PathBuf, PathBuf) {
    // Same as sci3 but change the anchor by 1 nt (CAGAGC -> CAGAGT)
    let mut r1_path = dir.to_path_buf();
    r1_path.push("r1_sci3_mis.fastq");
    let mut r2_path = dir.to_path_buf();
    r2_path.push("r2_sci3_mis.fastq");

    let mut r1 = File::create(&r1_path).unwrap();
    let mut r2 = File::create(&r2_path).unwrap();

    let bad_anchor = b"CAGAGT"; // 1 mismatch vs CAGAGC

    for i in 0..n {
        // R1: brc1 (9/10) + bad_anchor + UMI(8) + b[10]
        writeln!(r1, "@r{}", i).unwrap();
        let bc_len = if i % 2 == 0 { 9 } else { 10 };
        for j in 0..bc_len {
            r1.write_all(&[nuc(i + j * 5)]).unwrap();
        }
        r1.write_all(bad_anchor).unwrap();
        for j in 0..8 {
            r1.write_all(&[nuc(i + j * 11 + 2)]).unwrap();
        }
        for j in 0..10 {
            r1.write_all(&[nuc(i + j * 13 + 1)]).unwrap();
        }
        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..(bc_len + bad_anchor.len() + 8 + 10) {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();

        // R2: 80 nt cDNA
        writeln!(r2, "@r{}", i).unwrap();
        for j in 0..80 {
            r2.write_all(&[nuc(i + j * 9 + 7)]).unwrap();
        }
        writeln!(r2).unwrap();
        writeln!(r2, "+").unwrap();
        for _ in 0..80 {
            r2.write_all(b"I").unwrap();
        }
        writeln!(r2).unwrap();
    }

    (r1_path, r2_path)
}

fn write_fastq_pair_10x(dir: &Path, n: usize) -> (PathBuf, PathBuf) {
    let mut r1_path = dir.to_path_buf();
    r1_path.push("r1_10x.fastq");
    let mut r2_path = dir.to_path_buf();
    r2_path.push("r2_10x.fastq");

    let mut r1 = File::create(&r1_path).unwrap();
    let mut r2 = File::create(&r2_path).unwrap();

    for i in 0..n {
        writeln!(r1, "@r{}", i).unwrap();
        // R1 seq: 16bp CB + 10bp UMI
        for j in 0..16 {
            r1.write_all(&[nuc(i + j)]).unwrap();
        }
        for j in 0..10 {
            r1.write_all(&[nuc(i + 16 + j + 1)]).unwrap();
        }
        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..(16 + 10) {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();

        writeln!(r2, "@r{}", i).unwrap();
        for j in 0..60 {
            r2.write_all(&[nuc(i + j * 7 + 3)]).unwrap();
        }
        writeln!(r2).unwrap();
        writeln!(r2, "+").unwrap();
        for _ in 0..60 {
            r2.write_all(b"I").unwrap();
        }
        writeln!(r2).unwrap();
    }

    (r1_path, r2_path)
}

fn write_fastq_pair_sci3(dir: &Path, n: usize) -> (PathBuf, PathBuf) {
    let mut r1_path = dir.to_path_buf();
    r1_path.push("r1_sci3.fastq");
    let mut r2_path = dir.to_path_buf();
    r2_path.push("r2_sci3.fastq");

    let mut r1 = File::create(&r1_path).unwrap();
    let mut r2 = File::create(&r2_path).unwrap();

    let anchor = b"CAGAGC";

    for i in 0..n {
        writeln!(r1, "@r{}", i).unwrap();
        let bc_len = if i % 2 == 0 { 9 } else { 10 };
        for j in 0..bc_len {
            r1.write_all(&[nuc(i + j * 5)]).unwrap();
        }
        r1.write_all(anchor).unwrap();
        for j in 0..8 {
            r1.write_all(&[nuc(i + j * 11 + 2)]).unwrap();
        }
        for j in 0..10 {
            r1.write_all(&[nuc(i + j * 13 + 1)]).unwrap();
        }
        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..(bc_len + anchor.len() + 8 + 10) {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();

        writeln!(r2, "@r{}", i).unwrap();
        for j in 0..80 {
            r2.write_all(&[nuc(i + j * 9 + 7)]).unwrap();
        }
        writeln!(r2).unwrap();
        writeln!(r2, "+").unwrap();
        for _ in 0..80 {
            r2.write_all(b"I").unwrap();
        }
        writeln!(r2).unwrap();
    }

    (r1_path, r2_path)
}

fn parse_fastq_seq_lengths(path: &Path) -> Vec<usize> {
    let f = File::open(path).unwrap();
    let reader = BufReader::new(f);
    let mut lens = Vec::new();
    for (line_idx, line) in reader.lines().enumerate() {
        let line = line.unwrap();
        if line_idx % 4 == 1 {
            lens.push(line.len());
        }
    }
    lens
}

#[test]
fn regression_10x_trivial() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());
    let (in1, in2) = write_fastq_pair_10x(&dir, 200);
    let out1 = dir.join("out1_10x.fastq");
    let out2 = dir.join("out2_10x.fastq");

    let geom = "1{b[16]u[10]}2{r:}".to_string();
    let compiled = compile_geom(geom).expect("compile_geom");

    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let lens1 = parse_fastq_seq_lengths(&out1);
    let lens2 = parse_fastq_seq_lengths(&out2);

    assert_eq!(lens1.len(), 200);
    assert_eq!(lens2.len(), 200);
    assert!(lens1.iter().all(|&l| l == 26));
    assert!(lens2.iter().all(|&l| l == 60));

    // cleanup
    fs::remove_file(in1).ok();
    fs::remove_file(in2).ok();
    fs::remove_file(out1).ok();
    fs::remove_file(out2).ok();
}

#[test]
fn regression_10x_r2_exact_match() {
    // R2 should pass through unchanged for 10x geometry
    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());
    let (in1, in2) = write_fastq_pair_10x(&dir, 50);
    let out1 = dir.join("out1_10x_match.fastq");
    let out2 = dir.join("out2_10x_match.fastq");

    let geom = "1{b[16]u[10]}2{r:}".to_string();
    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let in2_seqs = parse_fastq_sequences(&in2);
    let out2_seqs = parse_fastq_sequences(&out2);
    assert_eq!(
        in2_seqs, out2_seqs,
        "R2 should be exactly preserved for 10x"
    );
}

#[test]
fn regression_sci3_anchor_content_and_r2_match() {
    // Check that the anchor CAGAGC appears at expected offsets in R1 and R2 is preserved
    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());
    let (in1, in2) = write_fastq_pair_sci3(&dir, 60);
    let out1 = dir.join("out1_sci3_anchor.fastq");
    let out2 = dir.join("out2_sci3_anchor.fastq");

    let geom = r#"
anchor = f[CAGAGC]
brc1  = b[9-10]
1{<brc1><anchor>u[8]b[10]}2{r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let out1_seqs = parse_fastq_sequences(&out1);
    for (i, seq) in out1_seqs.iter().enumerate() {
        let offset = if i % 2 == 0 { 9 } else { 10 };
        assert!(seq.len() >= offset + 6);
        assert_eq!(
            &seq[offset..offset + 6],
            "CAGAGC",
            "anchor at expected offset"
        );
    }

    let in2_seqs = parse_fastq_sequences(&in2);
    let out2_seqs = parse_fastq_sequences(&out2);
    assert_eq!(
        in2_seqs, out2_seqs,
        "R2 should be exactly preserved for sci3"
    );
}

#[test]
fn regression_sci3_strict_anchor_mismatch_drops_reads() {
    // With strict anchor (no tolerance), mismatch should yield zero outputs
    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());
    let (in1, in2) = write_fastq_pair_sci3_mismatch_anchor(&dir, 40);
    let out1 = dir.join("out1_sci3_strict.fastq");
    let out2 = dir.join("out2_sci3_strict.fastq");

    let geom = r#"
anchor = f[CAGAGC]
brc1  = b[9-10]
1{<brc1><anchor>u[8]b[10]}2{r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let n1 = seq_count_if_exists(&out1);
    let n2 = seq_count_if_exists(&out2);
    assert_eq!(n1, 0, "Strict anchor mismatch should drop reads: R1");
    assert_eq!(n2, 0, "Strict anchor mismatch should drop reads: R2");
}

#[test]
fn regression_sci3_tolerant_anchor_mismatch_passes_reads() {
    // With hamming tolerance 1, the 1-nt mismatched anchor should be accepted
    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());
    let (in1, in2) = write_fastq_pair_sci3_mismatch_anchor(&dir, 40);
    let out1 = dir.join("out1_sci3_tol.fastq");
    let out2 = dir.join("out2_sci3_tol.fastq");

    let geom = r#"
anchor = f[CAGAGC]
brc1  = b[9-10]
1{<brc1> hamming(<anchor>, 1) u[8] b[10]}2{r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let lens1 = parse_fastq_seq_lengths(&out1);
    let lens2 = parse_fastq_seq_lengths(&out2);
    assert_eq!(lens1.len(), 40);
    assert_eq!(lens2.len(), 40);

    // Check that the mismatched anchor (CAGAGT) appears at expected offset
    let out1_seqs = parse_fastq_sequences(&out1);
    for (i, seq) in out1_seqs.iter().enumerate() {
        let offset = if i % 2 == 0 { 9 } else { 10 };
        assert!(seq.len() >= offset + 6);
        assert_eq!(
            &seq[offset..offset + 6],
            "CAGAGT",
            "accepted mismatched anchor"
        );
    }
}

#[test]
fn regression_multithread_consistency_10x() {
    // The output should be identical with 1 thread vs 4 threads
    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());
    let (in1, in2) = write_fastq_pair_10x(&dir, 120);
    let out1_t1 = dir.join("out1_10x_t1.fastq");
    let out2_t1 = dir.join("out2_10x_t1.fastq");
    let out1_t4 = dir.join("out1_10x_t4.fastq");
    let out2_t4 = dir.join("out2_10x_t4.fastq");

    let geom = "1{b[16]u[10]}2{r:}".to_string();
    let compiled1 = compile_geom(geom.clone()).expect("compile_geom");
    let compiled2 = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(
        compiled1,
        &in1,
        Some(in2.as_path()),
        &out1_t1,
        &out2_t1,
        1,
        vec![],
    )
    .unwrap();
    read_pairs_to_file(
        compiled2,
        &in1,
        Some(in2.as_path()),
        &out1_t4,
        &out2_t4,
        4,
        vec![],
    )
    .unwrap();

    let r1_t1 = parse_fastq_sequences(&out1_t1);
    let r2_t1 = parse_fastq_sequences(&out2_t1);
    let r1_t4 = parse_fastq_sequences(&out1_t4);
    let r2_t4 = parse_fastq_sequences(&out2_t4);
    assert_eq!(
        r1_t1, r1_t4,
        "R1 content should be identical across threads"
    );
    assert_eq!(
        r2_t1, r2_t4,
        "R2 content should be identical across threads"
    );
}

#[test]
fn regression_multithread_consistency_sci3() {
    // The output should be identical with 1 thread vs 4 threads
    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());
    let (in1, in2) = write_fastq_pair_sci3(&dir, 120);
    let out1_t1 = dir.join("out1_sci3_t1.fastq");
    let out2_t1 = dir.join("out2_sci3_t1.fastq");
    let out1_t4 = dir.join("out1_sci3_t4.fastq");
    let out2_t4 = dir.join("out2_sci3_t4.fastq");

    let geom = r#"
anchor = f[CAGAGC]
brc1  = b[9-10]
1{<brc1><anchor>u[8]b[10]}2{r:}
"#
    .to_string();
    let compiled1 = compile_geom(geom.clone()).expect("compile_geom");
    let compiled2 = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(
        compiled1,
        &in1,
        Some(in2.as_path()),
        &out1_t1,
        &out2_t1,
        1,
        vec![],
    )
    .unwrap();
    read_pairs_to_file(
        compiled2,
        &in1,
        Some(in2.as_path()),
        &out1_t4,
        &out2_t4,
        4,
        vec![],
    )
    .unwrap();

    let r1_t1 = parse_fastq_sequences(&out1_t1);
    let r2_t1 = parse_fastq_sequences(&out2_t1);
    let r1_t4 = parse_fastq_sequences(&out1_t4);
    let r2_t4 = parse_fastq_sequences(&out2_t4);
    assert_eq!(
        r1_t1, r1_t4,
        "R1 content should be identical across threads"
    );
    assert_eq!(
        r2_t1, r2_t4,
        "R2 content should be identical across threads"
    );
}

#[test]
fn regression_sci3_nontrivial() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());
    let (in1, in2) = write_fastq_pair_sci3(&dir, 200);
    let out1 = dir.join("out1_sci3.fastq");
    let out2 = dir.join("out2_sci3.fastq");

    let geom = r#"
anchor = f[CAGAGC]
brc1  = b[9-10]
1{<brc1><anchor>u[8]b[10]}2{r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile_geom");

    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let lens1 = parse_fastq_seq_lengths(&out1);
    let lens2 = parse_fastq_seq_lengths(&out2);

    assert_eq!(lens1.len(), 200);
    assert_eq!(lens2.len(), 200);

    // Expect alternating 33 and 34 bases (bc_len 9 or 10 + anchor 6 + UMI 8 + b[10])
    for (i, &l) in lens1.iter().enumerate() {
        let expected = if i % 2 == 0 {
            9 + 6 + 8 + 10
        } else {
            10 + 6 + 8 + 10
        };
        assert_eq!(l, expected);
    }
    assert!(lens2.iter().all(|&l| l == 80));

    // cleanup
    fs::remove_file(in1).ok();
    fs::remove_file(in2).ok();
    fs::remove_file(out1).ok();
    fs::remove_file(out2).ok();
}

#[test]
fn stats_sci3_exact_anchor_distance_zero() {
    // For the standard sci3 geometry with an exact anchor, all accepted reads
    // should have edit distance 0 at the anchor-matching node.
    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());
    let (in1, in2) = write_fastq_pair_sci3(&dir, 50);
    let out1 = dir.join("out1_sci3_stats_exact.fastq");
    let out2 = dir.join("out2_sci3_stats_exact.fastq");

    let geom = r#"
anchor = f[CAGAGC]
brc1  = b[9-10]
1{<brc1><anchor>u[8]b[10]}2{r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile_geom");

    let stats = read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![])
        .expect("read_pairs_to_file");

    assert!(!stats.match_distance_stats.is_empty());

    let total_reads = 50u64;

    // Find the stats entry whose total count matches the number of reads
    let anchor_entry = stats
        .match_distance_stats
        .iter()
        .find(|s| s.distance_histogram.iter().map(|b| b.count).sum::<u64>() == total_reads)
        .expect("expected one stats entry with total count == total_reads");

    // All mass should be at distance 0
    let zero_count = anchor_entry
        .distance_histogram
        .iter()
        .find(|b| b.distance == 0)
        .map(|b| b.count)
        .unwrap_or(0);
    assert_eq!(zero_count, total_reads);

    for bin in anchor_entry
        .distance_histogram
        .iter()
        .filter(|b| b.distance != 0)
    {
        assert_eq!(bin.count, 0, "expected zero for distance {}", bin.distance);
    }
}

#[test]
fn stats_sci3_tolerant_anchor_mismatch_distance_one() {
    // For the tolerant sci3 geometry with a 1-nt-mismatched anchor and
    // Hamming tolerance 1, all accepted reads should have distance 1 at the
    // anchor-matching node.
    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());
    let (in1, in2) = write_fastq_pair_sci3_mismatch_anchor(&dir, 40);
    let out1 = dir.join("out1_sci3_stats_tol.fastq");
    let out2 = dir.join("out2_sci3_stats_tol.fastq");

    let geom = r#"
anchor = f[CAGAGC]
brc1  = b[9-10]
1{<brc1> hamming(<anchor>, 1) u[8] b[10]}2{r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile_geom");

    let stats = read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![])
        .expect("read_pairs_to_file");

    assert!(!stats.match_distance_stats.is_empty());

    let total_reads = 40u64;

    // Find the stats entry whose total count matches the number of reads
    let anchor_entry = stats
        .match_distance_stats
        .iter()
        .find(|s| s.distance_histogram.iter().map(|b| b.count).sum::<u64>() == total_reads)
        .expect("expected one stats entry with total count == total_reads");

    let one_count = anchor_entry
        .distance_histogram
        .iter()
        .find(|b| b.distance == 1)
        .map(|b| b.count)
        .unwrap_or(0);
    assert_eq!(one_count, total_reads);

    for bin in anchor_entry
        .distance_histogram
        .iter()
        .filter(|b| b.distance != 1)
    {
        assert_eq!(bin.count, 0, "expected zero for distance {}", bin.distance);
    }
}

#[test]
fn regression_fixedseq_linker_hamming_prefix_no_panic() {
    // Regression test for Hamming applied to a named FixedSeq linker.
    //
    // Geometry:
    //   linker = f[ACGTAC]
    //   1{hamming(<linker>, 2) r:}2{r:}
    //
    // This used to be able to leave a Hamming function on the GeometryMeta
    // stack and trigger a panic in execute_stack. The main assertion here
    // is simply that the pipeline runs to completion without error.

    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());

    // Reuse the existing 10x synthetic generator; any reads are fine as long
    // as the graph executes end‑to‑end.
    let (in1, in2) = write_fastq_pair_10x(&dir, 20);
    let out1 = dir.join("out1_linker_hamm.fastq");
    let out2 = dir.join("out2_linker_hamm.fastq");

    let geom = r#"
linker = f[ACGTAC]
1{hamming(<linker>, 2) r:}2{r:}
"#
    .to_string();

    let compiled = compile_geom(geom).expect("compile_geom");

    // The key property: this must not panic inside interpret / execute_stack.
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![])
        .expect("read_pairs_to_file should succeed for linker Hamming geometry");
}

// =============================================================================
// DEMULTIPLEXING TESTS
// =============================================================================

fn write_sample_map(dir: &Path, barcodes: &[(&str, &str)]) -> PathBuf {
    let mut path = dir.to_path_buf();
    path.push("sample_map.tsv");
    let mut f = File::create(&path).unwrap();
    writeln!(f, "# Barcode to Sample Mapping").unwrap();
    for (barcode, sample) in barcodes {
        writeln!(f, "{}\t{}", barcode, sample).unwrap();
    }
    path
}

#[allow(dead_code)]
fn write_fastq_pair_with_barcodes(dir: &Path, barcodes: &[&str]) -> (PathBuf, PathBuf) {
    // Create R1 and R2 files where R2 contains barcodes at position 0-8
    let mut r1_path = dir.to_path_buf();
    r1_path.push("r1_demux.fastq");
    let mut r2_path = dir.to_path_buf();
    r2_path.push("r2_demux.fastq");

    let mut r1 = File::create(&r1_path).unwrap();
    let mut r2 = File::create(&r2_path).unwrap();

    for (i, barcode) in barcodes.iter().enumerate() {
        // R1: 50bp cDNA read
        writeln!(r1, "@read{}", i).unwrap();
        for j in 0..50 {
            r1.write_all(&[nuc(i + j * 7 + 3)]).unwrap();
        }
        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..50 {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();

        // R2: barcode (8bp) + padding (42bp)
        writeln!(r2, "@read{}", i).unwrap();
        r2.write_all(barcode.as_bytes()).unwrap();
        for j in 0..42 {
            r2.write_all(&[nuc(i + j * 11 + 5)]).unwrap();
        }
        writeln!(r2).unwrap();
        writeln!(r2, "+").unwrap();
        for _ in 0..50 {
            r2.write_all(b"I").unwrap();
        }
        writeln!(r2).unwrap();
    }

    (r1_path, r2_path)
}

#[allow(dead_code)]
fn count_files_in_dir(dir: &PathBuf, extension: &str) -> usize {
    if !dir.exists() {
        return 0;
    }
    std::fs::read_dir(dir)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.path()
                .extension()
                .map(|ext| ext == extension)
                .unwrap_or(false)
        })
        .count()
}

#[allow(dead_code)]
fn count_reads_in_file(path: &Path) -> usize {
    if !path.exists() {
        return 0;
    }
    let f = File::open(path).unwrap();
    let reader = BufReader::new(f);
    reader
        .lines()
        .map_while(|l| l.ok())
        .filter(|l| l.starts_with('@'))
        .count()
}

#[test]
fn demux_config_load_sample_map() {
    // Test that DemuxConfig can load a sample map correctly
    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());

    let barcodes = vec![
        ("AACGTGAT", "sample_A"),
        ("TGGTGGTA", "sample_B"),
        ("CGCTGATC", "sample_C"),
    ];
    let map_path = write_sample_map(&dir, &barcodes);

    let config = DemuxConfig::new(&map_path, "seq2.bc1");
    let loaded_map = config.load_sample_map().expect("Failed to load sample map");

    assert_eq!(loaded_map.len(), 3, "Should load 3 barcode mappings");
    assert_eq!(
        loaded_map.get(b"AACGTGAT".as_slice()),
        Some(&b"sample_A".to_vec()),
        "Should correctly map AACGTGAT to sample_A"
    );
    assert_eq!(
        loaded_map.get(b"TGGTGGTA".as_slice()),
        Some(&b"sample_B".to_vec()),
        "Should correctly map TGGTGGTA to sample_B"
    );
}

#[test]
fn demux_config_with_comments_and_empty_lines() {
    // Test that sample map parsing handles comments and empty lines
    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());

    let mut map_path = dir.clone();
    map_path.push("map_with_comments.tsv");
    let mut f = File::create(&map_path).unwrap();
    writeln!(f, "# This is a header comment").unwrap();
    writeln!(f).unwrap();
    writeln!(f, "AACGTGAT\tsample_A").unwrap();
    writeln!(f, "# Another comment").unwrap();
    writeln!(f, "TGGTGGTA\tsample_B").unwrap();
    writeln!(f).unwrap();
    writeln!(f, "CGCTGATC\tsample_C").unwrap();

    let config = DemuxConfig::new(&map_path, "seq2.bc1");
    let loaded_map = config.load_sample_map().expect("Failed to load sample map");

    assert_eq!(
        loaded_map.len(),
        3,
        "Should load exactly 3 entries, ignoring comments and empty lines"
    );
}

#[test]
fn demux_config_output_path_expression() {
    // Test that output path expressions are generated correctly
    let config = DemuxConfig::new("/tmp/map.tsv", "seq2.bc1").with_output_dir("my_demux_output");

    let r1_expr = config.output_path_expr(1);
    let r2_expr = config.output_path_expr(2);

    assert!(
        r1_expr.contains("my_demux_output"),
        "R1 expr should contain output dir"
    );
    assert!(r1_expr.contains("R1"), "R1 expr should contain R1");
    assert!(r2_expr.contains("R2"), "R2 expr should contain R2");
    assert!(
        r1_expr.contains("sample"),
        "R1 expr should contain sample attribute"
    );
}

#[test]
fn demux_config_builder_pattern() {
    // Test the builder pattern for DemuxConfig
    let config = DemuxConfig::new("/path/to/map.tsv", "seq2.bc1")
        .with_output_dir("custom_output")
        .with_unassigned_name("unknown");

    assert_eq!(config.output_dir.to_str().unwrap(), "custom_output");
    assert_eq!(config.unassigned_name, "unknown");
    assert_eq!(config.barcode_label, "seq2.bc1");
    assert_eq!(config.sample_attr, "sample"); // Default value
}

#[test]
fn demux_empty_sample_map() {
    // Test handling of empty sample map
    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());

    let mut map_path = dir.clone();
    map_path.push("empty_map.tsv");
    let mut f = File::create(&map_path).unwrap();
    writeln!(f, "# Empty map with only comments").unwrap();

    let config = DemuxConfig::new(&map_path, "seq2.bc1");
    let loaded_map = config.load_sample_map().expect("Should handle empty map");

    assert_eq!(loaded_map.len(), 0, "Empty map should have 0 entries");
}

#[test]
fn demux_large_sample_map() {
    // Test loading a large sample map (96 wells)
    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());

    let bases = ['A', 'T', 'G', 'C'];
    let mut barcodes: Vec<(String, String)> = Vec::new();
    for i in 0..96 {
        // Generate unique 8bp barcodes using base-4 encoding
        let bc: String = (0..8)
            .map(|j| bases[(i / 4usize.pow(j as u32)) % 4])
            .collect();
        barcodes.push((bc, format!("well_{:02}", i)));
    }

    let mut map_path = dir.clone();
    map_path.push("large_map.tsv");
    let mut f = File::create(&map_path).unwrap();
    for (bc, sample) in &barcodes {
        writeln!(f, "{}\t{}", bc, sample).unwrap();
    }

    let config = DemuxConfig::new(&map_path, "seq2.bc1");
    let loaded_map = config.load_sample_map().expect("Should load large map");

    assert_eq!(loaded_map.len(), 96, "Should load all 96 barcode mappings");
}

#[test]
fn regression_single_end_processing() {
    // Test native single-end processing (no file2)
    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());
    let r1_path = dir.join("se_input.fastq");
    let out1_path = dir.join("se_output.fastq");
    // Dummy out2 path, shouldn't be created
    let out2_path = dir.join("se_output_dummy.fastq");

    let mut f = File::create(&r1_path).unwrap();
    writeln!(f, "@read1").unwrap();
    writeln!(f, "ACGT").unwrap();
    writeln!(f, "+").unwrap();
    writeln!(f, "IIII").unwrap();

    let geom = "1{r:}".to_string(); // Single-read geometry
    let compiled = compile_geom(geom).expect("compile_geom");

    // Pass None for in2
    read_pairs_to_file(compiled, &r1_path, None, &out1_path, &out2_path, 1, vec![])
        .expect("single-end processing failed");

    assert!(out1_path.exists(), "Output R1 should exist");
    assert!(
        !out2_path.exists(),
        "Output R2 should NOT exist for single-end run"
    );

    let out_seqs = parse_fastq_sequences(&out1_path);
    assert_eq!(out_seqs.len(), 1);
    assert_eq!(out_seqs[0], "ACGT");
}

#[test]
fn benchmark_se_vs_pe_overhead() {
    use std::time::Instant;

    let tmp = tempfile::tempdir().unwrap();
    let dir = PathBuf::from(tmp.path());

    // Generate 50k reads for a measurable duration
    let n_reads = 50_000;
    println!("Generating {} reads for benchmark...", n_reads);
    let (in1, in2) = write_fastq_pair_10x(&dir, n_reads);

    let out1_se = dir.join("out1_se.fastq");
    let out2_se = dir.join("out2_se.fastq");

    let out1_pe = dir.join("out1_pe.fastq");
    let out2_pe = dir.join("out2_pe.fastq");

    // 1. Single-End Run
    // Geometry: 1{r:} (Identity on R1)
    let geom_se = "1{r:}".to_string();
    let compiled_se = compile_geom(geom_se).expect("compile_geom se");

    let start_se = Instant::now();
    // Pass None for in2 -> Single-end mode
    read_pairs_to_file(compiled_se, &in1, None, &out1_se, &out2_se, 1, vec![])
        .expect("SE run failed");
    let duration_se = start_se.elapsed();

    // 2. Paired-End Run
    // Geometry: 1{r:}2{r:} (Identity on R1 and R2)
    let geom_pe = "1{r:}2{r:}".to_string();
    let compiled_pe = compile_geom(geom_pe).expect("compile_geom pe");

    let start_pe = Instant::now();
    // Pass Some(in2) -> Paired-end mode
    read_pairs_to_file(
        compiled_pe,
        &in1,
        Some(in2.as_path()),
        &out1_pe,
        &out2_pe,
        1,
        vec![],
    )
    .expect("PE run failed");
    let duration_pe = start_pe.elapsed();

    println!("Single-end duration (50k reads): {:?}", duration_se);
    println!("Paired-end duration (50k reads): {:?}", duration_pe);

    // Verify outputs exist/don't exist
    assert!(out1_se.exists());
    assert!(!out2_se.exists(), "SE run should not create out2");
    assert!(out1_pe.exists());
    assert!(out2_pe.exists(), "PE run should create out2");

    // Correctness check: R1 outputs should be identical
    let s1 = parse_fastq_seq_lengths(&out1_se);
    let p1 = parse_fastq_seq_lengths(&out1_pe);
    assert_eq!(s1.len(), n_reads);
    assert_eq!(s1, p1, "SE and PE R1 outputs should be identical");

    // Performance assertion: SE should be faster or very close to PE
    // (allowing for some system noise, but usually SE is much faster due to half I/O)
    // We assert SE is not > 1.2x PE time (generous margin for noise in small benchmark)
    // Realistically SE < PE.
    let ratio = duration_se.as_secs_f64() / duration_pe.as_secs_f64();
    println!("SE/PE Time Ratio: {:.3}", ratio);

    // If SE is significantly slower, something is wrong.
    if duration_se > duration_pe {
        // If SE is slower, check if it's within noise margin (e.g. < 50ms diff for short run?)
        // For 50k reads, it takes ~100-200ms?
        // Let's just warn for now if it fails, or soft assert.
        // But strict assertion: SE shouldn't be > 1.5x PE.
        assert!(
            ratio < 1.5,
            "Single-end processing significantly slower than Paired-end!"
        );
    }
}
