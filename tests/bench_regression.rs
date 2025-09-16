use std::fs::{self, File};
use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;

use seqproc::execute::{compile_geom, read_pairs_to_file};

fn nuc(i: usize) -> u8 {
    const N: [u8; 4] = [b'A', b'C', b'G', b'T'];
    N[i & 3]
}

fn seq_count_if_exists(path: &PathBuf) -> usize {
    if path.exists() {
        parse_fastq_seq_lengths(path).len()
    } else {
        0
    }
}

fn parse_fastq_sequences(path: &PathBuf) -> Vec<String> {
    let f = File::open(path).unwrap();
    let reader = BufReader::new(f);
    let mut seqs = Vec::new();
    let mut line_idx = 0usize;
    for line in reader.lines() {
        let line = line.unwrap();
        if line_idx % 4 == 1 {
            seqs.push(line);
        }
        line_idx += 1;
    }
    seqs
}

fn write_fastq_pair_sci3_mismatch_anchor(dir: &PathBuf, n: usize) -> (PathBuf, PathBuf) {
    // Same as sci3 but change the anchor by 1 nt (CAGAGC -> CAGAGT)
    let mut r1_path = dir.clone();
    r1_path.push("r1_sci3_mis.fastq");
    let mut r2_path = dir.clone();
    r2_path.push("r2_sci3_mis.fastq");

    let mut r1 = File::create(&r1_path).unwrap();
    let mut r2 = File::create(&r2_path).unwrap();

    let bad_anchor = b"CAGAGT"; // 1 mismatch vs CAGAGC

    for i in 0..n {
        // R1: brc1 (9/10) + bad_anchor + UMI(8) + b[10]
        writeln!(r1, "@r{}", i).unwrap();
        let bc_len = if i % 2 == 0 { 9 } else { 10 };
        for j in 0..bc_len { r1.write_all(&[nuc(i + j*5)]).unwrap(); }
        r1.write_all(bad_anchor).unwrap();
        for j in 0..8 { r1.write_all(&[nuc(i + j*11 + 2)]).unwrap(); }
        for j in 0..10 { r1.write_all(&[nuc(i + j*13 + 1)]).unwrap(); }
        writeln!(r1, "").unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..(bc_len + bad_anchor.len() + 8 + 10) { r1.write_all(b"I").unwrap(); }
        writeln!(r1, "").unwrap();

        // R2: 80 nt cDNA
        writeln!(r2, "@r{}", i).unwrap();
        for j in 0..80 { r2.write_all(&[nuc(i + j*9 + 7)]).unwrap(); }
        writeln!(r2, "").unwrap();
        writeln!(r2, "+").unwrap();
        for _ in 0..80 { r2.write_all(b"I").unwrap(); }
        writeln!(r2, "").unwrap();
    }

    (r1_path, r2_path)
}

fn write_fastq_pair_10x(dir: &PathBuf, n: usize) -> (PathBuf, PathBuf) {
    let mut r1_path = dir.clone();
    r1_path.push("r1_10x.fastq");
    let mut r2_path = dir.clone();
    r2_path.push("r2_10x.fastq");

    let mut r1 = File::create(&r1_path).unwrap();
    let mut r2 = File::create(&r2_path).unwrap();

    for i in 0..n {
        writeln!(r1, "@r{}", i).unwrap();
        // R1 seq: 16bp CB + 10bp UMI
        for j in 0..16 { r1.write_all(&[nuc(i + j)]).unwrap(); }
        for j in 0..10 { r1.write_all(&[nuc(i + 16 + j + 1)]).unwrap(); }
        writeln!(r1, "").unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..(16+10) { r1.write_all(b"I").unwrap(); }
        writeln!(r1, "").unwrap();

        writeln!(r2, "@r{}", i).unwrap();
        for j in 0..60 { r2.write_all(&[nuc(i + j*7 + 3)]).unwrap(); }
        writeln!(r2, "").unwrap();
        writeln!(r2, "+").unwrap();
        for _ in 0..60 { r2.write_all(b"I").unwrap(); }
        writeln!(r2, "").unwrap();
    }

    (r1_path, r2_path)
}

fn write_fastq_pair_sci3(dir: &PathBuf, n: usize) -> (PathBuf, PathBuf) {
    let mut r1_path = dir.clone();
    r1_path.push("r1_sci3.fastq");
    let mut r2_path = dir.clone();
    r2_path.push("r2_sci3.fastq");

    let mut r1 = File::create(&r1_path).unwrap();
    let mut r2 = File::create(&r2_path).unwrap();

    let anchor = b"CAGAGC";

    for i in 0..n {
        writeln!(r1, "@r{}", i).unwrap();
        let bc_len = if i % 2 == 0 { 9 } else { 10 };
        for j in 0..bc_len { r1.write_all(&[nuc(i + j*5)]).unwrap(); }
        r1.write_all(anchor).unwrap();
        for j in 0..8 { r1.write_all(&[nuc(i + j*11 + 2)]).unwrap(); }
        for j in 0..10 { r1.write_all(&[nuc(i + j*13 + 1)]).unwrap(); }
        writeln!(r1, "").unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..(bc_len + anchor.len() + 8 + 10) { r1.write_all(b"I").unwrap(); }
        writeln!(r1, "").unwrap();

        writeln!(r2, "@r{}", i).unwrap();
        for j in 0..80 { r2.write_all(&[nuc(i + j*9 + 7)]).unwrap(); }
        writeln!(r2, "").unwrap();
        writeln!(r2, "+").unwrap();
        for _ in 0..80 { r2.write_all(b"I").unwrap(); }
        writeln!(r2, "").unwrap();
    }

    (r1_path, r2_path)
}

fn parse_fastq_seq_lengths(path: &PathBuf) -> Vec<usize> {
    let f = File::open(path).unwrap();
    let reader = BufReader::new(f);
    let mut lens = Vec::new();
    let mut line_idx = 0usize;
    for line in reader.lines() {
        let line = line.unwrap();
        if line_idx % 4 == 1 { lens.push(line.len()); }
        line_idx += 1;
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

    read_pairs_to_file(compiled, &in1, &in2, &out1, &out2, 1, vec![]).unwrap();

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
    read_pairs_to_file(compiled, &in1, &in2, &out1, &out2, 1, vec![]).unwrap();

    let in2_seqs = parse_fastq_sequences(&in2);
    let out2_seqs = parse_fastq_sequences(&out2);
    assert_eq!(in2_seqs, out2_seqs, "R2 should be exactly preserved for 10x");
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
"#.to_string();
    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled, &in1, &in2, &out1, &out2, 1, vec![]).unwrap();

    let out1_seqs = parse_fastq_sequences(&out1);
    for (i, seq) in out1_seqs.iter().enumerate() {
        let offset = if i % 2 == 0 { 9 } else { 10 };
        assert!(seq.len() >= offset + 6);
        assert_eq!(&seq[offset..offset+6], "CAGAGC", "anchor at expected offset");
    }

    let in2_seqs = parse_fastq_sequences(&in2);
    let out2_seqs = parse_fastq_sequences(&out2);
    assert_eq!(in2_seqs, out2_seqs, "R2 should be exactly preserved for sci3");
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
"#.to_string();
    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled, &in1, &in2, &out1, &out2, 1, vec![]).unwrap();

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
"#.to_string();
    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled, &in1, &in2, &out1, &out2, 1, vec![]).unwrap();

    let lens1 = parse_fastq_seq_lengths(&out1);
    let lens2 = parse_fastq_seq_lengths(&out2);
    assert_eq!(lens1.len(), 40);
    assert_eq!(lens2.len(), 40);

    // Check that the mismatched anchor (CAGAGT) appears at expected offset
    let out1_seqs = parse_fastq_sequences(&out1);
    for (i, seq) in out1_seqs.iter().enumerate() {
        let offset = if i % 2 == 0 { 9 } else { 10 };
        assert!(seq.len() >= offset + 6);
        assert_eq!(&seq[offset..offset+6], "CAGAGT", "accepted mismatched anchor");
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
    read_pairs_to_file(compiled1, &in1, &in2, &out1_t1, &out2_t1, 1, vec![]).unwrap();
    read_pairs_to_file(compiled2, &in1, &in2, &out1_t4, &out2_t4, 4, vec![]).unwrap();

    let r1_t1 = parse_fastq_sequences(&out1_t1);
    let r2_t1 = parse_fastq_sequences(&out2_t1);
    let r1_t4 = parse_fastq_sequences(&out1_t4);
    let r2_t4 = parse_fastq_sequences(&out2_t4);
    assert_eq!(r1_t1, r1_t4, "R1 content should be identical across threads");
    assert_eq!(r2_t1, r2_t4, "R2 content should be identical across threads");
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
"#.to_string();
    let compiled1 = compile_geom(geom.clone()).expect("compile_geom");
    let compiled2 = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled1, &in1, &in2, &out1_t1, &out2_t1, 1, vec![]).unwrap();
    read_pairs_to_file(compiled2, &in1, &in2, &out1_t4, &out2_t4, 4, vec![]).unwrap();

    let r1_t1 = parse_fastq_sequences(&out1_t1);
    let r2_t1 = parse_fastq_sequences(&out2_t1);
    let r1_t4 = parse_fastq_sequences(&out1_t4);
    let r2_t4 = parse_fastq_sequences(&out2_t4);
    assert_eq!(r1_t1, r1_t4, "R1 content should be identical across threads");
    assert_eq!(r2_t1, r2_t4, "R2 content should be identical across threads");
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
"#.to_string();
    let compiled = compile_geom(geom).expect("compile_geom");

    read_pairs_to_file(compiled, &in1, &in2, &out1, &out2, 1, vec![]).unwrap();

    let lens1 = parse_fastq_seq_lengths(&out1);
    let lens2 = parse_fastq_seq_lengths(&out2);

    assert_eq!(lens1.len(), 200);
    assert_eq!(lens2.len(), 200);

    // Expect alternating 33 and 34 bases (bc_len 9 or 10 + anchor 6 + UMI 8 + b[10])
    for (i, &l) in lens1.iter().enumerate() {
        let expected = if i % 2 == 0 { 9 + 6 + 8 + 10 } else { 10 + 6 + 8 + 10 };
        assert_eq!(l, expected);
    }
    assert!(lens2.iter().all(|&l| l == 80));

    // cleanup
    fs::remove_file(in1).ok();
    fs::remove_file(in2).ok();
    fs::remove_file(out1).ok();
    fs::remove_file(out2).ok();
}
