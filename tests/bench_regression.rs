use std::fs::{self, File};
use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;

use seqproc::execute::{compile_geom, read_pairs_to_file};

fn nuc(i: usize) -> u8 {
    const N: [u8; 4] = [b'A', b'C', b'G', b'T'];
    N[i & 3]
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
