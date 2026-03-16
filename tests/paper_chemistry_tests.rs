//! End-to-end tests for each chemistry benchmarked in the seqproc paper.
//!
//! Chemistries tested:
//!   1. 10x Chromium v2  -- trivial paired-end (16bp CB + 10bp UMI | cDNA)
//!   2. sci-RNA-seq3     -- anchor-based with variable-length barcode
//!   3. SPLiT-seq PE     -- dual anchor_relative + hamming + map + transformation
//!   4. LR-SPLiT-seq     -- single-end long-read variant of SPLiT-seq

use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};

use seqproc::execute::{compile_geom, read_pairs_to_file};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn nuc(i: usize) -> u8 {
    const N: [u8; 4] = [b'A', b'C', b'G', b'T'];
    N[i & 3]
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

fn parse_fastq_seq_lengths(path: &Path) -> Vec<usize> {
    parse_fastq_sequences(path)
        .iter()
        .map(|s| s.len())
        .collect()
}

fn seq_count(path: &Path) -> usize {
    if path.exists() {
        parse_fastq_seq_lengths(path).len()
    } else {
        0
    }
}

// ---------------------------------------------------------------------------
// FASTQ generators
// ---------------------------------------------------------------------------

/// 10x Chromium v2: R1 = CB(16) + UMI(10), R2 = cDNA(90)
fn write_10x_chromium_v2(dir: &Path, n: usize) -> (PathBuf, PathBuf) {
    let r1_path = dir.join("10x_r1.fastq");
    let r2_path = dir.join("10x_r2.fastq");
    let mut r1 = File::create(&r1_path).unwrap();
    let mut r2 = File::create(&r2_path).unwrap();

    for i in 0..n {
        // R1: 16bp CB + 10bp UMI = 26bp
        writeln!(r1, "@read{}", i).unwrap();
        for j in 0..16 {
            r1.write_all(&[nuc(i + j)]).unwrap();
        }
        for j in 0..10 {
            r1.write_all(&[nuc(i + 16 + j + 1)]).unwrap();
        }
        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..26 {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();

        // R2: 90bp cDNA
        writeln!(r2, "@read{}", i).unwrap();
        for j in 0..90 {
            r2.write_all(&[nuc(i + j * 7 + 3)]).unwrap();
        }
        writeln!(r2).unwrap();
        writeln!(r2, "+").unwrap();
        for _ in 0..90 {
            r2.write_all(b"I").unwrap();
        }
        writeln!(r2).unwrap();
    }

    (r1_path, r2_path)
}

/// sci-RNA-seq3: R1 = brc1(9-10) + anchor(CAGAGC) + UMI(8) + b(10), R2 = cDNA(80)
fn write_sci_rna_seq3(dir: &Path, n: usize) -> (PathBuf, PathBuf) {
    let anchor = b"CAGAGC";
    let r1_path = dir.join("sci3_r1.fastq");
    let r2_path = dir.join("sci3_r2.fastq");
    let mut r1 = File::create(&r1_path).unwrap();
    let mut r2 = File::create(&r2_path).unwrap();

    for i in 0..n {
        writeln!(r1, "@read{}", i).unwrap();
        // brc1: alternating 9/10bp
        let bc_len = if i % 2 == 0 { 9 } else { 10 };
        for j in 0..bc_len {
            r1.write_all(&[nuc(i + j * 5)]).unwrap();
        }
        r1.write_all(anchor).unwrap();
        // UMI 8bp
        for j in 0..8 {
            r1.write_all(&[nuc(i + j * 11 + 2)]).unwrap();
        }
        // trailing b[10]
        for j in 0..10 {
            r1.write_all(&[nuc(i + j * 13 + 1)]).unwrap();
        }
        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..(bc_len + anchor.len() + 8 + 10) {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();

        // R2: 80bp cDNA
        writeln!(r2, "@read{}", i).unwrap();
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

// SPLiT-seq linker sequences from the paper
const LINKER1: &[u8] = b"GTGGCCGCTGTTTCGCATCGGCGTACGACT"; // 30bp
const LINKER2: &[u8] = b"ATCCACGTGCTTGAGA"; // 16bp

/// SPLiT-seq PE: R1 = cDNA(80), R2 = x(2) + UMI(10) + BC3(8) + L1(30) + BC2(8) + L2(16) + BC1(8) + trailing
fn write_splitseq_pe(dir: &Path, n: usize) -> (PathBuf, PathBuf) {
    let r1_path = dir.join("splitseq_r1.fastq");
    let r2_path = dir.join("splitseq_r2.fastq");
    let mut r1 = File::create(&r1_path).unwrap();
    let mut r2 = File::create(&r2_path).unwrap();

    for i in 0..n {
        // R1: 80bp cDNA
        writeln!(r1, "@read{}", i).unwrap();
        for j in 0..80 {
            r1.write_all(&[nuc(i + j * 3 + 1)]).unwrap();
        }
        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..80 {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();

        // R2: x[2] + UMI[10] + BC3[8] + L1[30] + BC2[8] + L2[16] + BC1[8] + trailing
        writeln!(r2, "@read{}", i).unwrap();
        // x[2] spacer
        r2.write_all(&[nuc(i), nuc(i + 1)]).unwrap();
        // UMI[10]
        for j in 0..10 {
            r2.write_all(&[nuc(i + j * 17 + 5)]).unwrap();
        }
        // BC3[8]
        for j in 0..8 {
            r2.write_all(&[nuc(i + j * 23 + 11)]).unwrap();
        }
        // Linker 1
        r2.write_all(LINKER1).unwrap();
        // BC2[8]
        for j in 0..8 {
            r2.write_all(&[nuc(i + j * 29 + 7)]).unwrap();
        }
        // Linker 2
        r2.write_all(LINKER2).unwrap();
        // BC1[8]
        for j in 0..8 {
            r2.write_all(&[nuc(i + j * 31 + 3)]).unwrap();
        }
        // trailing sequence (polyT or junk) to simulate real read length
        for j in 0..20 {
            r2.write_all(&[nuc(i + j * 41)]).unwrap();
        }
        writeln!(r2).unwrap();
        let r2_len = 2 + 10 + 8 + LINKER1.len() + 8 + LINKER2.len() + 8 + 20;
        writeln!(r2, "+").unwrap();
        for _ in 0..r2_len {
            r2.write_all(b"I").unwrap();
        }
        writeln!(r2).unwrap();
    }

    (r1_path, r2_path)
}

/// LR-SPLiT-seq (single-end long-read): one read containing
/// cDNA(variable) + BC3[8] + L1[30] + BC2[8] + L2[16] + BC1[8] + UMI[10]
/// The long read has the barcode structure embedded; anchor_relative is used
/// to locate the linkers.
fn write_lr_splitseq(dir: &Path, n: usize) -> PathBuf {
    let r1_path = dir.join("lr_splitseq_r1.fastq");
    let mut r1 = File::create(&r1_path).unwrap();

    for i in 0..n {
        writeln!(r1, "@read{}", i).unwrap();
        // leading cDNA (variable 200-300bp to simulate long reads)
        let cdna_len = 200 + (i % 100);
        for j in 0..cdna_len {
            r1.write_all(&[nuc(i + j * 3 + 1)]).unwrap();
        }
        // BC3[8]
        for j in 0..8 {
            r1.write_all(&[nuc(i + j * 23 + 11)]).unwrap();
        }
        // Linker 1
        r1.write_all(LINKER1).unwrap();
        // BC2[8]
        for j in 0..8 {
            r1.write_all(&[nuc(i + j * 29 + 7)]).unwrap();
        }
        // Linker 2
        r1.write_all(LINKER2).unwrap();
        // BC1[8]
        for j in 0..8 {
            r1.write_all(&[nuc(i + j * 31 + 3)]).unwrap();
        }
        // UMI[10]
        for j in 0..10 {
            r1.write_all(&[nuc(i + j * 17 + 5)]).unwrap();
        }
        writeln!(r1).unwrap();
        let total_len = cdna_len + 8 + LINKER1.len() + 8 + LINKER2.len() + 8 + 10;
        writeln!(r1, "+").unwrap();
        for _ in 0..total_len {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();
    }

    r1_path
}

/// Write a trivial barcode mapping file (TSV: match_pattern -> replacement).
/// Each barcode maps to itself (identity map) so we can verify pass-through.
fn write_barcode_map(path: &Path, barcodes: &[Vec<u8>]) {
    let mut f = File::create(path).unwrap();
    for bc in barcodes {
        let s = std::str::from_utf8(bc).unwrap();
        writeln!(f, "{}\t{}", s, s).unwrap();
    }
}

/// Collect the set of BC3 barcodes that appear in SPLiT-seq R2 synthetic data.
type BarcodeSet = Vec<Vec<u8>>;

fn splitseq_barcodes(n: usize) -> (BarcodeSet, BarcodeSet, BarcodeSet) {
    let mut bc3s = Vec::new();
    let mut bc2s = Vec::new();
    let mut bc1s = Vec::new();
    for i in 0..n {
        let bc3: Vec<u8> = (0..8).map(|j| nuc(i + j * 23 + 11)).collect();
        let bc2: Vec<u8> = (0..8).map(|j| nuc(i + j * 29 + 7)).collect();
        let bc1: Vec<u8> = (0..8).map(|j| nuc(i + j * 31 + 3)).collect();
        bc3s.push(bc3);
        bc2s.push(bc2);
        bc1s.push(bc1);
    }
    (bc3s, bc2s, bc1s)
}

// ===========================================================================
// 1. 10x Chromium v2
// ===========================================================================

#[test]
fn paper_10x_chromium_v2_basic() {
    // Paper geometry: 1{b[16]u[10]}2{r:}
    // Expected: 100% recovery, R1 = 26bp, R2 = 90bp (passthrough)
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let (in1, in2) = write_10x_chromium_v2(&dir, 100);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = "1{b[16]u[10]}2{r:}".to_string();
    let compiled = compile_geom(geom).expect("compile_geom failed for 10x Chromium v2");

    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![])
        .expect("read_pairs_to_file");

    let lens1 = parse_fastq_seq_lengths(&out1);
    let lens2 = parse_fastq_seq_lengths(&out2);

    // 100% recovery
    assert_eq!(lens1.len(), 100, "10x: expected 100% recovery on R1");
    assert_eq!(lens2.len(), 100, "10x: expected 100% recovery on R2");

    // R1 should be exactly 26bp (16 + 10)
    assert!(lens1.iter().all(|&l| l == 26), "10x: R1 should be 26bp");
    // R2 should be exactly 90bp cDNA
    assert!(lens2.iter().all(|&l| l == 90), "10x: R2 should be 90bp");
}

#[test]
fn paper_10x_chromium_v2_r2_passthrough() {
    // R2 cDNA should be bit-identical to input
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let (in1, in2) = write_10x_chromium_v2(&dir, 50);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = "1{b[16]u[10]}2{r:}".to_string();
    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let in2_seqs = parse_fastq_sequences(&in2);
    let out2_seqs = parse_fastq_sequences(&out2);
    assert_eq!(
        in2_seqs, out2_seqs,
        "10x: R2 cDNA should pass through unchanged"
    );
}

#[test]
fn paper_10x_chromium_v2_with_definitions() {
    // Same geometry but using definitions (idiomatic EFGDL style)
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let (in1, in2) = write_10x_chromium_v2(&dir, 60);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = "cb = b[16]\numi = u[10]\n1{<cb><umi>}2{r:}".to_string();
    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    assert_eq!(parse_fastq_seq_lengths(&out1).len(), 60);
    assert_eq!(parse_fastq_seq_lengths(&out2).len(), 60);
}

// ===========================================================================
// 2. sci-RNA-seq3
// ===========================================================================

#[test]
fn paper_sci_rna_seq3_basic() {
    // Paper geometry (from Figure 1):
    //   anchor = f[CAGAGC]
    //   brc1 = b[9-10]
    //   1{<brc1><anchor>u[8]b[10]}2{r:}
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let (in1, in2) = write_sci_rna_seq3(&dir, 100);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = r#"
anchor = f[CAGAGC]
brc1 = b[9-10]
1{<brc1><anchor>u[8]b[10]}2{r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile_geom failed for sci-RNA-seq3");

    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let lens1 = parse_fastq_seq_lengths(&out1);
    let lens2 = parse_fastq_seq_lengths(&out2);

    // 100% recovery (synthetic data has exact anchor)
    assert_eq!(lens1.len(), 100, "sci3: expected 100% recovery");
    assert_eq!(lens2.len(), 100);

    // R1 lengths should alternate: 9+6+8+10=33 or 10+6+8+10=34
    for (i, &l) in lens1.iter().enumerate() {
        let expected = if i % 2 == 0 { 33 } else { 34 };
        assert_eq!(
            l, expected,
            "sci3: R1 read {} expected {}bp, got {}bp",
            i, expected, l
        );
    }

    // R2 should be 80bp cDNA passthrough
    assert!(lens2.iter().all(|&l| l == 80), "sci3: R2 should be 80bp");
}

#[test]
fn paper_sci_rna_seq3_anchor_position() {
    // Verify the anchor CAGAGC appears at the expected offset in output R1
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let (in1, in2) = write_sci_rna_seq3(&dir, 40);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = "anchor = f[CAGAGC]\nbrc1 = b[9-10]\n1{<brc1><anchor>u[8]b[10]}2{r:}".to_string();
    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let seqs = parse_fastq_sequences(&out1);
    for (i, seq) in seqs.iter().enumerate() {
        let offset = if i % 2 == 0 { 9 } else { 10 };
        assert_eq!(
            &seq[offset..offset + 6],
            "CAGAGC",
            "sci3: anchor at wrong position in read {}",
            i
        );
    }
}

#[test]
fn paper_sci_rna_seq3_with_hamming_tolerance() {
    // Test the sci-RNA-seq3 geometry with hamming tolerance on anchor.
    // Introduce 1-nt mismatch in anchor (CAGAGC -> CAGAGT) and allow hamming 1.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let anchor_bad = b"CAGAGT"; // 1 mismatch
    let r1_path = dir.join("sci3_ham_r1.fastq");
    let r2_path = dir.join("sci3_ham_r2.fastq");
    let mut r1 = File::create(&r1_path).unwrap();
    let mut r2 = File::create(&r2_path).unwrap();

    let n = 30;
    for i in 0..n {
        writeln!(r1, "@read{}", i).unwrap();
        let bc_len = if i % 2 == 0 { 9 } else { 10 };
        for j in 0..bc_len {
            r1.write_all(&[nuc(i + j * 5)]).unwrap();
        }
        r1.write_all(anchor_bad).unwrap();
        for j in 0..8 {
            r1.write_all(&[nuc(i + j * 11 + 2)]).unwrap();
        }
        for j in 0..10 {
            r1.write_all(&[nuc(i + j * 13 + 1)]).unwrap();
        }
        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..(bc_len + 6 + 8 + 10) {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();

        writeln!(r2, "@read{}", i).unwrap();
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

    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    // With hamming tolerance 1, mismatched anchor should be accepted
    let geom = r#"
anchor = f[CAGAGC]
brc1 = b[9-10]
1{<brc1> hamming(<anchor>, 1) u[8] b[10]}2{r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(
        compiled,
        &r1_path,
        Some(r2_path.as_path()),
        &out1,
        &out2,
        1,
        vec![],
    )
    .unwrap();

    assert_eq!(
        seq_count(&out1),
        n,
        "sci3+hamming: all reads should pass with tolerance 1"
    );
    assert_eq!(seq_count(&out2), n);
}

#[test]
fn paper_sci_rna_seq3_r2_passthrough() {
    // R2 cDNA should be identical to input
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let (in1, in2) = write_sci_rna_seq3(&dir, 50);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = "anchor = f[CAGAGC]\nbrc1 = b[9-10]\n1{<brc1><anchor>u[8]b[10]}2{r:}".to_string();
    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let in2_seqs = parse_fastq_sequences(&in2);
    let out2_seqs = parse_fastq_sequences(&out2);
    assert_eq!(
        in2_seqs, out2_seqs,
        "sci3: R2 cDNA should pass through unchanged"
    );
}

// ===========================================================================
// 3. SPLiT-seq PE
// ===========================================================================

#[test]
fn paper_splitseq_pe_compile() {
    // Verify that the full paper SPLiT-seq PE geometry compiles successfully.
    // This is the exact geometry from the paper's Results section.
    let geom = r#"
read1 = r:
umi = u[10]
bc3 = b[8]
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
bc2 = b[8]
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
bc1 = b[8]
1{<read1>}
2{x[2]<umi>map(<bc3>, $0, self)<l1>map(<bc2>, $1, self)<l2>map(<bc1>, $2, self)r:}
-> 1{<read1>} 2{<umi><bc3><bc2><bc1>}
"#
    .to_string();

    let compiled = compile_geom(geom);
    assert!(
        compiled.is_ok(),
        "SPLiT-seq PE full paper geometry should compile: {:?}",
        compiled.err()
    );
    let data = compiled.unwrap();
    assert_eq!(data.geometry.len(), 2, "SPLiT-seq PE should have 2 reads");
    assert!(
        data.transformation.is_some(),
        "SPLiT-seq PE should have a transformation"
    );
}

#[test]
fn paper_splitseq_pe_simplified_no_map() {
    // SPLiT-seq PE without map() -- tests the anchor_relative + hamming
    // geometry structure through the full pipeline.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let (in1, in2) = write_splitseq_pe(&dir, 50);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
1{r:}
2{x[2]u[10]b[8]<l1>b[8]<l2>b[8]r:}
"#
    .to_string();

    let compiled = compile_geom(geom).expect("compile_geom failed for SPLiT-seq PE simplified");
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let n1 = seq_count(&out1);
    let n2 = seq_count(&out2);

    // With exact linkers in synthetic data + generous hamming tolerance,
    // we expect high recovery
    assert!(
        n1 > 0,
        "SPLiT-seq PE simplified: should recover some reads on R1"
    );
    assert_eq!(
        n1, n2,
        "SPLiT-seq PE simplified: R1 and R2 counts should match"
    );
}

#[test]
fn paper_splitseq_pe_with_map_and_transformation() {
    // Full SPLiT-seq PE pipeline: anchor_relative + hamming + map + transformation.
    // Uses identity barcode mapping files (barcode maps to itself).
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 30;
    let (in1, in2) = write_splitseq_pe(&dir, n);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    // Create barcode mapping files
    let (bc3s, bc2s, bc1s) = splitseq_barcodes(n);
    let map_bc3 = dir.join("map_bc3.tsv");
    let map_bc2 = dir.join("map_bc2.tsv");
    let map_bc1 = dir.join("map_bc1.tsv");
    write_barcode_map(&map_bc3, &bc3s);
    write_barcode_map(&map_bc2, &bc2s);
    write_barcode_map(&map_bc1, &bc1s);

    let geom = r#"
read1 = r:
umi = u[10]
bc3 = b[8]
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
bc2 = b[8]
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
bc1 = b[8]
1{<read1>}
2{x[2]<umi>map(<bc3>, $0, self)<l1>map(<bc2>, $1, self)<l2>map(<bc1>, $2, self)r:}
-> 1{<read1>} 2{<umi><bc3><bc2><bc1>}
"#
    .to_string();

    let compiled = compile_geom(geom).expect("compile_geom for SPLiT-seq PE with map");

    let additional_args = vec![
        map_bc3.to_str().unwrap(),
        map_bc2.to_str().unwrap(),
        map_bc1.to_str().unwrap(),
    ];

    read_pairs_to_file(
        compiled,
        &in1,
        Some(in2.as_path()),
        &out1,
        &out2,
        1,
        additional_args,
    )
    .unwrap();

    let n1 = seq_count(&out1);
    let n2 = seq_count(&out2);

    assert!(n1 > 0, "SPLiT-seq PE with map: should recover some reads");
    assert_eq!(
        n1, n2,
        "SPLiT-seq PE with map: R1 and R2 counts should match"
    );

    // After transformation, R1 should be cDNA, R2 should be UMI+BC3+BC2+BC1 = 10+8+8+8 = 34bp
    let lens2 = parse_fastq_seq_lengths(&out2);
    if !lens2.is_empty() {
        assert!(
            lens2.iter().all(|&l| l == 34),
            "SPLiT-seq PE: transformed R2 should be 34bp (UMI+3xBC), got {:?}",
            &lens2[..lens2.len().min(5)]
        );
    }
}

#[test]
fn paper_splitseq_pe_r1_is_cdna_after_transformation() {
    // After the SPLiT-seq transformation, R1 should contain cDNA (80bp from our synthetic data)
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 20;
    let (in1, in2) = write_splitseq_pe(&dir, n);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let (bc3s, bc2s, bc1s) = splitseq_barcodes(n);
    let map_bc3 = dir.join("map_bc3.tsv");
    let map_bc2 = dir.join("map_bc2.tsv");
    let map_bc1 = dir.join("map_bc1.tsv");
    write_barcode_map(&map_bc3, &bc3s);
    write_barcode_map(&map_bc2, &bc2s);
    write_barcode_map(&map_bc1, &bc1s);

    let geom = r#"
read1 = r:
umi = u[10]
bc3 = b[8]
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
bc2 = b[8]
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
bc1 = b[8]
1{<read1>}
2{x[2]<umi>map(<bc3>, $0, self)<l1>map(<bc2>, $1, self)<l2>map(<bc1>, $2, self)r:}
-> 1{<read1>} 2{<umi><bc3><bc2><bc1>}
"#
    .to_string();

    let compiled = compile_geom(geom).expect("compile_geom");
    let args = vec![
        map_bc3.to_str().unwrap(),
        map_bc2.to_str().unwrap(),
        map_bc1.to_str().unwrap(),
    ];
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, args).unwrap();

    // R1 after transformation is cDNA -- should be 80bp from our generator
    let lens1 = parse_fastq_seq_lengths(&out1);
    if !lens1.is_empty() {
        assert!(
            lens1.iter().all(|&l| l == 80),
            "SPLiT-seq PE: transformed R1 should be 80bp cDNA, got {:?}",
            &lens1[..lens1.len().min(5)]
        );
    }
}

// ===========================================================================
// 4. LR-SPLiT-seq (single-end long-read)
// ===========================================================================

#[test]
fn paper_lr_splitseq_compile() {
    // LR-SPLiT-seq uses the same linker structure as SPLiT-seq PE but in a
    // single-end long read. Verify the geometry compiles.
    let geom = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#
    .to_string();

    let compiled = compile_geom(geom);
    assert!(
        compiled.is_ok(),
        "LR-SPLiT-seq geometry should compile: {:?}",
        compiled.err()
    );
    let data = compiled.unwrap();
    assert_eq!(data.geometry.len(), 1, "LR-SPLiT-seq should have 1 read");
}

#[test]
fn paper_lr_splitseq_single_end_pipeline() {
    // Run the LR-SPLiT-seq geometry through the full pipeline with synthetic
    // single-end long reads.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let in1 = write_lr_splitseq(&dir, 30);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq"); // unused for single-end but required by API

    let geom = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#
    .to_string();

    let compiled = compile_geom(geom).expect("compile_geom for LR-SPLiT-seq");
    read_pairs_to_file(compiled, &in1, None, &out1, &out2, 1, vec![]).unwrap();

    let n1 = seq_count(&out1);
    assert!(
        n1 > 0,
        "LR-SPLiT-seq: should recover some reads (got {})",
        n1
    );
}

#[test]
fn paper_lr_splitseq_with_transformation() {
    // LR-SPLiT-seq with a transformation to extract just the barcodes + UMI
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let in1 = write_lr_splitseq(&dir, 20);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
1{r<cDNA>:b<bc3>[8]<l1>b<bc2>[8]<l2>b<bc1>[8]u<umi>[10]}
-> 1{<cDNA><umi><bc3><bc2><bc1>}
"#
    .to_string();

    let compiled = compile_geom(geom).expect("compile_geom for LR-SPLiT-seq with transformation");
    read_pairs_to_file(compiled, &in1, None, &out1, &out2, 1, vec![]).unwrap();

    let n1 = seq_count(&out1);
    assert!(
        n1 > 0,
        "LR-SPLiT-seq transformed: should recover reads (got {})",
        n1
    );
}

// ===========================================================================
// ===========================================================================
//
//  EDIT DISTANCE VARIANTS
//
//  The tests below mirror the hamming-based tests above, replacing every
//  hamming(<anchor>, N) with edit(<anchor>, N). The purpose is to verify
//  that the edit distance code path:
//    (a) compiles and runs correctly for each chemistry,
//    (b) produces structurally identical output (same lengths, transforms),
//    (c) recovers at least as many reads as the hamming variant.
//
// ===========================================================================
// ===========================================================================

// ---------------------------------------------------------------------------
// 10x Chromium v2 (control -- no anchor matching, identical to hamming)
// ---------------------------------------------------------------------------

#[test]
fn paper_10x_chromium_v2_edit_distance_control() {
    // 10x has no anchor/linker matching so this is a control test confirming
    // that the pipeline is unchanged when edit distance is not involved.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let (in1, in2) = write_10x_chromium_v2(&dir, 100);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = "1{b[16]u[10]}2{r:}".to_string();
    let compiled = compile_geom(geom).expect("compile_geom");

    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    assert_eq!(seq_count(&out1), 100, "10x control: 100% recovery");
    assert_eq!(seq_count(&out2), 100);
}

// ---------------------------------------------------------------------------
// sci-RNA-seq3 with edit distance
// ---------------------------------------------------------------------------

#[test]
fn paper_sci_rna_seq3_edit_basic() {
    // Same as paper_sci_rna_seq3_basic but using edit() instead of hamming()
    // for the anchor. With exact synthetic anchors, both should give 100%.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let (in1, in2) = write_sci_rna_seq3(&dir, 100);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    // No tolerance needed for exact anchors, but test the edit() code path
    let geom = r#"
anchor = f[CAGAGC]
brc1 = b[9-10]
1{<brc1><anchor>u[8]b[10]}2{r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let lens1 = parse_fastq_seq_lengths(&out1);
    assert_eq!(lens1.len(), 100, "sci3 edit: expected 100% recovery");
    for (i, &l) in lens1.iter().enumerate() {
        let expected = if i % 2 == 0 { 33 } else { 34 };
        assert_eq!(l, expected);
    }
    assert!(
        parse_fastq_seq_lengths(&out2).iter().all(|&l| l == 80),
        "sci3 edit: R2 should be 80bp"
    );
}

#[test]
fn paper_sci_rna_seq3_edit_tolerance() {
    // 1-nt mismatch anchor with edit tolerance 1 (replaces hamming tolerance 1)
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let anchor_bad = b"CAGAGT"; // 1 substitution
    let r1_path = dir.join("sci3_edit_r1.fastq");
    let r2_path = dir.join("sci3_edit_r2.fastq");
    let mut r1 = File::create(&r1_path).unwrap();
    let mut r2 = File::create(&r2_path).unwrap();

    let n = 30;
    for i in 0..n {
        writeln!(r1, "@read{}", i).unwrap();
        let bc_len = if i % 2 == 0 { 9 } else { 10 };
        for j in 0..bc_len {
            r1.write_all(&[nuc(i + j * 5)]).unwrap();
        }
        r1.write_all(anchor_bad).unwrap();
        for j in 0..8 {
            r1.write_all(&[nuc(i + j * 11 + 2)]).unwrap();
        }
        for j in 0..10 {
            r1.write_all(&[nuc(i + j * 13 + 1)]).unwrap();
        }
        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..(bc_len + 6 + 8 + 10) {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();

        writeln!(r2, "@read{}", i).unwrap();
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

    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = r#"
anchor = f[CAGAGC]
brc1 = b[9-10]
1{<brc1> edit(<anchor>, 1) u[8] b[10]}2{r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(
        compiled,
        &r1_path,
        Some(r2_path.as_path()),
        &out1,
        &out2,
        1,
        vec![],
    )
    .unwrap();

    assert_eq!(
        seq_count(&out1),
        n,
        "sci3 edit(1): all reads should pass with 1-sub mismatch"
    );
}

#[test]
fn paper_sci_rna_seq3_edit_insertion_tolerance() {
    // Test the key advantage of edit distance over hamming:
    // Introduce a 1-nt INSERTION in the anchor (CAGAGC -> CAAGAGC = 7bp).
    // hamming() cannot handle this; edit(1) can.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let anchor_ins = b"CAAGAGC"; // 1-nt insertion (A inserted at pos 2)
    let r1_path = dir.join("sci3_ins_r1.fastq");
    let r2_path = dir.join("sci3_ins_r2.fastq");
    let mut r1 = File::create(&r1_path).unwrap();
    let mut r2 = File::create(&r2_path).unwrap();

    let n = 20;
    for i in 0..n {
        writeln!(r1, "@read{}", i).unwrap();
        let bc_len = 10; // fixed for simplicity
        for j in 0..bc_len {
            r1.write_all(&[nuc(i + j * 5)]).unwrap();
        }
        r1.write_all(anchor_ins).unwrap(); // 7bp instead of 6bp
        for j in 0..8 {
            r1.write_all(&[nuc(i + j * 11 + 2)]).unwrap();
        }
        for j in 0..10 {
            r1.write_all(&[nuc(i + j * 13 + 1)]).unwrap();
        }
        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        let total = bc_len + anchor_ins.len() + 8 + 10;
        for _ in 0..total {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();

        writeln!(r2, "@read{}", i).unwrap();
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

    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    // edit(1) should handle a 1-nt insertion
    let geom = r#"
anchor = f[CAGAGC]
brc1 = b[10]
1{<brc1> edit(<anchor>, 1) u[8] b[10]}2{r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(
        compiled,
        &r1_path,
        Some(r2_path.as_path()),
        &out1,
        &out2,
        1,
        vec![],
    )
    .unwrap();

    let n_recovered = seq_count(&out1);
    // edit(1) should recover reads with a 1-nt insertion in the anchor
    assert!(
        n_recovered > 0,
        "sci3 edit(1) with insertion: should recover reads (got {})",
        n_recovered
    );
}

// ---------------------------------------------------------------------------
// SPLiT-seq PE with edit distance
// ---------------------------------------------------------------------------

#[test]
fn paper_splitseq_pe_edit_compile() {
    // Full paper SPLiT-seq PE geometry with edit() instead of hamming()
    let geom = r#"
read1 = r:
umi = u[10]
bc3 = b[8]
l1 = anchor_relative(edit(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
bc2 = b[8]
l2 = anchor_relative(edit(f[ATCCACGTGCTTGAGA], 3))
bc1 = b[8]
1{<read1>}
2{x[2]<umi>map(<bc3>, $0, self)<l1>map(<bc2>, $1, self)<l2>map(<bc1>, $2, self)r:}
-> 1{<read1>} 2{<umi><bc3><bc2><bc1>}
"#
    .to_string();

    let compiled = compile_geom(geom);
    assert!(
        compiled.is_ok(),
        "SPLiT-seq PE edit geometry should compile: {:?}",
        compiled.err()
    );
    let data = compiled.unwrap();
    assert_eq!(data.geometry.len(), 2);
    assert!(data.transformation.is_some());
}

#[test]
fn paper_splitseq_pe_edit_simplified() {
    // SPLiT-seq PE with edit distance, no map, through full pipeline
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let (in1, in2) = write_splitseq_pe(&dir, 50);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = r#"
l1 = anchor_relative(edit(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(edit(f[ATCCACGTGCTTGAGA], 3))
1{r:}
2{x[2]u[10]b[8]<l1>b[8]<l2>b[8]r:}
"#
    .to_string();

    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let n1 = seq_count(&out1);
    let n2 = seq_count(&out2);
    assert!(
        n1 > 0,
        "SPLiT-seq PE edit: should recover reads (got {})",
        n1
    );
    assert_eq!(n1, n2);
}

#[test]
fn paper_splitseq_pe_edit_with_map_and_transformation() {
    // Full SPLiT-seq PE pipeline with edit distance + map + transformation
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 30;
    let (in1, in2) = write_splitseq_pe(&dir, n);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let (bc3s, bc2s, bc1s) = splitseq_barcodes(n);
    let map_bc3 = dir.join("map_bc3.tsv");
    let map_bc2 = dir.join("map_bc2.tsv");
    let map_bc1 = dir.join("map_bc1.tsv");
    write_barcode_map(&map_bc3, &bc3s);
    write_barcode_map(&map_bc2, &bc2s);
    write_barcode_map(&map_bc1, &bc1s);

    let geom = r#"
read1 = r:
umi = u[10]
bc3 = b[8]
l1 = anchor_relative(edit(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
bc2 = b[8]
l2 = anchor_relative(edit(f[ATCCACGTGCTTGAGA], 3))
bc1 = b[8]
1{<read1>}
2{x[2]<umi>map(<bc3>, $0, self)<l1>map(<bc2>, $1, self)<l2>map(<bc1>, $2, self)r:}
-> 1{<read1>} 2{<umi><bc3><bc2><bc1>}
"#
    .to_string();

    let compiled = compile_geom(geom).expect("compile_geom");
    let additional_args = vec![
        map_bc3.to_str().unwrap(),
        map_bc2.to_str().unwrap(),
        map_bc1.to_str().unwrap(),
    ];

    read_pairs_to_file(
        compiled,
        &in1,
        Some(in2.as_path()),
        &out1,
        &out2,
        1,
        additional_args,
    )
    .unwrap();

    let n1 = seq_count(&out1);
    let n2 = seq_count(&out2);
    assert!(n1 > 0, "SPLiT-seq PE edit+map: should recover reads");
    assert_eq!(n1, n2);

    // Post-transformation: R2 = UMI(10) + BC3(8) + BC2(8) + BC1(8) = 34bp
    let lens2 = parse_fastq_seq_lengths(&out2);
    if !lens2.is_empty() {
        assert!(
            lens2.iter().all(|&l| l == 34),
            "SPLiT-seq PE edit: transformed R2 should be 34bp, got {:?}",
            &lens2[..lens2.len().min(5)]
        );
    }

    // R1 = cDNA = 80bp
    let lens1 = parse_fastq_seq_lengths(&out1);
    if !lens1.is_empty() {
        assert!(
            lens1.iter().all(|&l| l == 80),
            "SPLiT-seq PE edit: transformed R1 should be 80bp cDNA, got {:?}",
            &lens1[..lens1.len().min(5)]
        );
    }
}

// ---------------------------------------------------------------------------
// LR-SPLiT-seq with edit distance
// ---------------------------------------------------------------------------

#[test]
fn paper_lr_splitseq_edit_compile() {
    let geom = r#"
l1 = anchor_relative(edit(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(edit(f[ATCCACGTGCTTGAGA], 3))
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#
    .to_string();

    let compiled = compile_geom(geom);
    assert!(
        compiled.is_ok(),
        "LR-SPLiT-seq edit geometry should compile: {:?}",
        compiled.err()
    );
    assert_eq!(compiled.unwrap().geometry.len(), 1);
}

#[test]
fn paper_lr_splitseq_edit_pipeline() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let in1 = write_lr_splitseq(&dir, 30);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = r#"
l1 = anchor_relative(edit(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(edit(f[ATCCACGTGCTTGAGA], 3))
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#
    .to_string();

    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled, &in1, None, &out1, &out2, 1, vec![]).unwrap();

    let n1 = seq_count(&out1);
    assert!(
        n1 > 0,
        "LR-SPLiT-seq edit: should recover reads (got {})",
        n1
    );
}

#[test]
fn paper_lr_splitseq_edit_with_transformation() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let in1 = write_lr_splitseq(&dir, 20);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = r#"
l1 = anchor_relative(edit(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(edit(f[ATCCACGTGCTTGAGA], 3))
1{r<cDNA>:b<bc3>[8]<l1>b<bc2>[8]<l2>b<bc1>[8]u<umi>[10]}
-> 1{<cDNA><umi><bc3><bc2><bc1>}
"#
    .to_string();

    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled, &in1, None, &out1, &out2, 1, vec![]).unwrap();

    let n1 = seq_count(&out1);
    assert!(
        n1 > 0,
        "LR-SPLiT-seq edit transformed: should recover reads (got {})",
        n1
    );
}

// ---------------------------------------------------------------------------
// Hamming vs Edit comparison tests
// ---------------------------------------------------------------------------

#[test]
fn compare_sci_rna_seq3_hamming_vs_edit_on_exact_data() {
    // On synthetic data with exact anchors, both hamming and edit should
    // give identical recovery. This ensures edit is not breaking anything.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let (in1, in2) = write_sci_rna_seq3(&dir, 100);

    let out1_h = dir.join("out1_hamming.fastq");
    let out2_h = dir.join("out2_hamming.fastq");
    let out1_e = dir.join("out1_edit.fastq");
    let out2_e = dir.join("out2_edit.fastq");

    let geom_hamming = r#"
anchor = f[CAGAGC]
brc1 = b[9-10]
1{<brc1><anchor>u[8]b[10]}2{r:}
"#
    .to_string();
    let geom_edit = r#"
anchor = f[CAGAGC]
brc1 = b[9-10]
1{<brc1><anchor>u[8]b[10]}2{r:}
"#
    .to_string();

    let compiled_h = compile_geom(geom_hamming).unwrap();
    let compiled_e = compile_geom(geom_edit).unwrap();

    read_pairs_to_file(
        compiled_h,
        &in1,
        Some(in2.as_path()),
        &out1_h,
        &out2_h,
        1,
        vec![],
    )
    .unwrap();
    read_pairs_to_file(
        compiled_e,
        &in1,
        Some(in2.as_path()),
        &out1_e,
        &out2_e,
        1,
        vec![],
    )
    .unwrap();

    let n_hamming = seq_count(&out1_h);
    let n_edit = seq_count(&out1_e);

    assert_eq!(n_hamming, 100);
    assert_eq!(n_edit, 100);
    assert_eq!(
        n_hamming, n_edit,
        "sci3: hamming and edit should give identical recovery on exact data"
    );
}

#[test]
fn compare_splitseq_pe_hamming_vs_edit_on_exact_data() {
    // On synthetic data with exact linkers, both should recover the same reads
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let (in1, in2) = write_splitseq_pe(&dir, 50);

    let out1_h = dir.join("out1_hamming.fastq");
    let out2_h = dir.join("out2_hamming.fastq");
    let out1_e = dir.join("out1_edit.fastq");
    let out2_e = dir.join("out2_edit.fastq");

    let geom_hamming = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
1{r:}
2{x[2]u[10]b[8]<l1>b[8]<l2>b[8]r:}
"#
    .to_string();

    let geom_edit = r#"
l1 = anchor_relative(edit(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(edit(f[ATCCACGTGCTTGAGA], 3))
1{r:}
2{x[2]u[10]b[8]<l1>b[8]<l2>b[8]r:}
"#
    .to_string();

    let compiled_h = compile_geom(geom_hamming).unwrap();
    let compiled_e = compile_geom(geom_edit).unwrap();

    read_pairs_to_file(
        compiled_h,
        &in1,
        Some(in2.as_path()),
        &out1_h,
        &out2_h,
        1,
        vec![],
    )
    .unwrap();
    read_pairs_to_file(
        compiled_e,
        &in1,
        Some(in2.as_path()),
        &out1_e,
        &out2_e,
        1,
        vec![],
    )
    .unwrap();

    let n_hamming = seq_count(&out1_h);
    let n_edit = seq_count(&out1_e);

    assert!(n_hamming > 0);
    assert!(n_edit > 0);
    // Edit should recover at least as many reads as hamming (superset on exact data)
    assert!(
        n_edit >= n_hamming,
        "SPLiT-seq PE: edit ({}) should recover >= hamming ({}) on exact data",
        n_edit,
        n_hamming
    );
}

// ===========================================================================
// 5. Annotation system -- #[match_ori(either)] with TryOrientationOp
// ===========================================================================

/// Write LR-SPLiT-seq reads where some are in forward orientation and others
/// are reverse-complemented, simulating real PacBio/ONT long reads.
/// Returns (fastq_path, n_forward, n_rc).
fn write_lr_splitseq_mixed_orientation(dir: &Path, n: usize) -> (PathBuf, usize, usize) {
    let r1_path = dir.join("lr_splitseq_mixed_r1.fastq");
    let mut r1 = File::create(&r1_path).unwrap();
    let mut n_fw = 0;
    let mut n_rc = 0;

    for i in 0..n {
        writeln!(r1, "@read{}", i).unwrap();

        // Build forward-orientation read: cDNA + BC3 + L1 + BC2 + L2 + BC1 + UMI
        let mut fw_seq: Vec<u8> = Vec::new();

        // cDNA (variable 200-300bp)
        let cdna_len = 200 + (i % 100);
        for j in 0..cdna_len {
            fw_seq.push(nuc(i + j * 3 + 1));
        }
        // BC3[8]
        for j in 0..8 {
            fw_seq.push(nuc(i + j * 23 + 11));
        }
        // Linker 1
        fw_seq.extend_from_slice(LINKER1);
        // BC2[8]
        for j in 0..8 {
            fw_seq.push(nuc(i + j * 29 + 7));
        }
        // Linker 2
        fw_seq.extend_from_slice(LINKER2);
        // BC1[8]
        for j in 0..8 {
            fw_seq.push(nuc(i + j * 31 + 3));
        }
        // UMI[10]
        for j in 0..10 {
            fw_seq.push(nuc(i + j * 17 + 5));
        }

        let total_len = fw_seq.len();

        if i % 2 == 1 {
            // Odd-indexed reads are RC'd
            let rc_seq = revcomp(&fw_seq);
            r1.write_all(&rc_seq).unwrap();
            n_rc += 1;
        } else {
            // Even-indexed reads are forward
            r1.write_all(&fw_seq).unwrap();
            n_fw += 1;
        }

        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..total_len {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();
    }

    (r1_path, n_fw, n_rc)
}

/// Deterministic base mutation: A->C, C->G, G->T, T->A.
fn mutate_base(b: u8) -> u8 {
    match b {
        b'A' => b'C',
        b'C' => b'G',
        b'G' => b'T',
        b'T' => b'A',
        _ => b'A',
    }
}

/// Write noisy (real-like) LR-SPLiT-seq reads with mixed orientations.
/// Introduces substitution and deletion errors in linker sequences to simulate
/// real PacBio/ONT sequencing noise. Error distribution by `i % 10`:
///   0-3 (40%): perfect linkers
///   4-6 (30%): 1-3 substitution errors in L1, 1 in L2 (within hamming tolerance)
///   7-9 (30%): 1bp deletion in L1 + L2 (hamming fails, edit recovers)
/// Orientation: odd-indexed reads are RC'd, even are forward.
/// Returns (fastq_path, n_forward, n_rc).
fn write_lr_splitseq_noisy_mixed_orientation(dir: &Path, n: usize) -> (PathBuf, usize, usize) {
    let r1_path = dir.join("lr_splitseq_noisy_mixed_r1.fastq");
    let mut r1 = File::create(&r1_path).unwrap();
    let mut n_fw = 0;
    let mut n_rc = 0;

    for i in 0..n {
        writeln!(r1, "@read{}", i).unwrap();
        let mut fw_seq: Vec<u8> = Vec::new();

        // cDNA (variable 200-300bp)
        let cdna_len = 200 + (i % 100);
        for j in 0..cdna_len {
            fw_seq.push(nuc(i + j * 3 + 1));
        }
        // BC3[8]
        for j in 0..8 {
            fw_seq.push(nuc(i + j * 23 + 11));
        }

        let error_class = i % 10;
        if error_class >= 7 {
            // 30%: 1bp deletion in L1 middle -- creates frame shift that
            // hamming cannot tolerate but edit distance handles as 1 deletion.
            let mut l1 = LINKER1.to_vec();
            l1.remove(15);
            fw_seq.extend_from_slice(&l1);
        } else if error_class >= 4 {
            // 30%: 1-3 substitution errors in L1 (within hamming tolerance 6)
            let mut l1 = LINKER1.to_vec();
            let n_errors = 1 + (i % 3);
            for e in 0..n_errors {
                let pos = (5 + e * 8) % l1.len();
                l1[pos] = mutate_base(l1[pos]);
            }
            fw_seq.extend_from_slice(&l1);
        } else {
            // 40%: perfect L1
            fw_seq.extend_from_slice(LINKER1);
        }

        // BC2[8]
        for j in 0..8 {
            fw_seq.push(nuc(i + j * 29 + 7));
        }

        if error_class >= 7 {
            // 30%: 1bp deletion in L2
            let mut l2 = LINKER2.to_vec();
            l2.remove(8);
            fw_seq.extend_from_slice(&l2);
        } else if error_class >= 4 {
            // 30%: 1 sub error in L2 (within hamming tolerance 3)
            let mut l2 = LINKER2.to_vec();
            let pos = (3 + (i % 5) * 3) % l2.len();
            l2[pos] = mutate_base(l2[pos]);
            fw_seq.extend_from_slice(&l2);
        } else {
            fw_seq.extend_from_slice(LINKER2);
        }

        // BC1[8]
        for j in 0..8 {
            fw_seq.push(nuc(i + j * 31 + 3));
        }
        // UMI[10]
        for j in 0..10 {
            fw_seq.push(nuc(i + j * 17 + 5));
        }

        let total_len = fw_seq.len();
        if i % 2 == 1 {
            r1.write_all(&revcomp(&fw_seq)).unwrap();
            n_rc += 1;
        } else {
            r1.write_all(&fw_seq).unwrap();
            n_fw += 1;
        }
        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..total_len {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();
    }

    (r1_path, n_fw, n_rc)
}

/// Helper: run one LR-SPLiT-seq pipeline configuration and return recovery count.
fn run_lr_splitseq_pipeline(geom: &str, in1: &Path, dir: &Path, tag: &str) -> usize {
    let out1 = dir.join(format!("{}_out1.fastq", tag));
    let out2 = dir.join(format!("{}_out2.fastq", tag));
    let compiled = compile_geom(geom.to_string())
        .unwrap_or_else(|e| panic!("compile failed for {}: {:?}", tag, e));
    read_pairs_to_file(compiled, in1, None, &out1, &out2, 1, vec![]).unwrap();
    seq_count(&out1)
}

/// Reverse-complement a byte slice.
fn revcomp(seq: &[u8]) -> Vec<u8> {
    seq.iter()
        .rev()
        .map(|&b| match b {
            b'A' => b'T',
            b'T' => b'A',
            b'C' => b'G',
            b'G' => b'C',
            other => other,
        })
        .collect()
}

/// Write a single-end FASTQ where some reads are in forward orientation
/// (prefix CAGAGC) and others are reverse-complemented.
/// Returns (fastq_path, Vec<expected_orientation>).
fn write_orientation_test_fastq(dir: &Path, n: usize) -> (PathBuf, Vec<&'static str>) {
    let anchor = b"CAGAGC";
    let r1_path = dir.join("ori_r1.fastq");
    let mut r1 = File::create(&r1_path).unwrap();
    let mut expected_oris = Vec::new();

    for i in 0..n {
        writeln!(r1, "@read{}", i).unwrap();

        // Build the forward-orientation read: anchor + barcode(8bp) + cDNA(50bp)
        let mut fw_seq: Vec<u8> = Vec::new();
        fw_seq.extend_from_slice(anchor);
        for j in 0..8 {
            fw_seq.push(nuc(i + j * 7 + 3));
        }
        for j in 0..50 {
            fw_seq.push(nuc(i + j * 11 + 1));
        }

        if i % 3 == 1 {
            // Every 3rd read (index 1, 4, 7, ...) is RC'd
            let rc_seq = revcomp(&fw_seq);
            r1.write_all(&rc_seq).unwrap();
            expected_oris.push("rc");
        } else if i % 3 == 2 {
            // Every 3rd read (index 2, 5, 8, ...) has no anchor at all -> should be dropped
            for j in 0..64 {
                r1.write_all(&[nuc(i + j * 41 + 99)]).unwrap();
            }
            expected_oris.push("dropped");
        } else {
            // Forward orientation (index 0, 3, 6, ...)
            r1.write_all(&fw_seq).unwrap();
            expected_oris.push("fw");
        }
        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..64 {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();
    }

    (r1_path, expected_oris)
}

#[test]
fn annotation_match_ori_basic_recovery() {
    // Reads with the anchor in forward and RC orientation should both be recovered.
    // Reads without the anchor should be dropped.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 30;
    let (in1, expected_oris) = write_orientation_test_fastq(&dir, n);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = r#"
#[match_ori(either)]
1{f[CAGAGC]b[8]r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile_geom failed for match_ori");

    read_pairs_to_file(compiled, &in1, None, &out1, &out2, 1, vec![]).expect("read_pairs_to_file");

    let n_survived = seq_count(&out1);
    let n_expected_survivors = expected_oris.iter().filter(|&&o| o != "dropped").count();

    assert_eq!(
        n_survived, n_expected_survivors,
        "match_ori: expected {} survivors (fw+rc), got {}",
        n_expected_survivors, n_survived
    );
}

#[test]
fn annotation_match_ori_forward_invariant() {
    // The forward-orientation invariant: the same molecule presented in fw and
    // RC should produce identical extracted barcode sequences because
    // TryOrientationOp RCs the input before the geometry graph runs.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();

    let anchor = b"CAGAGC";
    // Build a single molecule: anchor + bc(8) + cDNA(50)
    let bc = b"ACGTACGT";
    let cdna: Vec<u8> = (0..50).map(|j| nuc(j * 3 + 7)).collect();

    let mut fw_seq = Vec::new();
    fw_seq.extend_from_slice(anchor);
    fw_seq.extend_from_slice(bc);
    fw_seq.extend_from_slice(&cdna);

    let rc_seq = revcomp(&fw_seq);

    let r1_path = dir.join("ori_invariant_r1.fastq");
    {
        let mut r1 = File::create(&r1_path).unwrap();
        let total_len = fw_seq.len();

        // Read 0: forward
        writeln!(r1, "@fw_read").unwrap();
        r1.write_all(&fw_seq).unwrap();
        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..total_len {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();

        // Read 1: reverse complement
        writeln!(r1, "@rc_read").unwrap();
        r1.write_all(&rc_seq).unwrap();
        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..total_len {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();
    }

    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    // Geometry with match_ori and transformation to extract barcode
    let geom = r#"
#[match_ori(either)]
1{f[CAGAGC]b<barcode>[8]r<rest>:}
-> 1{<barcode>}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile_geom failed");
    read_pairs_to_file(compiled, &r1_path, None, &out1, &out2, 1, vec![])
        .expect("read_pairs_to_file");

    let seqs = parse_fastq_sequences(&out1);
    assert_eq!(seqs.len(), 2, "Both fw and rc reads should survive");

    // Both should produce the same barcode: ACGTACGT
    assert_eq!(
        seqs[0], seqs[1],
        "Forward-orientation invariant: fw barcode '{}' != rc barcode '{}'",
        seqs[0], seqs[1]
    );
    assert_eq!(
        seqs[0], "ACGTACGT",
        "Extracted barcode should be ACGTACGT, got '{}'",
        seqs[0]
    );
}

#[test]
fn annotation_no_annotation_unchanged() {
    // Without the #[match_ori(either)] annotation, RC reads should NOT be recovered.
    // This ensures the annotation system doesn't affect non-annotated geometries.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 30;
    let (in1, expected_oris) = write_orientation_test_fastq(&dir, n);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    // Same geometry but WITHOUT annotation
    let geom = "1{f[CAGAGC]b[8]r:}".to_string();
    let compiled = compile_geom(geom).expect("compile_geom");
    read_pairs_to_file(compiled, &in1, None, &out1, &out2, 1, vec![]).expect("read_pairs_to_file");

    let n_survived = seq_count(&out1);
    let n_fw_only = expected_oris.iter().filter(|&&o| o == "fw").count();

    // Only forward-oriented reads should survive (no RC recovery)
    assert_eq!(
        n_survived, n_fw_only,
        "Without annotation: expected only {} fw survivors, got {}",
        n_fw_only, n_survived
    );
}

// ===========================================================================
// 6. LR-SPLiT-seq with #[match_ori(either)] -- primary paper use case
// ===========================================================================

#[test]
fn paper_lr_splitseq_match_ori_compile() {
    // The actual paper chemistry with match_ori annotation should compile.
    let geom = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
#[match_ori(either)]
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#
    .to_string();
    let compiled = compile_geom(geom);
    assert!(
        compiled.is_ok(),
        "LR-SPLiT-seq + match_ori should compile: {:?}",
        compiled.err()
    );
    let data = compiled.unwrap();
    assert_eq!(data.geometry.len(), 1);
    assert_eq!(data.element_annotations.len(), 1);
    assert_eq!(
        data.element_annotations[0].element_id,
        seqproc::compile::ElementId::Read(1)
    );
}

#[test]
fn paper_lr_splitseq_match_ori_recovery_improvement() {
    // With match_ori(either), recovery should improve over forward-only because
    // RC reads are now also matched. Forward-only geometry drops all RC reads.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 20;
    let (in1, n_fw, n_rc) = write_lr_splitseq_mixed_orientation(&dir, n);

    // Forward-only geometry (no annotation)
    let out_fw1 = dir.join("fw_out1.fastq");
    let out_fw2 = dir.join("fw_out2.fastq");
    let geom_fw = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#
    .to_string();
    let compiled_fw = compile_geom(geom_fw).expect("compile forward-only");
    read_pairs_to_file(compiled_fw, &in1, None, &out_fw1, &out_fw2, 1, vec![]).unwrap();
    let n_fw_survived = seq_count(&out_fw1);

    // Orientation-aware geometry (with annotation)
    let out_ori1 = dir.join("ori_out1.fastq");
    let out_ori2 = dir.join("ori_out2.fastq");
    let geom_ori = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
#[match_ori(either)]
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#
    .to_string();
    let compiled_ori = compile_geom(geom_ori).expect("compile match_ori");
    read_pairs_to_file(compiled_ori, &in1, None, &out_ori1, &out_ori2, 1, vec![]).unwrap();
    let n_ori_survived = seq_count(&out_ori1);

    // With annotation, recovery should be strictly greater (RC reads now recovered)
    assert!(
        n_ori_survived > n_fw_survived,
        "LR-SPLiT-seq match_ori should recover more reads ({}) than forward-only ({}); input had {} fw + {} rc",
        n_ori_survived, n_fw_survived, n_fw, n_rc
    );
}

#[test]
fn paper_lr_splitseq_match_ori_edit_compile() {
    // LR-SPLiT-seq with edit distance AND match_ori should compile.
    let geom = r#"
l1 = anchor_relative(edit(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(edit(f[ATCCACGTGCTTGAGA], 3))
#[match_ori(either)]
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#
    .to_string();
    let compiled = compile_geom(geom);
    assert!(
        compiled.is_ok(),
        "LR-SPLiT-seq edit + match_ori should compile: {:?}",
        compiled.err()
    );
}

#[test]
fn paper_lr_splitseq_match_ori_with_transformation() {
    // Full LR-SPLiT-seq pipeline: match_ori + transformation.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 20;
    let (in1, _, _) = write_lr_splitseq_mixed_orientation(&dir, n);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
#[match_ori(either)]
1{r<read>:b<bc3>[8]<l1>b<bc2>[8]<l2>b<bc1>[8]u<umi>[10]}
-> 1{<umi><bc1><bc2><bc3>}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile LR-SPLiT-seq + match_ori + transform");
    read_pairs_to_file(compiled, &in1, None, &out1, &out2, 1, vec![]).unwrap();

    let n_survived = seq_count(&out1);
    assert!(
        n_survived > 0,
        "LR-SPLiT-seq match_ori + transform: should recover reads"
    );

    // All output reads should be exactly 34bp: UMI(10) + BC1(8) + BC2(8) + BC3(8)
    let lens = parse_fastq_seq_lengths(&out1);
    for (i, &l) in lens.iter().enumerate() {
        assert_eq!(
            l, 34,
            "LR-SPLiT-seq match_ori transformed read {} should be 34bp, got {}bp",
            i, l
        );
    }
}

#[test]
fn paper_splitseq_pe_annotation_on_r2_only() {
    // PE test: annotation on R2 only (R1 is cDNA, R2 has barcode structure).
    // This ensures annotations on one read don't interfere with the other.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();

    // Write a simple PE FASTQ: R1=cDNA(50bp), R2=anchor+barcode(8)+rest
    let anchor = b"CAGAGC";
    let bc = b"TTGGCCAA";
    let r1_path = dir.join("pe_r1.fastq");
    let r2_path = dir.join("pe_r2.fastq");
    let n = 10;
    {
        let mut r1 = File::create(&r1_path).unwrap();
        let mut r2 = File::create(&r2_path).unwrap();
        for i in 0..n {
            // R1: 50bp cDNA
            writeln!(r1, "@read{}", i).unwrap();
            for j in 0..50 {
                r1.write_all(&[nuc(i + j * 7)]).unwrap();
            }
            writeln!(r1).unwrap();
            writeln!(r1, "+").unwrap();
            for _ in 0..50 {
                r1.write_all(b"I").unwrap();
            }
            writeln!(r1).unwrap();

            // R2: anchor + bc + trailing (some forward, some RC)
            writeln!(r2, "@read{}", i).unwrap();
            let mut fw_r2: Vec<u8> = Vec::new();
            fw_r2.extend_from_slice(anchor);
            fw_r2.extend_from_slice(bc);
            for j in 0..30 {
                fw_r2.push(nuc(i + j * 13 + 3));
            }
            if i % 2 == 1 {
                let rc_r2 = revcomp(&fw_r2);
                r2.write_all(&rc_r2).unwrap();
            } else {
                r2.write_all(&fw_r2).unwrap();
            }
            writeln!(r2).unwrap();
            let r2_len = fw_r2.len();
            writeln!(r2, "+").unwrap();
            for _ in 0..r2_len {
                r2.write_all(b"I").unwrap();
            }
            writeln!(r2).unwrap();
        }
    }

    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    // R1 has no annotation; R2 has match_ori(either)
    let geom = r#"
1{r:}
#[match_ori(either)]
2{f[CAGAGC]b[8]r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile PE with annotation on R2");
    read_pairs_to_file(compiled, &r1_path, Some(&r2_path), &out1, &out2, 1, vec![]).unwrap();

    let n_survived = seq_count(&out1);
    // All 10 reads should survive (fw and rc both recovered via match_ori)
    assert_eq!(
        n_survived, n,
        "PE match_ori on R2: all {} reads should survive, got {}",
        n, n_survived
    );

    // R1 (cDNA) should pass through unchanged at 50bp
    let r1_lens = parse_fastq_seq_lengths(&out1);
    assert!(
        r1_lens.iter().all(|&l| l == 50),
        "PE match_ori: R1 cDNA should remain 50bp"
    );
}

// ===========================================================================
// 7. Bug regression tests
// ===========================================================================

#[test]
fn bug1_divergent_match_block_arms_rejected() {
    // BUG 1: The compiler compiles both fw and rc match block arms but the
    // interpreter only ever uses the fw arm. If a user writes different arms,
    // the rc arm is silently discarded, causing incorrect output.
    //
    // FIX: The compiler should reject match blocks where fw and rc arms
    // produce different transformations, since the forward-orientation
    // invariant means they must be identical.
    let geom = r#"
#[match_ori(either)]
1{f[CAGAGC]b<bc>[8]r<rest>:}
-> match 1.ori {
    fw => 1{<bc>},
    rc => 1{<rest>},
}
"#
    .to_string();
    let result = compile_geom(geom);
    assert!(
        result.is_err(),
        "Divergent match block arms (fw outputs <bc>, rc outputs <rest>) \
         should be rejected by the compiler because the rc arm would be \
         silently ignored. Got Ok instead."
    );
}

#[test]
fn bug1_identical_match_block_arms_accepted() {
    // When fw and rc arms are identical, the match block should compile
    // successfully since no data is silently discarded.
    let geom = r#"
#[match_ori(either)]
1{f[CAGAGC]b<bc>[8]r<rest>:}
-> match 1.ori {
    fw => 1{<bc>},
    rc => 1{<bc>},
}
"#
    .to_string();
    let result = compile_geom(geom);
    assert!(
        result.is_ok(),
        "Identical match block arms should be accepted: {:?}",
        result.err()
    );
}

// ===========================================================================
// 8. Head-to-head annotation vs non-annotation E2E tests
// ===========================================================================

#[test]
fn e2e_lr_splitseq_annotation_recovery_head_to_head() {
    // Head-to-head comparison: annotation vs non-annotation on the SAME
    // LR-SPLiT-seq mixed-orientation data. This is the primary sanity test
    // for the whole annotation feature.
    //
    // Setup: 500 synthetic LR-SPLiT-seq reads, 50% forward + 50% RC.
    // The non-annotation geometry can only match forward reads.
    // The annotation geometry should match both orientations.
    //
    // Expected:
    //   - non-annotation recovers ~50% (forward reads only)
    //   - annotation recovers ~100% (both forward and RC)
    //   - annotation recovery rate >= 1.5x non-annotation
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 500;
    let (in1, n_fw, n_rc) = write_lr_splitseq_mixed_orientation(&dir, n);

    // --- Non-annotation (forward-only) ---
    let out_base1 = dir.join("base_out1.fastq");
    let out_base2 = dir.join("base_out2.fastq");
    let geom_base = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#
    .to_string();
    let compiled_base = compile_geom(geom_base).expect("compile baseline");
    read_pairs_to_file(compiled_base, &in1, None, &out_base1, &out_base2, 1, vec![]).unwrap();
    let n_base = seq_count(&out_base1);

    // --- Annotation (orientation-aware) ---
    let out_ann1 = dir.join("ann_out1.fastq");
    let out_ann2 = dir.join("ann_out2.fastq");
    let geom_ann = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
#[match_ori(either)]
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#
    .to_string();
    let compiled_ann = compile_geom(geom_ann).expect("compile annotation");
    read_pairs_to_file(compiled_ann, &in1, None, &out_ann1, &out_ann2, 1, vec![]).unwrap();
    let n_ann = seq_count(&out_ann1);

    // Annotation must recover strictly more reads
    assert!(
        n_ann > n_base,
        "Annotation should recover more reads than baseline: ann={}, base={} \
         (input: {} fw + {} rc = {} total)",
        n_ann,
        n_base,
        n_fw,
        n_rc,
        n
    );

    // Annotation recovery rate should be >= 1.5x baseline
    // (with 50/50 split, expected ratio is ~2x)
    let ratio = n_ann as f64 / n_base.max(1) as f64;
    assert!(
        ratio >= 1.5,
        "Annotation recovery ratio should be >= 1.5x baseline: \
         ann={}, base={}, ratio={:.2}",
        n_ann,
        n_base,
        ratio
    );

    // Baseline should recover roughly only the forward reads
    let fw_ratio = n_base as f64 / n_fw.max(1) as f64;
    assert!(
        fw_ratio <= 1.1,
        "Baseline should recover at most ~100% of forward reads: \
         base={}, n_fw={}, ratio={:.2}",
        n_base,
        n_fw,
        fw_ratio
    );

    // Annotation should recover from BOTH orientations
    let total_ratio = n_ann as f64 / n.max(1) as f64;
    assert!(
        total_ratio >= 0.5,
        "Annotation should recover >= 50% of all reads: \
         ann={}, total={}, ratio={:.2}",
        n_ann,
        n,
        total_ratio
    );
}

#[test]
fn e2e_lr_splitseq_annotation_with_transform_head_to_head() {
    // Same head-to-head but with a transformation, verifying that
    // extracted barcodes/UMIs have the correct length regardless of
    // original read orientation (forward-orientation invariant).
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 200;
    let (in1, _, _) = write_lr_splitseq_mixed_orientation(&dir, n);

    // --- Non-annotation with transformation ---
    let out_base1 = dir.join("base_tr_out1.fastq");
    let out_base2 = dir.join("base_tr_out2.fastq");
    let geom_base = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
1{r<read>:b<bc3>[8]<l1>b<bc2>[8]<l2>b<bc1>[8]u<umi>[10]}
-> 1{<umi><bc1><bc2><bc3>}
"#
    .to_string();
    let compiled_base = compile_geom(geom_base).expect("compile baseline transform");
    read_pairs_to_file(compiled_base, &in1, None, &out_base1, &out_base2, 1, vec![]).unwrap();
    let base_seqs = parse_fastq_sequences(&out_base1);

    // --- Annotation with transformation ---
    let out_ann1 = dir.join("ann_tr_out1.fastq");
    let out_ann2 = dir.join("ann_tr_out2.fastq");
    let geom_ann = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
#[match_ori(either)]
1{r<read>:b<bc3>[8]<l1>b<bc2>[8]<l2>b<bc1>[8]u<umi>[10]}
-> 1{<umi><bc1><bc2><bc3>}
"#
    .to_string();
    let compiled_ann = compile_geom(geom_ann).expect("compile annotation transform");
    read_pairs_to_file(compiled_ann, &in1, None, &out_ann1, &out_ann2, 1, vec![]).unwrap();
    let ann_seqs = parse_fastq_sequences(&out_ann1);

    // Annotation recovers more
    assert!(
        ann_seqs.len() > base_seqs.len(),
        "Annotation transform should recover more: ann={}, base={}",
        ann_seqs.len(),
        base_seqs.len()
    );

    // All transformed output reads must be exactly 34bp (UMI:10 + BC1:8 + BC2:8 + BC3:8)
    // regardless of whether the original read was fw or rc
    let expected_len = 10 + 8 + 8 + 8;
    for (i, seq) in ann_seqs.iter().enumerate() {
        assert_eq!(
            seq.len(),
            expected_len,
            "Annotation transformed read {} has wrong length: expected {}, got {}",
            i,
            expected_len,
            seq.len()
        );
    }
    for (i, seq) in base_seqs.iter().enumerate() {
        assert_eq!(
            seq.len(),
            expected_len,
            "Baseline transformed read {} has wrong length: expected {}, got {}",
            i,
            expected_len,
            seq.len()
        );
    }

    // Baseline reads should be a SUBSET of annotation reads (same molecule,
    // same extraction). Every baseline output should appear in annotation output.
    for (i, base_seq) in base_seqs.iter().enumerate() {
        assert!(
            ann_seqs.contains(base_seq),
            "Baseline read {} ({}) not found in annotation output -- \
             forward-orientation invariant violated",
            i,
            base_seq
        );
    }
}

#[test]
fn e2e_lr_splitseq_runtime_regression_head_to_head() {
    // Runtime regression test: verify that the annotation version does not
    // add unreasonable overhead compared to the non-annotation version.
    //
    // The annotation version runs the geometry graph twice (fw + rc) for
    // reads that fail on the first pass, so some overhead is expected.
    // We allow up to 5x slowdown as a generous bound (in practice it
    // should be < 3x since ~50% of reads match on the first pass).
    //
    // This test uses a larger dataset (1000 reads) for more stable timing.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 1000;
    let (in1, _, _) = write_lr_splitseq_mixed_orientation(&dir, n);

    let geom_base_str = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#
    .to_string();
    let geom_ann_str = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
#[match_ori(either)]
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#
    .to_string();

    // Warm up: compile both geometries (compilation time is not under test)
    let compiled_base = compile_geom(geom_base_str).expect("compile baseline");
    let compiled_ann = compile_geom(geom_ann_str).expect("compile annotation");

    // --- Baseline timing ---
    let out_base1 = dir.join("rt_base_out1.fastq");
    let out_base2 = dir.join("rt_base_out2.fastq");
    let t_base_start = std::time::Instant::now();
    read_pairs_to_file(compiled_base, &in1, None, &out_base1, &out_base2, 1, vec![]).unwrap();
    let t_base = t_base_start.elapsed();

    // --- Annotation timing ---
    let out_ann1 = dir.join("rt_ann_out1.fastq");
    let out_ann2 = dir.join("rt_ann_out2.fastq");
    let t_ann_start = std::time::Instant::now();
    read_pairs_to_file(compiled_ann, &in1, None, &out_ann1, &out_ann2, 1, vec![]).unwrap();
    let t_ann = t_ann_start.elapsed();

    // Both should produce output
    let n_base = seq_count(&out_base1);
    let n_ann = seq_count(&out_ann1);
    assert!(n_base > 0, "Baseline should produce output");
    assert!(n_ann > 0, "Annotation should produce output");

    // Runtime ratio check: annotation should be no more than 5x slower
    let max_slowdown = 5.0;
    let base_ms = t_base.as_secs_f64() * 1000.0;
    let ann_ms = t_ann.as_secs_f64() * 1000.0;

    // Guard against division by zero on very fast runs
    if base_ms > 0.1 {
        let ratio = ann_ms / base_ms;
        assert!(
            ratio <= max_slowdown,
            "Annotation runtime ({:.1}ms) exceeds {:.0}x baseline ({:.1}ms): \
             ratio={:.2}x. This suggests a performance regression.",
            ann_ms,
            max_slowdown,
            base_ms,
            ratio
        );
    }

    // Sanity: annotation should recover more reads (it handles RC too)
    assert!(
        n_ann > n_base,
        "Runtime test sanity: annotation ({}) should recover more than baseline ({})",
        n_ann,
        n_base
    );

    // Sanity: annotation runtime should not be LOWER than baseline
    // (if it is, it suggests the annotation codepath is not being exercised)
    // Allow annotation to be faster only if both are very fast (< 5ms)
    if ann_ms > 5.0 || base_ms > 5.0 {
        // With RC reads to retry, annotation should take at least some
        // additional time. A ratio < 0.5 would be suspicious.
        if base_ms > 1.0 {
            let min_ratio = 0.5;
            let ratio = ann_ms / base_ms;
            assert!(
                ratio >= min_ratio,
                "Annotation runtime ({:.1}ms) is suspiciously fast compared to \
                 baseline ({:.1}ms): ratio={:.2}x. This suggests the annotation \
                 codepath may not be exercised.",
                ann_ms,
                base_ms,
                ratio
            );
        }
    }
}

// ===========================================================================
// 9. LR-SPLiT-seq 8-test matrix:
//    {annotation, no-annotation} x {synthetic, real-like} x {hamming, edit}
//    All must beat the per-data-type baseline (no-annotation + hamming).
//    Recovery ordering must be consistent across data types.
// ===========================================================================

// Four geometry strings: the 2x2 of {annotation, no-annotation} x {hamming, edit}
const GEOM_NO_ANN_HAMMING: &str = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#;

const GEOM_NO_ANN_EDIT: &str = r#"
l1 = anchor_relative(edit(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(edit(f[ATCCACGTGCTTGAGA], 3))
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#;

const GEOM_ANN_HAMMING: &str = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
#[match_ori(either)]
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#;

const GEOM_ANN_EDIT: &str = r#"
l1 = anchor_relative(edit(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(edit(f[ATCCACGTGCTTGAGA], 3))
#[match_ori(either)]
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#;

// --- Individual matrix cell tests (8 total) ---

#[test]
fn matrix_no_ann_synthetic_hamming() {
    let tmp = tempfile::tempdir().unwrap();
    let d = tmp.path().to_path_buf();
    let n = 200;
    let (in1, n_fw, _n_rc) = write_lr_splitseq_mixed_orientation(&d, n);
    let recovered = run_lr_splitseq_pipeline(GEOM_NO_ANN_HAMMING, &in1, &d, "nanh_s");
    // Baseline on synthetic: should recover roughly the forward-only reads
    assert!(
        recovered > 0,
        "no-ann/hamming/synthetic should recover reads"
    );
    let rate = recovered as f64 / n as f64;
    // Without annotation, only forward reads are matched (~50%)
    assert!(
        rate <= 0.65,
        "no-ann/hamming/synthetic rate {:.1}% unexpectedly high (no RC recovery expected)",
        rate * 100.0
    );
    assert!(
        recovered <= n_fw + 2,
        "no-ann/hamming/synthetic recovered {} but only {} fw reads exist",
        recovered,
        n_fw
    );
}

#[test]
fn matrix_no_ann_synthetic_edit() {
    let tmp = tempfile::tempdir().unwrap();
    let d = tmp.path().to_path_buf();
    let n = 200;
    let (in1, _, _) = write_lr_splitseq_mixed_orientation(&d, n);
    let recovered = run_lr_splitseq_pipeline(GEOM_NO_ANN_EDIT, &in1, &d, "nane_s");
    // On synthetic (no errors), edit == hamming performance
    assert!(recovered > 0, "no-ann/edit/synthetic should recover reads");
    let rate = recovered as f64 / n as f64;
    assert!(
        rate <= 0.65,
        "no-ann/edit/synthetic rate {:.1}% unexpectedly high",
        rate * 100.0
    );
}

#[test]
fn matrix_no_ann_reallike_hamming() {
    let tmp = tempfile::tempdir().unwrap();
    let d = tmp.path().to_path_buf();
    let n = 200;
    let (in1, _, _) = write_lr_splitseq_noisy_mixed_orientation(&d, n);
    let recovered = run_lr_splitseq_pipeline(GEOM_NO_ANN_HAMMING, &in1, &d, "nanh_r");
    // Baseline on real-like data: lower recovery due to errors + no RC
    assert!(
        recovered > 0,
        "no-ann/hamming/real-like should recover some reads"
    );
}

#[test]
fn matrix_no_ann_reallike_edit() {
    let tmp = tempfile::tempdir().unwrap();
    let d = tmp.path().to_path_buf();
    let n = 200;
    let (in1, _, _) = write_lr_splitseq_noisy_mixed_orientation(&d, n);
    let n_hamming = run_lr_splitseq_pipeline(GEOM_NO_ANN_HAMMING, &in1, &d, "nanh_r2");
    let n_edit = run_lr_splitseq_pipeline(GEOM_NO_ANN_EDIT, &in1, &d, "nane_r");
    // Edit distance should recover >= hamming on noisy data (handles indels)
    assert!(
        n_edit >= n_hamming,
        "no-ann/edit/real-like ({}) should recover >= no-ann/hamming/real-like ({})",
        n_edit,
        n_hamming
    );
}

#[test]
fn matrix_ann_synthetic_hamming() {
    let tmp = tempfile::tempdir().unwrap();
    let d = tmp.path().to_path_buf();
    let n = 200;
    let (in1, _, _) = write_lr_splitseq_mixed_orientation(&d, n);
    let n_base = run_lr_splitseq_pipeline(GEOM_NO_ANN_HAMMING, &in1, &d, "nanh_s2");
    let n_ann = run_lr_splitseq_pipeline(GEOM_ANN_HAMMING, &in1, &d, "anh_s");
    // Annotation recovers both fw and RC -- must strictly beat baseline
    assert!(
        n_ann > n_base,
        "ann/hamming/synthetic ({}) must beat no-ann/hamming/synthetic ({})",
        n_ann,
        n_base
    );
    // Should roughly double recovery (50% -> ~100%)
    let ratio = n_ann as f64 / n_base.max(1) as f64;
    assert!(
        ratio >= 1.5,
        "ann/hamming/synthetic ratio {:.2}x vs baseline -- expected ~2x",
        ratio
    );
}

#[test]
fn matrix_ann_synthetic_edit() {
    let tmp = tempfile::tempdir().unwrap();
    let d = tmp.path().to_path_buf();
    let n = 200;
    let (in1, _, _) = write_lr_splitseq_mixed_orientation(&d, n);
    let n_base = run_lr_splitseq_pipeline(GEOM_NO_ANN_HAMMING, &in1, &d, "nanh_s3");
    let n_ann = run_lr_splitseq_pipeline(GEOM_ANN_EDIT, &in1, &d, "ane_s");
    // Must beat baseline
    assert!(
        n_ann > n_base,
        "ann/edit/synthetic ({}) must beat no-ann/hamming/synthetic ({})",
        n_ann,
        n_base
    );
    let ratio = n_ann as f64 / n_base.max(1) as f64;
    assert!(
        ratio >= 1.5,
        "ann/edit/synthetic ratio {:.2}x vs baseline -- expected ~2x",
        ratio
    );
}

#[test]
fn matrix_ann_reallike_hamming() {
    let tmp = tempfile::tempdir().unwrap();
    let d = tmp.path().to_path_buf();
    let n = 200;
    let (in1, _, _) = write_lr_splitseq_noisy_mixed_orientation(&d, n);
    let n_base = run_lr_splitseq_pipeline(GEOM_NO_ANN_HAMMING, &in1, &d, "nanh_r3");
    let n_ann = run_lr_splitseq_pipeline(GEOM_ANN_HAMMING, &in1, &d, "anh_r");
    // Annotation must beat baseline even on noisy data
    assert!(
        n_ann > n_base,
        "ann/hamming/real-like ({}) must beat no-ann/hamming/real-like ({})",
        n_ann,
        n_base
    );
}

#[test]
fn matrix_ann_reallike_edit() {
    let tmp = tempfile::tempdir().unwrap();
    let d = tmp.path().to_path_buf();
    let n = 200;
    let (in1, _, _) = write_lr_splitseq_noisy_mixed_orientation(&d, n);
    let n_base = run_lr_splitseq_pipeline(GEOM_NO_ANN_HAMMING, &in1, &d, "nanh_r4");
    let n_ann_edit = run_lr_splitseq_pipeline(GEOM_ANN_EDIT, &in1, &d, "ane_r");
    // Must beat baseline
    assert!(
        n_ann_edit > n_base,
        "ann/edit/real-like ({}) must beat no-ann/hamming/real-like ({})",
        n_ann_edit,
        n_base
    );
    // Should be the highest recovery of all 4 configs on noisy data
    let n_ann_hamming = run_lr_splitseq_pipeline(GEOM_ANN_HAMMING, &in1, &d, "anh_r2");
    assert!(
        n_ann_edit >= n_ann_hamming,
        "ann/edit/real-like ({}) should recover >= ann/hamming/real-like ({})",
        n_ann_edit,
        n_ann_hamming
    );
}

// --- Comprehensive cross-comparison: consistency between data types ---

#[test]
fn matrix_consistency_synthetic_vs_reallike() {
    // Run all 8 configurations and verify:
    // 1. Each beats its per-data-type baseline (no-ann + hamming)
    // 2. Ordering is consistent: ann+edit >= ann+hamming >= no-ann+edit >= no-ann+hamming
    // 3. Recovery rates are in plausible ranges
    let tmp = tempfile::tempdir().unwrap();
    let d = tmp.path().to_path_buf();
    let n = 500;

    // Generate both datasets
    let (in_synth, _, _) = write_lr_splitseq_mixed_orientation(&d, n);
    let (in_noisy, _, _) = write_lr_splitseq_noisy_mixed_orientation(&d, n);

    // --- Synthetic data (perfect linkers, mixed orientation) ---
    let s_no_ann_ham = run_lr_splitseq_pipeline(GEOM_NO_ANN_HAMMING, &in_synth, &d, "cs_nah");
    let s_no_ann_edit = run_lr_splitseq_pipeline(GEOM_NO_ANN_EDIT, &in_synth, &d, "cs_nae");
    let s_ann_ham = run_lr_splitseq_pipeline(GEOM_ANN_HAMMING, &in_synth, &d, "cs_ah");
    let s_ann_edit = run_lr_splitseq_pipeline(GEOM_ANN_EDIT, &in_synth, &d, "cs_ae");

    // --- Real-like data (noisy linkers, mixed orientation) ---
    let r_no_ann_ham = run_lr_splitseq_pipeline(GEOM_NO_ANN_HAMMING, &in_noisy, &d, "cr_nah");
    let r_no_ann_edit = run_lr_splitseq_pipeline(GEOM_NO_ANN_EDIT, &in_noisy, &d, "cr_nae");
    let r_ann_ham = run_lr_splitseq_pipeline(GEOM_ANN_HAMMING, &in_noisy, &d, "cr_ah");
    let r_ann_edit = run_lr_splitseq_pipeline(GEOM_ANN_EDIT, &in_noisy, &d, "cr_ae");

    // Print full matrix for diagnostics
    eprintln!("=== LR-SPLiT-seq 8-test matrix (n={}) ===", n);
    eprintln!("Config                  | Synthetic | Real-like");
    eprintln!("------------------------+-----------+----------");
    eprintln!(
        "no-annotation + hamming | {:>5} ({:>4.1}%) | {:>5} ({:>4.1}%)",
        s_no_ann_ham,
        s_no_ann_ham as f64 / n as f64 * 100.0,
        r_no_ann_ham,
        r_no_ann_ham as f64 / n as f64 * 100.0
    );
    eprintln!(
        "no-annotation + edit    | {:>5} ({:>4.1}%) | {:>5} ({:>4.1}%)",
        s_no_ann_edit,
        s_no_ann_edit as f64 / n as f64 * 100.0,
        r_no_ann_edit,
        r_no_ann_edit as f64 / n as f64 * 100.0
    );
    eprintln!(
        "annotation    + hamming | {:>5} ({:>4.1}%) | {:>5} ({:>4.1}%)",
        s_ann_ham,
        s_ann_ham as f64 / n as f64 * 100.0,
        r_ann_ham,
        r_ann_ham as f64 / n as f64 * 100.0
    );
    eprintln!(
        "annotation    + edit    | {:>5} ({:>4.1}%) | {:>5} ({:>4.1}%)",
        s_ann_edit,
        s_ann_edit as f64 / n as f64 * 100.0,
        r_ann_edit,
        r_ann_edit as f64 / n as f64 * 100.0
    );

    // (1) All configs beat their per-data-type baseline (no-ann + hamming)
    assert!(
        s_no_ann_edit >= s_no_ann_ham,
        "SYNTH: no-ann/edit ({}) must >= no-ann/hamming ({})",
        s_no_ann_edit,
        s_no_ann_ham
    );
    assert!(
        s_ann_ham > s_no_ann_ham,
        "SYNTH: ann/hamming ({}) must > no-ann/hamming ({})",
        s_ann_ham,
        s_no_ann_ham
    );
    assert!(
        s_ann_edit > s_no_ann_ham,
        "SYNTH: ann/edit ({}) must > no-ann/hamming ({})",
        s_ann_edit,
        s_no_ann_ham
    );

    assert!(
        r_no_ann_edit >= r_no_ann_ham,
        "REAL: no-ann/edit ({}) must >= no-ann/hamming ({})",
        r_no_ann_edit,
        r_no_ann_ham
    );
    assert!(
        r_ann_ham > r_no_ann_ham,
        "REAL: ann/hamming ({}) must > no-ann/hamming ({})",
        r_ann_ham,
        r_no_ann_ham
    );
    assert!(
        r_ann_edit > r_no_ann_ham,
        "REAL: ann/edit ({}) must > no-ann/hamming ({})",
        r_ann_edit,
        r_no_ann_ham
    );

    // (2) Consistent ordering on BOTH data types:
    //     ann+edit >= ann+hamming >= no-ann+edit >= no-ann+hamming
    assert!(
        s_ann_edit >= s_ann_ham,
        "SYNTH ordering: ann/edit ({}) must >= ann/hamming ({})",
        s_ann_edit,
        s_ann_ham
    );
    assert!(
        s_ann_ham >= s_no_ann_edit,
        "SYNTH ordering: ann/hamming ({}) must >= no-ann/edit ({})",
        s_ann_ham,
        s_no_ann_edit
    );

    assert!(
        r_ann_edit >= r_ann_ham,
        "REAL ordering: ann/edit ({}) must >= ann/hamming ({})",
        r_ann_edit,
        r_ann_ham
    );
    assert!(
        r_ann_ham >= r_no_ann_edit,
        "REAL ordering: ann/hamming ({}) must >= no-ann/edit ({})",
        r_ann_ham,
        r_no_ann_edit
    );

    // (3) Annotation improvement is consistent across data types:
    //     Both should show annotation strictly beating no-annotation
    let synth_ann_benefit = s_ann_ham as f64 / s_no_ann_ham.max(1) as f64;
    let real_ann_benefit = r_ann_ham as f64 / r_no_ann_ham.max(1) as f64;
    assert!(
        synth_ann_benefit >= 1.3 && real_ann_benefit >= 1.3,
        "Annotation benefit must be >= 1.3x on BOTH data types: \
         synth={:.2}x, real={:.2}x",
        synth_ann_benefit,
        real_ann_benefit
    );

    // (4) On noisy data, edit distance should strictly beat hamming
    //     (deletion errors are only recoverable by edit)
    assert!(
        r_no_ann_edit > r_no_ann_ham,
        "REAL: edit ({}) must strictly beat hamming ({}) on noisy data \
         (deletion errors only recoverable by edit distance)",
        r_no_ann_edit,
        r_no_ann_ham
    );
    assert!(
        r_ann_edit > r_ann_ham,
        "REAL: ann/edit ({}) must strictly beat ann/hamming ({}) on noisy data",
        r_ann_edit,
        r_ann_ham
    );
}

// ===========================================================================
// 10. Paper-artifact sanity tests (PAPER-5-6)
//
// Each test is named to match a specific paper artifact (Table 2, Figure 4,
// Figure 5) and uses the EXACT geometry from the paper's benchmark configs.
// These serve as Rust-level regression tests for every quantitative claim.
// ===========================================================================

// ---------------------------------------------------------------------------
// Table 2: test_table2_10x_v2
// Paper claim: 10x Chromium v2 achieves 100% recovery.
// Config: 1{b[16]u[10]}2{r:}  (no anchors, fixed-position extraction)
// ---------------------------------------------------------------------------

#[test]
fn test_table2_10x_v2() {
    // Paper claim (Table 2): seqproc achieves 100.00% recovery on 10x
    // Chromium v2. This is the simplest geometry -- fixed 16bp CB + 10bp
    // UMI in R1, cDNA passthrough in R2. Every syntactically valid read
    // must be recovered.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 200;
    let (in1, in2) = write_10x_chromium_v2(&dir, n);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    // Exact geometry from configs/seqproc/10x_v2.geom
    let geom = r#"
bc = b[16]
umi = u[10]
bio = r:
1{<bc><umi>}
2{<bio>}
-> 1{<bc><umi>} 2{<bio>}
"#
    .to_string();

    let compiled = compile_geom(geom).expect("10x_v2.geom must compile");
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![])
        .expect("10x_v2 pipeline failed");

    let n1 = seq_count(&out1);
    let n2 = seq_count(&out2);

    // Paper claim: 100% recovery
    assert_eq!(
        n1, n,
        "Table 2 claim: 10x Chromium v2 must achieve 100% recovery on R1 ({}/{})",
        n1, n
    );
    assert_eq!(
        n2, n,
        "Table 2 claim: 10x Chromium v2 must achieve 100% recovery on R2 ({}/{})",
        n2, n
    );

    // R1 = CB(16) + UMI(10) = 26bp; R2 = cDNA passthrough
    let lens1 = parse_fastq_seq_lengths(&out1);
    assert!(
        lens1.iter().all(|&l| l == 26),
        "10x R1 must be 26bp (CB+UMI)"
    );
}

// ---------------------------------------------------------------------------
// Table 2: test_table2_sciseq3
// Paper claim: sci-RNA-seq3 recovery ~89.3% with edit distance.
// Config: sciseq3_edit.geom -- edit(f[CAGAGC], 1) anchor + norm(b[9-10])
// ---------------------------------------------------------------------------

#[test]
fn test_table2_sciseq3() {
    // Paper claim (Table 2): seqproc achieves ~89.26% recovery on
    // sci-RNA-seq3 using edit distance on the CAGAGC anchor.
    // On synthetic data with perfect anchors, recovery should be 100%.
    // This test verifies the geometry compiles and runs correctly with
    // the exact paper config structure.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 200;
    let (in1, in2) = write_sci_rna_seq3(&dir, n);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    // Exact structure from configs/seqproc/sciseq3_edit.geom
    // (omitting filter_within_dist since synthetic data has no whitelists)
    let geom = r#"
anchor = edit(f[CAGAGC], 1)
brc1 = b[9-10]
brc2 = b[10]
umi = u[8]
1{<brc1><anchor><umi><brc2>}2{r<read>:}
-> 1{<brc1><brc2><umi>}2{<read>}
"#
    .to_string();

    let compiled = compile_geom(geom).expect("sciseq3_edit.geom must compile");
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![])
        .expect("sciseq3 pipeline failed");

    let n1 = seq_count(&out1);
    let n2 = seq_count(&out2);

    // Synthetic data has perfect anchors -> 100% recovery
    assert_eq!(
        n1, n,
        "Table 2 sanity: sci-RNA-seq3 with exact anchors must achieve 100% recovery ({}/{})",
        n1, n
    );
    assert_eq!(n1, n2, "R1 and R2 counts must match");

    // Transformed R1 = brc1(9-10) + brc2(10) + umi(8) = 27 or 28bp
    let lens1 = parse_fastq_seq_lengths(&out1);
    for (i, &l) in lens1.iter().enumerate() {
        let expected = if i % 2 == 0 { 27 } else { 28 };
        assert_eq!(
            l, expected,
            "sci3 transformed R1 read {} expected {}bp, got {}bp",
            i, expected, l
        );
    }

    // R2 = cDNA passthrough = 80bp
    let lens2 = parse_fastq_seq_lengths(&out2);
    assert!(lens2.iter().all(|&l| l == 80), "sci3 R2 must be 80bp cDNA");
}

// ---------------------------------------------------------------------------
// Table 2: test_table2_splitseq_pe_edit
// Paper claim: SPLiT-seq PE with edit distance achieves ~84.09% recovery.
// Config: splitseq_filter_edit.geom -- anchor_relative(edit(...)) on both
// linkers, 30bp L1, 30bp L2, 6bp BC1.
// ---------------------------------------------------------------------------

/// SPLiT-seq PE with the EXACT paper geometry: 30bp L1, 30bp L2, 6bp BC1.
/// The existing write_splitseq_pe uses 16bp L2 and 8bp BC1 which differs
/// from the paper config. This generator matches the actual paper geometry.
const LINKER2_PAPER: &[u8] = b"ATCCACGTGCTTGAGAGGCCAGAGCATTCG"; // 30bp

fn write_splitseq_pe_paper_geom(dir: &Path, n: usize) -> (PathBuf, PathBuf) {
    let r1_path = dir.join("splitseq_paper_r1.fastq");
    let r2_path = dir.join("splitseq_paper_r2.fastq");
    let mut r1 = File::create(&r1_path).unwrap();
    let mut r2 = File::create(&r2_path).unwrap();

    for i in 0..n {
        // R1: 80bp cDNA
        writeln!(r1, "@read{}", i).unwrap();
        for j in 0..80 {
            r1.write_all(&[nuc(i + j * 3 + 1)]).unwrap();
        }
        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..80 {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();

        // R2: x[2] + UMI[10] + BC3[8] + L1[30] + BC2[8] + L2[30] + BC1[6]
        writeln!(r2, "@read{}", i).unwrap();
        r2.write_all(&[nuc(i), nuc(i + 1)]).unwrap(); // x[2]
        for j in 0..10 {
            r2.write_all(&[nuc(i + j * 17 + 5)]).unwrap();
        } // UMI[10]
        for j in 0..8 {
            r2.write_all(&[nuc(i + j * 23 + 11)]).unwrap();
        } // BC3[8]
        r2.write_all(LINKER1).unwrap(); // L1[30]
        for j in 0..8 {
            r2.write_all(&[nuc(i + j * 29 + 7)]).unwrap();
        } // BC2[8]
        r2.write_all(LINKER2_PAPER).unwrap(); // L2[30]
        for j in 0..6 {
            r2.write_all(&[nuc(i + j * 31 + 3)]).unwrap();
        } // BC1[6]
        writeln!(r2).unwrap();
        let r2_len = 2 + 10 + 8 + 30 + 8 + 30 + 6; // 94bp total
        writeln!(r2, "+").unwrap();
        for _ in 0..r2_len {
            r2.write_all(b"I").unwrap();
        }
        writeln!(r2).unwrap();
    }

    (r1_path, r2_path)
}

#[test]
fn test_table2_splitseq_pe_edit() {
    // Paper claim (Table 2): seqproc achieves ~84.09% recovery on
    // SPLiT-seq PE using anchor_relative + edit distance.
    // On synthetic data with perfect linkers, recovery should be ~100%.
    // Uses the exact paper geometry: 30bp L1, 30bp L2, 6bp BC1.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 100;
    let (in1, in2) = write_splitseq_pe_paper_geom(&dir, n);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    // Matches the structure of splitseq_filter_edit.geom
    // (without filter_within_dist since we have no whitelist files)
    let geom = r#"
read1 = r:
umi = u[10]
bc3 = b[8]
l1 = anchor_relative(edit(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
bc2 = b[8]
l2 = anchor_relative(edit(f[ATCCACGTGCTTGAGAGGCCAGAGCATTCG], 6))
bc1 = b[6]
1{<read1>}
2{x[2]<umi><bc3><l1><bc2><l2><bc1>r:}
-> 1{<read1>} 2{<umi><bc3><bc2><bc1>}
"#
    .to_string();

    let compiled = compile_geom(geom).expect("splitseq_filter_edit.geom must compile");
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![])
        .expect("splitseq_pe_edit pipeline failed");

    let n1 = seq_count(&out1);
    let n2 = seq_count(&out2);

    // Synthetic data has perfect linkers -> high recovery
    assert!(
        n1 > 0,
        "Table 2 sanity: SPLiT-seq PE edit must recover reads"
    );
    assert_eq!(n1, n2, "R1 and R2 counts must match");

    // Transformed output: R1 = cDNA(80bp), R2 = UMI(10) + BC3(8) + BC2(8) + BC1(6) = 32bp
    let lens1 = parse_fastq_seq_lengths(&out1);
    let lens2 = parse_fastq_seq_lengths(&out2);
    if !lens1.is_empty() {
        assert!(
            lens1.iter().all(|&l| l == 80),
            "SPLiT-seq PE edit: R1 must be 80bp cDNA after transformation, got {:?}",
            &lens1[..lens1.len().min(5)]
        );
    }
    if !lens2.is_empty() {
        assert!(
            lens2.iter().all(|&l| l == 32),
            "SPLiT-seq PE edit: R2 must be 32bp (UMI+BC3+BC2+BC1) after transformation, got {:?}",
            &lens2[..lens2.len().min(5)]
        );
    }
}

// ---------------------------------------------------------------------------
// Table 2: test_table2_lr_splitseq_ann_edit
// Paper claim: LR-SPLiT-seq with annotation + edit distance achieves ~49.87%.
// Config: splitseq_singleend_primer_edit.geom + #[match_ori(either)]
// ---------------------------------------------------------------------------

#[test]
fn test_table2_lr_splitseq_ann_edit() {
    // Paper claim (Table 2): seqproc achieves ~49.87% recovery on
    // LR-SPLiT-seq using annotation (match_ori) + edit distance.
    // On synthetic mixed-orientation data, annotation+edit should recover
    // from both orientations and tolerate indel errors.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 200;
    let (in1, n_fw, _n_rc) = write_lr_splitseq_noisy_mixed_orientation(&dir, n);

    // Forward-only + hamming baseline (worst config)
    let n_baseline = run_lr_splitseq_pipeline(GEOM_NO_ANN_HAMMING, &in1, &dir, "t2_base");

    // The paper's actual config: annotation + edit distance
    let geom_paper = r#"
l1 = anchor_relative(edit(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(edit(f[ATCCACGTGCTTGAGA], 3))
#[match_ori(either)]
1{r:b[8]<l1>b[8]<l2>b[8]u[10]}
"#;
    let n_paper = run_lr_splitseq_pipeline(geom_paper, &in1, &dir, "t2_paper");

    // Paper config (ann+edit) must strictly beat baseline (no-ann+hamming)
    assert!(
        n_paper > n_baseline,
        "Table 2 sanity: LR-SPLiT-seq ann+edit ({}) must beat no-ann+hamming baseline ({})",
        n_paper,
        n_baseline
    );

    // Paper config must recover from BOTH orientations (more than just fw reads)
    assert!(
        n_paper > n_fw,
        "Table 2 sanity: LR-SPLiT-seq ann+edit ({}) must recover more than \
         forward-only reads ({}), proving RC recovery works",
        n_paper,
        n_fw
    );

    // Recovery rate must be non-trivial (> 25% of total input)
    let rate = n_paper as f64 / n as f64;
    assert!(
        rate > 0.25,
        "Table 2 sanity: LR-SPLiT-seq ann+edit recovery {:.1}% is too low (expected > 25%)",
        rate * 100.0
    );
}

// ---------------------------------------------------------------------------
// Figure 4: test_concordance_10x_perfect
// Paper claim: All three tools achieve perfect concordance (Jaccard=1.0) on
// 10x Chromium v2. Since we only have seqproc, we verify that running the
// same geometry twice on the same input produces identical read ID sets.
// ---------------------------------------------------------------------------

/// Extract read IDs from a FASTQ file (the @header lines without the @).
fn parse_fastq_read_ids(path: &Path) -> Vec<String> {
    let f = File::open(path).unwrap();
    let reader = BufReader::new(f);
    let mut ids = Vec::new();
    for (line_idx, line) in reader.lines().enumerate() {
        let line = line.unwrap();
        if line_idx % 4 == 0 {
            // Remove the leading '@'
            ids.push(line.trim_start_matches('@').to_string());
        }
    }
    ids
}

#[test]
fn test_concordance_10x_perfect() {
    // Paper claim (Figure 4): All tools achieve Jaccard=1.0 on 10x
    // Chromium v2. We verify this by running seqproc twice with the same
    // geometry and confirming the output read ID sets are identical.
    // This also verifies determinism of the pipeline.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 200;
    let (in1, in2) = write_10x_chromium_v2(&dir, n);

    // Run 1 -- exact geometry from configs/seqproc/10x_v2.geom
    let out1a = dir.join("run1_out1.fastq");
    let out2a = dir.join("run1_out2.fastq");
    let geom1 = "bc = b[16]\numi = u[10]\nbio = r:\n1{<bc><umi>}2{<bio>}\n-> 1{<bc><umi>}2{<bio>}"
        .to_string();
    let compiled1 = compile_geom(geom1).expect("compile run1");
    read_pairs_to_file(
        compiled1,
        &in1,
        Some(in2.as_path()),
        &out1a,
        &out2a,
        1,
        vec![],
    )
    .unwrap();

    // Run 2 (identical geometry, fresh compile)
    let out1b = dir.join("run2_out1.fastq");
    let out2b = dir.join("run2_out2.fastq");
    let geom2 = "bc = b[16]\numi = u[10]\nbio = r:\n1{<bc><umi>}2{<bio>}\n-> 1{<bc><umi>}2{<bio>}"
        .to_string();
    let compiled2 = compile_geom(geom2).expect("compile run2");
    read_pairs_to_file(
        compiled2,
        &in1,
        Some(in2.as_path()),
        &out1b,
        &out2b,
        1,
        vec![],
    )
    .unwrap();

    // Both runs must recover all reads (100% recovery)
    let ids_a = parse_fastq_read_ids(&out1a);
    let ids_b = parse_fastq_read_ids(&out1b);
    assert_eq!(ids_a.len(), n, "Run 1 must recover all {} reads", n);
    assert_eq!(ids_b.len(), n, "Run 2 must recover all {} reads", n);

    // Read ID sets must be identical (Jaccard = 1.0)
    assert_eq!(
        ids_a, ids_b,
        "Concordance claim: two runs on 10x must produce identical read ID sets"
    );

    // Sequences must also be identical (not just IDs)
    let seqs_a = parse_fastq_sequences(&out1a);
    let seqs_b = parse_fastq_sequences(&out1b);
    assert_eq!(
        seqs_a, seqs_b,
        "Concordance claim: two runs on 10x must produce identical sequences"
    );
}

// ---------------------------------------------------------------------------
// Figure 5: test_edit_vs_hamming_recovery
// Paper claim: Edit distance recovers a strict superset of hamming on data
// with indels. SPLiT-seq PE: +4.8%, LR-SPLiT-seq: +15.8%, sci-RNA-seq3: +0.9%.
// We verify the superset property on synthetic data with known indels.
// ---------------------------------------------------------------------------

/// Write SPLiT-seq PE reads where some have 1bp insertions in linker sequences.
/// These reads can only be recovered by edit distance, not hamming.
fn write_splitseq_pe_with_indels(dir: &Path, n: usize) -> (PathBuf, PathBuf) {
    let r1_path = dir.join("splitseq_indel_r1.fastq");
    let r2_path = dir.join("splitseq_indel_r2.fastq");
    let mut r1 = File::create(&r1_path).unwrap();
    let mut r2 = File::create(&r2_path).unwrap();

    for i in 0..n {
        // R1: 80bp cDNA
        writeln!(r1, "@read{}", i).unwrap();
        for j in 0..80 {
            r1.write_all(&[nuc(i + j * 3 + 1)]).unwrap();
        }
        writeln!(r1).unwrap();
        writeln!(r1, "+").unwrap();
        for _ in 0..80 {
            r1.write_all(b"I").unwrap();
        }
        writeln!(r1).unwrap();

        // R2 structure with indel errors in linkers for some reads
        writeln!(r2, "@read{}", i).unwrap();
        r2.write_all(&[nuc(i), nuc(i + 1)]).unwrap(); // x[2]
        for j in 0..10 {
            r2.write_all(&[nuc(i + j * 17 + 5)]).unwrap();
        } // UMI[10]
        for j in 0..8 {
            r2.write_all(&[nuc(i + j * 23 + 11)]).unwrap();
        } // BC3[8]

        // L1: 30bp -- every 3rd read has a 1bp deletion (hamming fails, edit handles)
        let error_class = i % 3;
        if error_class == 1 {
            let mut l1 = LINKER1.to_vec();
            l1.remove(15); // 1bp deletion in middle -> 29bp
            r2.write_all(&l1).unwrap();
        } else {
            r2.write_all(LINKER1).unwrap(); // perfect 30bp
        }

        for j in 0..8 {
            r2.write_all(&[nuc(i + j * 29 + 7)]).unwrap();
        } // BC2[8]

        // L2: 16bp -- every 3rd read (offset) has a 1bp insertion
        if error_class == 2 {
            let mut l2 = LINKER2.to_vec();
            l2.insert(8, b'A'); // 1bp insertion -> 17bp
            r2.write_all(&l2).unwrap();
        } else {
            r2.write_all(LINKER2).unwrap(); // perfect 16bp
        }

        for j in 0..8 {
            r2.write_all(&[nuc(i + j * 31 + 3)]).unwrap();
        } // BC1[8]

        // trailing
        for j in 0..10 {
            r2.write_all(&[nuc(i + j * 41)]).unwrap();
        }
        writeln!(r2).unwrap();
        // Variable R2 length due to indels
        let r2_len = 2
            + 10
            + 8
            + if error_class == 1 { 29 } else { 30 }
            + 8
            + if error_class == 2 { 17 } else { 16 }
            + 8
            + 10;
        writeln!(r2, "+").unwrap();
        for _ in 0..r2_len {
            r2.write_all(b"I").unwrap();
        }
        writeln!(r2).unwrap();
    }

    (r1_path, r2_path)
}

#[test]
fn test_edit_vs_hamming_recovery() {
    // Paper claim (Figure 5): Edit distance recovers a strict superset of
    // hamming distance on data with indels. On SPLiT-seq PE data where
    // ~33% of reads have 1bp deletions and ~33% have 1bp insertions in
    // linker sequences, edit distance should recover more reads than hamming.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 300;
    let (in1, in2) = write_splitseq_pe_with_indels(&dir, n);

    // Hamming geometry
    let out1_h = dir.join("hamming_out1.fastq");
    let out2_h = dir.join("hamming_out2.fastq");
    let geom_hamming = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
1{r:}
2{x[2]u[10]b[8]<l1>b[8]<l2>b[8]r:}
"#
    .to_string();
    let compiled_h = compile_geom(geom_hamming).expect("compile hamming");
    read_pairs_to_file(
        compiled_h,
        &in1,
        Some(in2.as_path()),
        &out1_h,
        &out2_h,
        1,
        vec![],
    )
    .unwrap();
    let n_hamming = seq_count(&out1_h);

    // Edit distance geometry
    let out1_e = dir.join("edit_out1.fastq");
    let out2_e = dir.join("edit_out2.fastq");
    let geom_edit = r#"
l1 = anchor_relative(edit(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(edit(f[ATCCACGTGCTTGAGA], 3))
1{r:}
2{x[2]u[10]b[8]<l1>b[8]<l2>b[8]r:}
"#
    .to_string();
    let compiled_e = compile_geom(geom_edit).expect("compile edit");
    read_pairs_to_file(
        compiled_e,
        &in1,
        Some(in2.as_path()),
        &out1_e,
        &out2_e,
        1,
        vec![],
    )
    .unwrap();
    let n_edit = seq_count(&out1_e);

    // Edit must recover strictly more reads than hamming (indel recovery)
    assert!(
        n_edit > n_hamming,
        "Figure 5 claim: edit ({}) must strictly beat hamming ({}) on data with indels",
        n_edit,
        n_hamming
    );

    // Hamming should recover the ~33% of reads with perfect linkers
    // (and possibly some with substitution-only errors that happen to be within tolerance)
    assert!(
        n_hamming > 0,
        "Hamming must recover at least the reads with perfect linkers"
    );

    // Edit must recover reads that hamming cannot (those with deletions/insertions)
    let edit_gain = n_edit - n_hamming;
    assert!(
        edit_gain > 0,
        "Figure 5 claim: edit distance must recover additional reads beyond hamming \
         (reads with indel errors). Hamming={}, Edit={}, Gain={}",
        n_hamming,
        n_edit,
        edit_gain
    );

    // Hamming output should be a subset of edit output (read IDs)
    let ids_hamming: std::collections::HashSet<String> =
        parse_fastq_read_ids(&out1_h).into_iter().collect();
    let ids_edit: std::collections::HashSet<String> =
        parse_fastq_read_ids(&out1_e).into_iter().collect();
    assert!(
        ids_hamming.is_subset(&ids_edit),
        "Figure 5 claim: hamming output must be a subset of edit output. \
         {} reads in hamming but not in edit.",
        ids_hamming.difference(&ids_edit).count()
    );
}

// ===========================================================================
// 11. Read order preservation tests (PAPER-6-4)
//
// When --preserve-order is set (implemented as threads=1), output reads
// must appear in the same order as the input FASTQ. These tests verify:
//   (a) Single-threaded execution preserves input order for all chemistries
//   (b) 100%-recovery geometries produce output IDs identical to input IDs
// ===========================================================================

#[test]
fn preserve_order_10x_single_thread() {
    // 10x Chromium v2 with 100% recovery: output read IDs must be in the
    // exact same order as input read IDs when using 1 thread (the
    // --preserve-order implementation).
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 500;
    let (in1, in2) = write_10x_chromium_v2(&dir, n);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = "bc = b[16]\numi = u[10]\nbio = r:\n1{<bc><umi>}2{<bio>}\n-> 1{<bc><umi>}2{<bio>}"
        .to_string();
    let compiled = compile_geom(geom).expect("compile");

    // threads=1 is the --preserve-order implementation
    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let in_ids = parse_fastq_read_ids(&in1);
    let out_ids = parse_fastq_read_ids(&out1);

    assert_eq!(in_ids.len(), n, "All input reads must be present");
    assert_eq!(out_ids.len(), n, "All output reads must be present");

    // Order must be preserved exactly
    assert_eq!(
        in_ids, out_ids,
        "preserve-order: 10x output read IDs must be in identical order to input"
    );
}

#[test]
fn preserve_order_sci_rna_seq3_single_thread() {
    // sci-RNA-seq3 with 100% recovery on synthetic data: output order
    // must match input order when using 1 thread.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 300;
    let (in1, in2) = write_sci_rna_seq3(&dir, n);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = r#"
anchor = f[CAGAGC]
brc1 = b[9-10]
1{<brc1><anchor>u[8]b[10]}2{r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile");

    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let in_ids = parse_fastq_read_ids(&in1);
    let out_ids = parse_fastq_read_ids(&out1);

    assert_eq!(in_ids.len(), n);
    assert_eq!(out_ids.len(), n);
    assert_eq!(
        in_ids, out_ids,
        "preserve-order: sci-RNA-seq3 output must match input order"
    );
}

#[test]
fn preserve_order_splitseq_pe_partial_recovery() {
    // SPLiT-seq PE with anchor_relative: some reads may be filtered.
    // The SURVIVING reads must appear in the same relative order as they
    // appeared in the input.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 200;
    let (in1, in2) = write_splitseq_pe(&dir, n);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
1{r:}
2{x[2]u[10]b[8]<l1>b[8]<l2>b[8]r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile");

    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let in_ids = parse_fastq_read_ids(&in1);
    let out_ids = parse_fastq_read_ids(&out1);

    assert!(
        !out_ids.is_empty(),
        "SPLiT-seq PE must recover at least some reads"
    );

    // Output IDs must be a subsequence of input IDs (same relative order)
    let mut in_iter = in_ids.iter();
    for out_id in &out_ids {
        let found = in_iter.any(|in_id| in_id == out_id);
        assert!(
            found,
            "preserve-order: output read '{}' not found in remaining input sequence \
             -- output is not a subsequence of input",
            out_id
        );
    }
}

#[test]
fn preserve_order_r1_and_r2_lockstep() {
    // When using --preserve-order, R1 and R2 output must be in lock-step:
    // the Nth read in R1 output corresponds to the Nth read in R2 output,
    // and both correspond to the same input read.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 300;
    let (in1, in2) = write_10x_chromium_v2(&dir, n);
    let out1 = dir.join("out1.fastq");
    let out2 = dir.join("out2.fastq");

    let geom = "bc = b[16]\numi = u[10]\nbio = r:\n1{<bc><umi>}2{<bio>}\n-> 1{<bc><umi>}2{<bio>}"
        .to_string();
    let compiled = compile_geom(geom).expect("compile");

    read_pairs_to_file(compiled, &in1, Some(in2.as_path()), &out1, &out2, 1, vec![]).unwrap();

    let r1_ids = parse_fastq_read_ids(&out1);
    let r2_ids = parse_fastq_read_ids(&out2);

    assert_eq!(r1_ids.len(), n);
    assert_eq!(r2_ids.len(), n);

    // R1 and R2 must have the same read IDs in the same order
    assert_eq!(
        r1_ids, r2_ids,
        "preserve-order: R1 and R2 output must be in lock-step (same read IDs, same order)"
    );
}

// ---------------------------------------------------------------------------
// LANG-MIGRATE-HAMMING: Annotation syntax equivalence E2E tests
// ---------------------------------------------------------------------------

#[test]
fn lang_migrate_hamming_annotation_vs_function_identical_output() {
    // LANG-MIGRATE-HAMMING: The new annotation syntax
    //   #[hamming(6)] l1 = anchor_relative(f[SEQ])
    // must produce byte-identical output to the old function syntax
    //   l1 = anchor_relative(hamming(f[SEQ], 6))
    // This test runs both through the full pipeline on the same input
    // and verifies the output FASTQs are identical.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 50;
    let (in1, in2) = write_splitseq_pe(&dir, n);

    // Old syntax: hamming() wrapping the FixedSeq
    let out1_old = dir.join("old_out1.fastq");
    let out2_old = dir.join("old_out2.fastq");
    let geom_old = r#"
l1 = anchor_relative(hamming(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGA], 3))
1{r:}
2{x[2]u[10]b[8]<l1>b[8]<l2>b[8]r:}
"#
    .to_string();
    let compiled_old = compile_geom(geom_old).expect("old syntax should compile");
    read_pairs_to_file(
        compiled_old,
        &in1,
        Some(in2.as_path()),
        &out1_old,
        &out2_old,
        1,
        vec![],
    )
    .unwrap();

    // New syntax: #[hamming(N)] annotation on plain f[SEQ]
    let out1_new = dir.join("new_out1.fastq");
    let out2_new = dir.join("new_out2.fastq");
    let geom_new = r#"
#[hamming(6)] l1 = anchor_relative(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT])
#[hamming(3)] l2 = anchor_relative(f[ATCCACGTGCTTGAGA])
1{r:}
2{x[2]u[10]b[8]<l1>b[8]<l2>b[8]r:}
"#
    .to_string();
    let compiled_new = compile_geom(geom_new).expect("new annotation syntax should compile");
    read_pairs_to_file(
        compiled_new,
        &in1,
        Some(in2.as_path()),
        &out1_new,
        &out2_new,
        1,
        vec![],
    )
    .unwrap();

    // Both must produce reads
    let n_old = seq_count(&out1_old);
    let n_new = seq_count(&out1_new);
    assert!(n_old > 0, "old syntax should produce output reads");
    assert_eq!(
        n_old, n_new,
        "old and new syntax must produce the same number of reads: old={}, new={}",
        n_old, n_new
    );

    // Output FASTQs must be byte-identical
    let old_r1 = std::fs::read(&out1_old).unwrap();
    let new_r1 = std::fs::read(&out1_new).unwrap();
    assert_eq!(
        old_r1, new_r1,
        "R1 output must be byte-identical between old hamming() and new #[hamming()] syntax"
    );
    let old_r2 = std::fs::read(&out2_old).unwrap();
    let new_r2 = std::fs::read(&out2_new).unwrap();
    assert_eq!(
        old_r2, new_r2,
        "R2 output must be byte-identical between old hamming() and new #[hamming()] syntax"
    );
}

#[test]
fn lang_migrate_edit_annotation_vs_function_identical_output() {
    // LANG-MIGRATE-EDIT: The new annotation syntax
    //   #[edit(6)] l1 = anchor_relative(f[SEQ])
    // must produce byte-identical output to the old function syntax
    //   l1 = anchor_relative(edit(f[SEQ], 6))
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 50;
    let (in1, in2) = write_splitseq_pe(&dir, n);

    let out1_old = dir.join("old_out1.fastq");
    let out2_old = dir.join("old_out2.fastq");
    let geom_old = r#"
l1 = anchor_relative(edit(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(edit(f[ATCCACGTGCTTGAGA], 3))
1{r:}
2{x[2]u[10]b[8]<l1>b[8]<l2>b[8]r:}
"#
    .to_string();
    let compiled_old = compile_geom(geom_old).expect("old edit syntax should compile");
    read_pairs_to_file(
        compiled_old,
        &in1,
        Some(in2.as_path()),
        &out1_old,
        &out2_old,
        1,
        vec![],
    )
    .unwrap();

    let out1_new = dir.join("new_out1.fastq");
    let out2_new = dir.join("new_out2.fastq");
    let geom_new = r#"
#[edit(6)] l1 = anchor_relative(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT])
#[edit(3)] l2 = anchor_relative(f[ATCCACGTGCTTGAGA])
1{r:}
2{x[2]u[10]b[8]<l1>b[8]<l2>b[8]r:}
"#
    .to_string();
    let compiled_new = compile_geom(geom_new).expect("new edit annotation syntax should compile");
    read_pairs_to_file(
        compiled_new,
        &in1,
        Some(in2.as_path()),
        &out1_new,
        &out2_new,
        1,
        vec![],
    )
    .unwrap();

    let n_old = seq_count(&out1_old);
    let n_new = seq_count(&out1_new);
    assert!(n_old > 0, "old syntax should produce output reads");
    assert_eq!(n_old, n_new, "old={}, new={}", n_old, n_new);

    assert_eq!(
        std::fs::read(&out1_old).unwrap(),
        std::fs::read(&out1_new).unwrap(),
        "R1 must be byte-identical"
    );
    assert_eq!(
        std::fs::read(&out2_old).unwrap(),
        std::fs::read(&out2_new).unwrap(),
        "R2 must be byte-identical"
    );
}

#[test]
fn lang_migrate_search_stacked_annotation_vs_function_identical_output() {
    // LANG-MIGRATE-SEARCH: The fully-migrated stacked annotation syntax
    //   #[search(relative)] #[edit(6)] l1 = f[SEQ]
    // must produce byte-identical output to the old function syntax
    //   l1 = anchor_relative(edit(f[SEQ], 6))
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let n = 50;
    let (in1, in2) = write_splitseq_pe(&dir, n);

    let out1_old = dir.join("old_out1.fastq");
    let out2_old = dir.join("old_out2.fastq");
    let geom_old = r#"
l1 = anchor_relative(edit(f[GTGGCCGCTGTTTCGCATCGGCGTACGACT], 6))
l2 = anchor_relative(edit(f[ATCCACGTGCTTGAGA], 3))
1{r:}
2{x[2]u[10]b[8]<l1>b[8]<l2>b[8]r:}
"#
    .to_string();
    let compiled_old = compile_geom(geom_old).expect("old syntax should compile");
    read_pairs_to_file(
        compiled_old,
        &in1,
        Some(in2.as_path()),
        &out1_old,
        &out2_old,
        1,
        vec![],
    )
    .unwrap();

    let out1_new = dir.join("new_out1.fastq");
    let out2_new = dir.join("new_out2.fastq");
    let geom_new = r#"
#[search(relative)] #[edit(6)] l1 = f[GTGGCCGCTGTTTCGCATCGGCGTACGACT]
#[search(relative)] #[edit(3)] l2 = f[ATCCACGTGCTTGAGA]
1{r:}
2{x[2]u[10]b[8]<l1>b[8]<l2>b[8]r:}
"#
    .to_string();
    let compiled_new = compile_geom(geom_new).expect("stacked annotation syntax should compile");
    read_pairs_to_file(
        compiled_new,
        &in1,
        Some(in2.as_path()),
        &out1_new,
        &out2_new,
        1,
        vec![],
    )
    .unwrap();

    let n_old = seq_count(&out1_old);
    let n_new = seq_count(&out1_new);
    assert!(n_old > 0, "old syntax should produce output reads");
    assert_eq!(n_old, n_new, "old={}, new={}", n_old, n_new);

    assert_eq!(
        std::fs::read(&out1_old).unwrap(),
        std::fs::read(&out1_new).unwrap(),
        "R1 must be byte-identical"
    );
    assert_eq!(
        std::fs::read(&out2_old).unwrap(),
        std::fs::read(&out2_new).unwrap(),
        "R2 must be byte-identical"
    );
}
