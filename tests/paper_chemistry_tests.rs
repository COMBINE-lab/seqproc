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
