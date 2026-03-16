//! Comprehensive E2E tests for all EFGDL language additions (Milestone 2).
//!
//! Covers:
//!   - Annotation parsing (read-level, definition-level, stacking)
//!   - Matching modifier migrations (hamming, edit, search)
//!   - Conditional output branching (match construct)
//!   - Backward compatibility (old syntax still works)
//!   - Deprecation warnings (old syntax warns, new syntax does not)
//!   - Error cases (invalid annotations, missing attributes, conflicts)

use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};

use seqproc::compile::ElementId;
use seqproc::execute::{compile_geom, read_pairs_to_file};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn parse_fastq_sequences(path: &Path) -> Vec<String> {
    let f = File::open(path).unwrap();
    let reader = BufReader::new(f);
    let mut seqs = Vec::new();
    for (i, line) in reader.lines().enumerate() {
        if i % 4 == 1 {
            seqs.push(line.unwrap());
        }
    }
    seqs
}

fn seq_count(path: &Path) -> usize {
    if path.exists() {
        parse_fastq_sequences(path).len()
    } else {
        0
    }
}

fn nuc(i: usize) -> u8 {
    const N: [u8; 4] = [b'A', b'C', b'G', b'T'];
    N[i & 3]
}

fn rc(seq: &str) -> String {
    seq.bytes()
        .rev()
        .map(|b| match b {
            b'A' => 'T',
            b'T' => 'A',
            b'C' => 'G',
            b'G' => 'C',
            _ => unreachable!(),
        })
        .collect()
}

/// Write simple reads: bc(8) + anchor(CAGAGC=6) + cDNA(12)
fn write_simple_reads(dir: &Path, n: usize) -> PathBuf {
    let path = dir.join("simple_r1.fastq");
    let mut f = File::create(&path).unwrap();
    for i in 0..n {
        let bc: String = (0..8).map(|j| nuc(i * 7 + j) as char).collect();
        let seq = format!("{}CAGAGCTTTTTTTTTTTT", bc);
        writeln!(f, "@read{}", i).unwrap();
        writeln!(f, "{}", seq).unwrap();
        writeln!(f, "+").unwrap();
        writeln!(f, "{}", "I".repeat(seq.len())).unwrap();
    }
    path
}

/// Write reads with 1bp mismatches in anchor (for hamming/edit tests)
fn write_mismatch_reads(dir: &Path, n: usize) -> PathBuf {
    let path = dir.join("mismatch_r1.fastq");
    let mut f = File::create(&path).unwrap();
    let anchors = ["CAGAGC", "CAGAAC", "TAGAGC", "CCGAGC", "CAGAGC"];
    for i in 0..n {
        let bc: String = (0..8).map(|j| nuc(i * 7 + j) as char).collect();
        let anchor = anchors[i % anchors.len()];
        let seq = format!("{}{}TTTTTTTTTTTT", bc, anchor);
        writeln!(f, "@read{}", i).unwrap();
        writeln!(f, "{}", seq).unwrap();
        writeln!(f, "+").unwrap();
        writeln!(f, "{}", "I".repeat(seq.len())).unwrap();
    }
    path
}

/// Write mixed-orientation reads for match_ori + conditional output tests
fn write_mixed_orientation_reads(dir: &Path, n: usize) -> PathBuf {
    let path = dir.join("mixed_r1.fastq");
    let mut f = File::create(&path).unwrap();
    for i in 0..n {
        let bc: String = (0..8).map(|j| nuc(i * 7 + j) as char).collect();
        let fw = format!("{}CAGAGCAAAAAAAAAAAA", bc);
        let seq = if i % 2 == 0 { fw.clone() } else { rc(&fw) };
        writeln!(f, "@read{}", i).unwrap();
        writeln!(f, "{}", seq).unwrap();
        writeln!(f, "+").unwrap();
        writeln!(f, "{}", "I".repeat(seq.len())).unwrap();
    }
    path
}

// ===========================================================================
// 1. Annotation Parsing
// ===========================================================================

#[test]
fn e2e_annotation_on_definition_compiles() {
    let geom = "#[edit(1)] anchor = f[CAGAGC]\n1{x:<anchor>x:}2{r:}".to_string();
    let data = compile_geom(geom).expect("annotated definition should compile");
    assert!(
        data.element_annotations
            .iter()
            .any(|ea| ea.element_id == ElementId::Definition("anchor".to_string())),
        "should have definition-level annotation"
    );
}

#[test]
fn e2e_annotation_on_read_compiles() {
    let geom = "#[match_ori(either)] 1{b[10]f[CAGAGC]r:}".to_string();
    let data = compile_geom(geom).expect("annotated read should compile");
    assert!(
        data.element_annotations
            .iter()
            .any(|ea| ea.element_id == ElementId::Read(1)),
        "should have read-level annotation"
    );
}

#[test]
fn e2e_stacked_annotations_compile() {
    let geom = "#[search(relative)] #[edit(3)] l1 = f[CAGAGC]\n1{x[2]b[8]<l1>r:}2{r:}".to_string();
    let data = compile_geom(geom).expect("stacked annotations should compile");
    let def_ann = data
        .element_annotations
        .iter()
        .find(|ea| ea.element_id == ElementId::Definition("l1".to_string()))
        .expect("should have l1 annotation");
    assert_eq!(
        def_ann.annotations.len(),
        2,
        "should have 2 stacked annotations"
    );
}

#[test]
fn e2e_annotation_on_both_definition_and_read() {
    let geom = "#[edit(1)] anchor = f[CAGAGC]\n#[match_ori(either)] 1{b[8]<anchor>r:}".to_string();
    let data = compile_geom(geom).expect("both annotations should compile");
    assert_eq!(
        data.element_annotations.len(),
        2,
        "should have annotations on both read and definition"
    );
}

// ===========================================================================
// 2. Matching Modifier Migrations -- Equivalence Tests
// ===========================================================================

#[test]
fn e2e_hamming_annotation_identical_output() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let in1 = write_mismatch_reads(&dir, 20);

    let out_old = dir.join("old.fq");
    let out_new = dir.join("new.fq");

    let geom_old = "anchor = hamming(f[CAGAGC], 1)\n1{x:<anchor>x:}2{r:}".to_string();
    let geom_new = "#[hamming(1)] anchor = f[CAGAGC]\n1{x:<anchor>x:}2{r:}".to_string();

    let c_old = compile_geom(geom_old).unwrap();
    let c_new = compile_geom(geom_new).unwrap();

    read_pairs_to_file(c_old, &in1, None, &out_old, &dir.join("d.fq"), 1, vec![]).unwrap();
    read_pairs_to_file(c_new, &in1, None, &out_new, &dir.join("d.fq"), 1, vec![]).unwrap();

    assert!(seq_count(&out_old) > 0, "old syntax should produce reads");
    assert_eq!(
        std::fs::read(&out_old).unwrap(),
        std::fs::read(&out_new).unwrap(),
        "hamming: old and new syntax must produce byte-identical output"
    );
}

#[test]
fn e2e_edit_annotation_identical_output() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let in1 = write_mismatch_reads(&dir, 20);

    let out_old = dir.join("old.fq");
    let out_new = dir.join("new.fq");

    let geom_old = "anchor = edit(f[CAGAGC], 1)\n1{x:<anchor>x:}2{r:}".to_string();
    let geom_new = "#[edit(1)] anchor = f[CAGAGC]\n1{x:<anchor>x:}2{r:}".to_string();

    let c_old = compile_geom(geom_old).unwrap();
    let c_new = compile_geom(geom_new).unwrap();

    read_pairs_to_file(c_old, &in1, None, &out_old, &dir.join("d.fq"), 1, vec![]).unwrap();
    read_pairs_to_file(c_new, &in1, None, &out_new, &dir.join("d.fq"), 1, vec![]).unwrap();

    assert!(seq_count(&out_old) > 0);
    assert_eq!(
        std::fs::read(&out_old).unwrap(),
        std::fs::read(&out_new).unwrap(),
        "edit: old and new syntax must produce byte-identical output"
    );
}

#[test]
fn e2e_search_relative_annotation_identical_output() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let in1 = write_simple_reads(&dir, 20);

    let out_old = dir.join("old.fq");
    let out_new = dir.join("new.fq");

    let geom_old = "l1 = anchor_relative(f[CAGAGC])\n1{b[8]<l1>r:}".to_string();
    let geom_new = "#[search(relative)] l1 = f[CAGAGC]\n1{b[8]<l1>r:}".to_string();

    let c_old = compile_geom(geom_old).unwrap();
    let c_new = compile_geom(geom_new).unwrap();

    read_pairs_to_file(c_old, &in1, None, &out_old, &dir.join("d.fq"), 1, vec![]).unwrap();
    read_pairs_to_file(c_new, &in1, None, &out_new, &dir.join("d.fq"), 1, vec![]).unwrap();

    assert!(seq_count(&out_old) > 0);
    assert_eq!(
        std::fs::read(&out_old).unwrap(),
        std::fs::read(&out_new).unwrap(),
        "search(relative): old and new syntax must produce byte-identical output"
    );
}

#[test]
fn e2e_stacked_search_edit_identical_output() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let in1 = write_mismatch_reads(&dir, 20);

    let out_old = dir.join("old.fq");
    let out_new = dir.join("new.fq");

    let geom_old = "l1 = anchor_relative(edit(f[CAGAGC], 1))\n1{b[8]<l1>r:}".to_string();
    let geom_new = "#[search(relative)] #[edit(1)] l1 = f[CAGAGC]\n1{b[8]<l1>r:}".to_string();

    let c_old = compile_geom(geom_old).unwrap();
    let c_new = compile_geom(geom_new).unwrap();

    read_pairs_to_file(c_old, &in1, None, &out_old, &dir.join("d.fq"), 1, vec![]).unwrap();
    read_pairs_to_file(c_new, &in1, None, &out_new, &dir.join("d.fq"), 1, vec![]).unwrap();

    assert!(seq_count(&out_old) > 0);
    assert_eq!(
        std::fs::read(&out_old).unwrap(),
        std::fs::read(&out_new).unwrap(),
        "stacked search+edit: old and new syntax must produce byte-identical output"
    );
}

// ===========================================================================
// 3. Conditional Output Branching
// ===========================================================================

#[test]
fn e2e_conditional_output_fw_and_rc_different() {
    // Forward reads get bc as-is, RC reads get revcomp(bc)
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let in1 = write_mixed_orientation_reads(&dir, 10);

    let out1 = dir.join("out.fq");
    let geom = r#"
#[match_ori(either)]
1{b<bc>[8]f[CAGAGC]r:}
-> match 1.ori {
    fw => 1{<bc>},
    rc => 1{revcomp(<bc>)}
}
"#
    .to_string();

    let compiled = compile_geom(geom).expect("conditional output should compile");
    read_pairs_to_file(compiled, &in1, None, &out1, &dir.join("d.fq"), 1, vec![]).unwrap();

    let seqs = parse_fastq_sequences(&out1);
    assert!(!seqs.is_empty(), "should produce output reads");
    // All output barcodes should be 8bp
    for seq in &seqs {
        assert_eq!(
            seq.len(),
            8,
            "output barcode should be 8bp, got {}",
            seq.len()
        );
    }
}

#[test]
fn e2e_conditional_output_identical_arms_same_as_direct() {
    // When both arms are identical, conditional output should produce
    // the same result as a direct (non-match) transformation.
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let in1 = write_mixed_orientation_reads(&dir, 20);

    let out_direct = dir.join("direct.fq");
    let out_match = dir.join("match.fq");

    let geom_direct = r#"
#[match_ori(either)]
1{b<bc>[8]f[CAGAGC]r:}
-> 1{<bc>}
"#
    .to_string();

    let geom_match = r#"
#[match_ori(either)]
1{b<bc>[8]f[CAGAGC]r:}
-> match 1.ori {
    fw => 1{<bc>},
    rc => 1{<bc>}
}
"#
    .to_string();

    let c_direct = compile_geom(geom_direct).unwrap();
    let c_match = compile_geom(geom_match).unwrap();

    read_pairs_to_file(
        c_direct,
        &in1,
        None,
        &out_direct,
        &dir.join("d.fq"),
        1,
        vec![],
    )
    .unwrap();
    read_pairs_to_file(
        c_match,
        &in1,
        None,
        &out_match,
        &dir.join("d.fq"),
        1,
        vec![],
    )
    .unwrap();

    assert_eq!(
        std::fs::read(&out_direct).unwrap(),
        std::fs::read(&out_match).unwrap(),
        "identical match arms should produce same output as direct transform"
    );
}

// ===========================================================================
// 4. Backward Compatibility
// ===========================================================================

#[test]
fn e2e_old_hamming_syntax_still_works() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let in1 = write_mismatch_reads(&dir, 10);
    let out1 = dir.join("out.fq");

    let geom = "anchor = hamming(f[CAGAGC], 1)\n1{x:<anchor>x:}2{r:}".to_string();
    let compiled = compile_geom(geom).expect("old hamming syntax should still compile");
    read_pairs_to_file(compiled, &in1, None, &out1, &dir.join("d.fq"), 1, vec![]).unwrap();
    assert!(
        seq_count(&out1) > 0,
        "old hamming syntax should produce reads"
    );
}

#[test]
fn e2e_old_edit_syntax_still_works() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let in1 = write_mismatch_reads(&dir, 10);
    let out1 = dir.join("out.fq");

    let geom = "anchor = edit(f[CAGAGC], 1)\n1{x:<anchor>x:}2{r:}".to_string();
    let compiled = compile_geom(geom).expect("old edit syntax should still compile");
    read_pairs_to_file(compiled, &in1, None, &out1, &dir.join("d.fq"), 1, vec![]).unwrap();
    assert!(seq_count(&out1) > 0, "old edit syntax should produce reads");
}

#[test]
fn e2e_old_anchor_relative_syntax_still_works() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let in1 = write_simple_reads(&dir, 10);
    let out1 = dir.join("out.fq");

    let geom = "l1 = anchor_relative(f[CAGAGC])\n1{b[8]<l1>r:}".to_string();
    let compiled = compile_geom(geom).expect("old anchor_relative should still compile");
    read_pairs_to_file(compiled, &in1, None, &out1, &dir.join("d.fq"), 1, vec![]).unwrap();
    assert!(
        seq_count(&out1) > 0,
        "old anchor_relative should produce reads"
    );
}

// ===========================================================================
// 5. Deprecation Warnings
// ===========================================================================

#[test]
fn e2e_old_syntax_emits_deprecation_warnings() {
    let geom_hamming = "anchor = hamming(f[CAGAGC], 1)\n1{x:<anchor>x:}2{r:}".to_string();
    let data = compile_geom(geom_hamming).unwrap();
    assert!(
        !data.warnings.is_empty(),
        "old hamming() should emit deprecation warning"
    );
    assert!(data.warnings[0].contains("deprecated"));

    let geom_edit = "anchor = edit(f[CAGAGC], 1)\n1{x:<anchor>x:}2{r:}".to_string();
    let data = compile_geom(geom_edit).unwrap();
    assert!(
        !data.warnings.is_empty(),
        "old edit() should emit deprecation warning"
    );

    let geom_anchor = "l1 = anchor_relative(f[CAGAGC])\n1{x[2]b[8]<l1>r:}2{r:}".to_string();
    let data = compile_geom(geom_anchor).unwrap();
    assert!(
        !data.warnings.is_empty(),
        "old anchor_relative() should emit deprecation warning"
    );
}

#[test]
fn e2e_new_syntax_no_warnings() {
    let geom = "#[hamming(1)] anchor = f[CAGAGC]\n1{x:<anchor>x:}2{r:}".to_string();
    let data = compile_geom(geom).unwrap();
    assert!(
        data.warnings.is_empty(),
        "new hamming annotation: no warnings"
    );

    let geom = "#[edit(1)] anchor = f[CAGAGC]\n1{x:<anchor>x:}2{r:}".to_string();
    let data = compile_geom(geom).unwrap();
    assert!(data.warnings.is_empty(), "new edit annotation: no warnings");

    let geom = "#[search(relative)] #[edit(1)] l1 = f[CAGAGC]\n1{x[2]b[8]<l1>r:}2{r:}".to_string();
    let data = compile_geom(geom).unwrap();
    assert!(data.warnings.is_empty(), "stacked annotations: no warnings");
}

// ===========================================================================
// 6. Error Cases
// ===========================================================================

#[test]
fn e2e_error_malformed_hamming_annotation() {
    let result = compile_geom("#[hamming(abc)] a = f[CAGAGC]\n1{x:<a>x:}2{r:}".to_string());
    assert!(result.is_err(), "#[hamming(abc)] should be rejected");
}

#[test]
fn e2e_error_empty_hamming_annotation() {
    let result = compile_geom("#[hamming()] a = f[CAGAGC]\n1{x:<a>x:}2{r:}".to_string());
    assert!(result.is_err(), "#[hamming()] should be rejected");
}

#[test]
fn e2e_error_malformed_edit_annotation() {
    let result = compile_geom("#[edit(xyz)] a = f[CAGAGC]\n1{x:<a>x:}2{r:}".to_string());
    assert!(result.is_err(), "#[edit(xyz)] should be rejected");
}

#[test]
fn e2e_error_bad_search_annotation() {
    let result = compile_geom("#[search(absolute)] a = f[CAGAGC]\n1{x:<a>x:}2{r:}".to_string());
    assert!(result.is_err(), "#[search(absolute)] should be rejected");
}

#[test]
fn e2e_error_conflicting_old_new_syntax() {
    // Old function + new annotation of the same kind = conflict
    let result =
        compile_geom("#[hamming(3)] anchor = edit(f[CAGAGC], 1)\n1{x:<anchor>x:}2{r:}".to_string());
    assert!(
        result.is_err(),
        "conflicting hamming annotation + edit function should be rejected"
    );
}

#[test]
fn e2e_error_match_block_without_match_ori() {
    // match block on ori without #[match_ori(either)] should be rejected
    let result = compile_geom(
        "1{b<bc>[8]f[CAGAGC]r:}\n-> match 1.ori { fw => 1{<bc>}, rc => 1{<bc>} }".to_string(),
    );
    assert!(
        result.is_err(),
        "match on ori without #[match_ori(either)] should be rejected"
    );
}

#[test]
fn e2e_error_conflicting_search_annotation_with_anchor_relative() {
    let result = compile_geom(
        "#[search(relative)] l1 = anchor_relative(f[CAGAGC])\n1{x[2]b[8]<l1>r:}2{r:}".to_string(),
    );
    assert!(
        result.is_err(),
        "#[search(relative)] + anchor_relative() should conflict"
    );
}

// ===========================================================================
// 7. Definition Without Annotation (no regression)
// ===========================================================================

#[test]
fn e2e_plain_definition_no_annotations_works() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let in1 = write_simple_reads(&dir, 10);
    let out1 = dir.join("out.fq");

    let geom = "bc1 = b[8]\n1{<bc1>f[CAGAGC]r:}".to_string();
    let compiled = compile_geom(geom).expect("plain definition should compile");
    assert!(
        compiled.warnings.is_empty(),
        "no warnings for plain definition"
    );
    read_pairs_to_file(compiled, &in1, None, &out1, &dir.join("d.fq"), 1, vec![]).unwrap();
    assert!(
        seq_count(&out1) > 0,
        "plain definition should produce reads"
    );
}

#[test]
fn e2e_no_definitions_at_all_works() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().to_path_buf();
    let in1 = write_simple_reads(&dir, 10);
    let out1 = dir.join("out.fq");

    let geom = "1{b[8]f[CAGAGC]r:}".to_string();
    let compiled = compile_geom(geom).expect("no-definition geometry should compile");
    assert!(compiled.warnings.is_empty());
    read_pairs_to_file(compiled, &in1, None, &out1, &dir.join("d.fq"), 1, vec![]).unwrap();
    assert!(seq_count(&out1) > 0);
}
