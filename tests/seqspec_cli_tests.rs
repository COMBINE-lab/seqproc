use std::{fs, io::Write};

use assert_cmd::Command;
use predicates::prelude::*;

fn fixture(onlist_filename: &str) -> String {
    format!(
        r#"seqspec_version: 0.4.0
assay_id: cli-fixture
name: CLI fixture
modalities: [rna]
sequence_spec:
  - read_id: R1
    name: R1
    modality: rna
    primer_id: primer
    min_len: 4
    max_len: 4
    strand: pos
    files: []
library_spec:
  - region_id: rna
    region_type: rna
    sequence_type: joined
    min_len: 7
    max_len: 7
    regions:
      - region_id: primer
        region_type: primer
        sequence_type: fixed
        sequence: ACG
        min_len: 3
        max_len: 3
      - region_id: barcode
        region_type: barcode
        sequence_type: onlist
        min_len: 4
        max_len: 4
        onlist:
          file_id: whitelist
          filename: {onlist_filename}
          urltype: local
"#
    )
}

#[test]
fn import_writes_a_reviewable_atomic_bundle_with_verified_local_resource() {
    let directory = tempfile::tempdir().unwrap();
    let source = directory.path().join("protocol.yaml");
    fs::write(&source, fixture("whitelist.txt")).unwrap();
    fs::write(directory.path().join("whitelist.txt"), "ACGT\nTGCA\n").unwrap();
    let output = directory.path().join("imported");

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "import",
            "seqspec",
            source.to_str().unwrap(),
            "--output-dir",
            output.to_str().unwrap(),
            "--resources",
            "required",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("imported 1 geometry"));

    assert!(output.join("rna.geom").is_file());
    assert!(output.join("source.seqspec.yaml").is_file());
    assert!(output.join("import-report.json").is_file());
    let report: serde_json::Value =
        serde_json::from_slice(&fs::read(output.join("import-report.json")).unwrap()).unwrap();
    assert_eq!(report["resources"][0]["status"], "resolved");
    assert!(report["resources"][0]["stored_blake3"]
        .as_str()
        .unwrap()
        .starts_with("blake3:"));
    assert_eq!(
        report["resources"][0]["stored_blake3"],
        report["resources"][0]["content_blake3"]
    );
}

#[test]
fn compressed_resource_checks_stored_size_and_uncompressed_seqspec_md5() {
    use md5::{Digest, Md5};

    let directory = tempfile::tempdir().unwrap();
    let content = b"ACGT\nTGCA\n";
    let mut encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::new(3));
    encoder.write_all(content).unwrap();
    let compressed = encoder.finish().unwrap();
    fs::write(directory.path().join("whitelist.txt.gz"), &compressed).unwrap();
    let content_md5 = format!("{:x}", Md5::digest(content));
    let yaml = fixture("whitelist.txt.gz").replace(
        "          urltype: local",
        &format!(
            "          urltype: local\n          filesize: {}\n          md5: {content_md5}",
            compressed.len()
        ),
    );
    let source = directory.path().join("protocol.yaml");
    fs::write(&source, yaml).unwrap();
    let output = directory.path().join("imported");

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "import",
            "seqspec",
            source.to_str().unwrap(),
            "--output-dir",
            output.to_str().unwrap(),
            "--resources",
            "required",
        ])
        .assert()
        .success();

    let report: serde_json::Value =
        serde_json::from_slice(&fs::read(output.join("import-report.json")).unwrap()).unwrap();
    let resource = &report["resources"][0];
    assert_eq!(resource["stored_byte_count"], compressed.len());
    assert_eq!(resource["content_byte_count"], content.len());
    assert_eq!(resource["content_md5"], content_md5);
    assert_ne!(resource["stored_blake3"], resource["content_blake3"]);
}

#[test]
fn check_only_is_network_free_and_reports_blockers_without_writing() {
    let directory = tempfile::tempdir().unwrap();
    let source = directory.path().join("protocol.yaml");
    fs::write(&source, fixture("missing.txt")).unwrap();
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "import",
            "seqspec",
            source.to_str().unwrap(),
            "--check-only",
            "--resources",
            "required",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("supported"));
    assert_eq!(fs::read_dir(directory.path()).unwrap().count(), 1);
}

#[test]
fn required_resource_policy_fails_before_publishing_a_partial_directory() {
    let directory = tempfile::tempdir().unwrap();
    let source = directory.path().join("protocol.yaml");
    fs::write(&source, fixture("missing.txt")).unwrap();
    let output = directory.path().join("imported");
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "import",
            "seqspec",
            source.to_str().unwrap(),
            "--output-dir",
            output.to_str().unwrap(),
            "--resources",
            "required",
        ])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("could not be resolved"));
    assert!(!output.exists());
}

#[test]
fn allow_partial_does_not_override_a_document_level_version_error() {
    let directory = tempfile::tempdir().unwrap();
    let source = directory.path().join("protocol.yaml");
    fs::write(&source, fixture("whitelist.txt").replace("0.4.0", "1.0.0")).unwrap();
    fs::write(directory.path().join("whitelist.txt"), "ACGT\n").unwrap();
    let output = directory.path().join("imported");
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "import",
            "seqspec",
            source.to_str().unwrap(),
            "--output-dir",
            output.to_str().unwrap(),
            "--allow-partial",
        ])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("import is blocked"));
    assert!(!output.exists());
}

#[test]
fn multi_modality_bundle_uses_collision_safe_geometry_and_resource_paths() {
    let directory = tempfile::tempdir().unwrap();
    fs::write(directory.path().join("first.txt"), "AAAA\n").unwrap();
    fs::write(directory.path().join("second.txt"), "CCCC\n").unwrap();
    let source = directory.path().join("protocol.yaml");
    fs::write(
        &source,
        r#"seqspec_version: 0.4.0
assay_id: collisions
name: collision fixture
modalities: [rna/a, rna-a]
sequence_spec:
  - read_id: R1
    name: first
    modality: rna/a
    primer_id: p1
    min_len: 4
    max_len: 4
    strand: pos
    files: []
  - read_id: R2
    name: second
    modality: rna-a
    primer_id: p2
    min_len: 4
    max_len: 4
    strand: pos
    files: []
library_spec:
  - region_id: rna/a
    region_type: rna
    sequence_type: joined
    min_len: 5
    max_len: 5
    regions:
      - { region_id: p1, region_type: primer, sequence_type: fixed, sequence: A, min_len: 1, max_len: 1 }
      - region_id: barcode
        region_type: barcode
        sequence_type: onlist
        min_len: 4
        max_len: 4
        onlist: { file_id: first, filename: first.txt, urltype: local }
  - region_id: rna-a
    region_type: rna
    sequence_type: joined
    min_len: 5
    max_len: 5
    regions:
      - { region_id: p2, region_type: primer, sequence_type: fixed, sequence: C, min_len: 1, max_len: 1 }
      - region_id: barcode
        region_type: barcode
        sequence_type: onlist
        min_len: 4
        max_len: 4
        onlist: { file_id: second, filename: second.txt, urltype: local }
"#,
    )
    .unwrap();
    let output = directory.path().join("imported");
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "import",
            "seqspec",
            source.to_str().unwrap(),
            "--output-dir",
            output.to_str().unwrap(),
            "--resources",
            "required",
        ])
        .assert()
        .success();

    assert!(output.join("rna-a.geom").is_file());
    assert!(output.join("rna-a-2.geom").is_file());
    let report: serde_json::Value =
        serde_json::from_slice(&fs::read(output.join("import-report.json")).unwrap()).unwrap();
    let first_path = report["resources"][0]["output_path"].as_str().unwrap();
    let second_path = report["resources"][1]["output_path"].as_str().unwrap();
    assert_ne!(first_path, second_path);
    assert_ne!(
        fs::read(output.join(first_path)).unwrap(),
        fs::read(output.join(second_path)).unwrap()
    );
}
