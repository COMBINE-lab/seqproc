use assert_cmd::Command;
use serde_json::Value;
use std::fs;
use std::io::Read;
use tempfile::tempdir;

fn fixture(path: &str) -> String {
    format!("{}/tests/{path}", env!("CARGO_MANIFEST_DIR"))
}

fn gzip_copy(source: &str, destination: &std::path::Path) {
    use std::io::Write;
    let mut encoder = flate2::write::GzEncoder::new(
        fs::File::create(destination).unwrap(),
        flate2::Compression::new(3),
    );
    encoder.write_all(&fs::read(source).unwrap()).unwrap();
    encoder.finish().unwrap();
}

fn assert_current_summary_shape(report: &Value) {
    let schema: Value =
        serde_json::from_str(include_str!("../schemas/seqproc-summary-1.4.0.schema.json")).unwrap();
    assert_eq!(
        report["schema_version"],
        schema["properties"]["schema_version"]["const"]
    );
    let properties = schema["properties"].as_object().unwrap();
    for key in report.as_object().unwrap().keys() {
        assert!(
            properties.contains_key(key),
            "summary field {key:?} is absent from schema 1.4.0"
        );
    }
    for required in schema["required"].as_array().unwrap() {
        let required = required.as_str().unwrap();
        assert!(
            report.get(required).is_some(),
            "required summary field {required:?} is absent"
        );
    }
}

#[test]
fn validate_and_explain_commands_report_compiled_geometry() {
    let geometry = fixture("fgdl/match.geom");
    let validate = Command::cargo_bin("seqproc")
        .unwrap()
        .args(["validate", &geometry])
        .assert()
        .success();
    assert!(String::from_utf8_lossy(&validate.get_output().stdout).contains("valid:"));

    let explain = Command::cargo_bin("seqproc")
        .unwrap()
        .args(["explain", &geometry])
        .assert()
        .success();
    let stdout = String::from_utf8_lossy(&explain.get_output().stdout);
    assert!(stdout.contains("Normalized EFGDL:"));
    assert!(stdout.contains("Compiled geometry:"));
}

#[test]
fn explain_handles_parameterized_ambiguity_policy() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("ambiguity.geom");
    fs::write(
        &geometry,
        "#[ambig_policy = random(seed = 42)] bc = map_with_mismatch(b[4], \"mapping.tsv\", self, 1)\n1{<bc>r:}\n",
    )
    .unwrap();

    let explain = Command::cargo_bin("seqproc")
        .unwrap()
        .args(["explain", geometry.to_str().unwrap()])
        .assert()
        .success();
    let stdout = String::from_utf8_lossy(&explain.get_output().stdout);
    assert!(stdout.contains("Normalized EFGDL:"));
    assert!(stdout.contains("Random"));
    assert!(stdout.contains("seed: 42"));
}

#[test]
fn validate_returns_nonzero_for_invalid_geometry() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("invalid.geom");
    fs::write(&geometry, "1{b[16]r:@}\n").unwrap();

    let invalid = Command::cargo_bin("seqproc")
        .unwrap()
        .args(["validate", geometry.to_str().unwrap()])
        .assert()
        .failure();
    let mut diagnostic = invalid.get_output().stdout.clone();
    diagnostic.extend_from_slice(&invalid.get_output().stderr);
    assert!(String::from_utf8_lossy(&diagnostic).contains("found '@'"));
}

#[test]
fn summary_mode_uses_the_same_processing_pipeline() {
    let directory = tempdir().unwrap();
    let geometry = fixture("fgdl/match.geom");
    let input1 = fixture("test_data/in/match/match_l.fastq");
    let input2 = fixture("test_data/in/match/match_r.fastq");
    let normal1 = directory.path().join("normal-r1.fastq");
    let normal2 = directory.path().join("normal-r2.fastq");
    let summary1 = directory.path().join("summary-r1.fastq");
    let summary2 = directory.path().join("summary-r2.fastq");
    let summary = directory.path().join("summary.json");
    let basic1 = directory.path().join("basic-r1.fastq");
    let basic2 = directory.path().join("basic-r2.fastq");
    let basic_summary = directory.path().join("basic-summary.json");

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            &geometry,
            "--file1",
            &input1,
            "--file2",
            &input2,
            "--out1",
            normal1.to_str().unwrap(),
            "--out2",
            normal2.to_str().unwrap(),
            "--threads",
            "2",
            "--preserve-order",
            "--pipeline-input-mode",
            "dedicated-reader",
            "--queue-capacity",
            "1",
            "--max-in-flight-batches",
            "2",
        ])
        .assert()
        .success();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            &geometry,
            "--file1",
            &input1,
            "--file2",
            &input2,
            "--out1",
            summary1.to_str().unwrap(),
            "--out2",
            summary2.to_str().unwrap(),
            "--threads",
            "2",
            "--preserve-order",
            "--pipeline-input-mode",
            "dedicated-reader",
            "--queue-capacity",
            "1",
            "--max-in-flight-batches",
            "2",
            "--summary",
            summary.to_str().unwrap(),
        ])
        .assert()
        .success();

    assert_eq!(fs::read(&normal1).unwrap(), fs::read(summary1).unwrap());
    assert_eq!(fs::read(&normal2).unwrap(), fs::read(summary2).unwrap());

    let report: Value = serde_json::from_slice(&fs::read(summary).unwrap()).unwrap();
    assert_current_summary_shape(&report);
    assert_eq!(report["schema_version"], "1.4.0");
    assert_eq!(report["statistics_level"], "detailed");
    assert_eq!(report["gzip_compression_level"], 3);
    assert_eq!(report["parallel_gzip_members"], false);
    assert_eq!(report["parallel_gzip_stream"], false);
    assert_eq!(report["gzip_compression_threads"], 1);
    assert_eq!(report["gzip_block_size"], 128 * 1024);
    assert_eq!(report["gzip_input_backend"], "needletail-auto");
    assert_eq!(report["gzip_input_threads"], 1);
    assert_eq!(report["gzip_input_chunk_size"], 0);
    assert_eq!(report["effective_threads"], 2);
    assert_eq!(report["ordering_mode"], "input-order");
    assert_eq!(report["graph_optimization"]["enabled"], true);
    assert_eq!(report["execution_plan"]["requested_mode"], "auto");
    assert_eq!(
        report["execution_plan"]["backend"],
        "dedicated_reader_pipeline"
    );
    assert_eq!(
        report["execution_plan"]["pipeline"]["input_mode"],
        "dedicated_reader"
    );
    assert_eq!(
        report["execution_plan"]["reason_codes"][0],
        "ordered_output_requires_pipeline"
    );
    let geometry_digest = report["geometry_digest"].as_str().unwrap();
    assert!(geometry_digest.starts_with("blake3:"));
    assert_eq!(geometry_digest.len(), "blake3:".len() + 64);
    assert_eq!(report["n_processed"], 3);
    assert_eq!(report["failed_parsing"], 0);

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            &geometry,
            "--file1",
            &input1,
            "--file2",
            &input2,
            "--out1",
            basic1.to_str().unwrap(),
            "--out2",
            basic2.to_str().unwrap(),
            "--threads",
            "2",
            "--statistics-level",
            "basic",
            "--summary",
            basic_summary.to_str().unwrap(),
        ])
        .assert()
        .success();

    assert_eq!(fs::read(normal1).unwrap(), fs::read(basic1).unwrap());
    assert_eq!(fs::read(normal2).unwrap(), fs::read(basic2).unwrap());
    let basic: Value = serde_json::from_slice(&fs::read(basic_summary).unwrap()).unwrap();
    assert_eq!(basic["statistics_level"], "basic");
    assert_eq!(basic["n_processed"], 3);
    assert_eq!(basic["match_distance_stats"], serde_json::json!([]));
    assert_eq!(basic["read_length_mean"], serde_json::json!([]));
    assert_eq!(basic["read_length_min"], serde_json::json!([]));
    assert_eq!(basic["read_length_max"], serde_json::json!([]));
}

#[test]
fn efgdl_two_constructs_fixed_output_bases_and_qualities() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("fixed-output.geom");
    let input = directory.path().join("input.fastq");
    let output = directory.path().join("output.fastq");
    fs::write(
        &geometry,
        "header { efgdl = 2 }\n1{b<bc>[2]r<rest>:} -> 1{f[TT]<bc>f[A]<rest>}\n",
    )
    .unwrap();
    fs::write(&input, "@read1\nACGT\n+\n1234\n").unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--file1",
            input.to_str().unwrap(),
            "--out1",
            output.to_str().unwrap(),
        ])
        .assert()
        .success();

    assert_eq!(
        fs::read_to_string(output).unwrap(),
        "@read1\nTTACAGT\n+\nII12I34\n"
    );
}

#[test]
fn efgdl_two_modifies_fastq_headers_with_captured_labels() {
    let directory = tempdir().unwrap();
    let input = directory.path().join("input.fastq");
    fs::write(&input, "@read1\nACGT\n+\n1234\n").unwrap();

    for (mode, template, expected_name) in [
        ("append", "\" CB:Z:\", <bc>", "read1 CB:Z:AC"),
        ("prepend", "\"sample:\", <bc>, \" \"", "sample:AC read1"),
        ("replace", "\"new:\", <bc>", "new:AC"),
    ] {
        let geometry = directory.path().join(format!("header-{mode}.geom"));
        let output = directory.path().join(format!("header-{mode}.fastq"));
        fs::write(
            &geometry,
            format!(
                "header {{ efgdl = 2 }}\n1{{b<bc>[2]r<rest>:}} -> #[header = {mode}({template})] 1{{<rest>}}\n"
            ),
        )
        .unwrap();

        Command::cargo_bin("seqproc")
            .unwrap()
            .args([
                "run",
                "--geom",
                geometry.to_str().unwrap(),
                "--file1",
                input.to_str().unwrap(),
                "--out1",
                output.to_str().unwrap(),
            ])
            .assert()
            .success();

        assert_eq!(
            fs::read_to_string(output).unwrap(),
            format!("@{expected_name}\nGT\n+\n34\n")
        );
    }
}

#[test]
fn paired_output_headers_capture_labels_before_terminal_projection() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("paired-headers.geom");
    let input1 = directory.path().join("input_R1.fastq");
    let input2 = directory.path().join("input_R2.fastq");
    let output1 = directory.path().join("output_R1.fastq");
    let output2 = directory.path().join("output_R2.fastq");

    fs::write(
        &geometry,
        r#"header { efgdl = 2 }
1{b<bc>[2]u<umi>[2]}
2{r<bio>:}
-> #[header = append(" CB:Z:", <bc>, " UB:Z:", <umi>)] 1{f[TT]<bc><umi>}
   #[header = append(" CB:Z:", <bc>, " UB:Z:", <umi>)] 2{<bio>}
"#,
    )
    .unwrap();
    fs::write(&input1, "@pair/1\nACGT\n+\n1234\n").unwrap();
    fs::write(&input2, "@pair/2\nGGA\n+\n567\n").unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--file1",
            input1.to_str().unwrap(),
            "--file2",
            input2.to_str().unwrap(),
            "--out1",
            output1.to_str().unwrap(),
            "--out2",
            output2.to_str().unwrap(),
        ])
        .assert()
        .success();

    assert_eq!(
        fs::read_to_string(output1).unwrap(),
        "@pair/1 CB:Z:AC UB:Z:GT\nTTACGT\n+\nII1234\n"
    );
    assert_eq!(
        fs::read_to_string(output2).unwrap(),
        "@pair/2 CB:Z:AC UB:Z:GT\nGGA\n+\n567\n"
    );
}

#[test]
fn detailed_summary_reports_ambiguity_outcomes() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("ambiguity.geom");
    let whitelist = directory.path().join("whitelist.txt");
    let input = directory.path().join("ambiguous.fastq");
    let output = directory.path().join("output.fastq");
    let summary = directory.path().join("summary.json");

    fs::write(&whitelist, "CAAA\nCAAA\nAAAC\n").unwrap();
    fs::write(&input, "@ambiguous\nAAAA\n+\nIIII\n").unwrap();
    fs::write(
        &geometry,
        format!(
            "#[ambig_policy = no_match] bc = filter_within_dist(b[4], \"{}\", 1)\n1{{<bc>}}\n-> 1{{<bc>}}\n",
            whitelist.display()
        ),
    )
    .unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--file1",
            input.to_str().unwrap(),
            "--out1",
            output.to_str().unwrap(),
            "--summary",
            summary.to_str().unwrap(),
        ])
        .assert()
        .success();

    let report: Value = serde_json::from_slice(&fs::read(summary).unwrap()).unwrap();
    let stage = &report["match_distance_stats"][0];
    assert_eq!(stage["stage_index"], 0);
    assert_eq!(stage["attempted"], 1);
    assert_eq!(stage["matched"], 0);
    assert_eq!(stage["unmatched"], 1);
    assert_eq!(stage["ambiguity"]["total"], 1);
    assert_eq!(stage["ambiguity"]["dropped"], 1);
}

#[test]
fn unassigned_routing_and_rejection_counts_are_per_read() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("mixed.geom");
    let input = directory.path().join("mixed.fastq");
    let accepted = directory.path().join("accepted.fastq");
    let rejected = directory.path().join("rejected.fastq");
    let summary = directory.path().join("summary.json");
    fs::write(
        &geometry,
        "anchor = f[AAAA]\n1{<anchor>r:}\n-> 1{<anchor>}\n",
    )
    .unwrap();
    fs::write(
        &input,
        b"@accepted\nAAAACC\n+\nIIIIII\n@rejected\nTTTTCC\n+\nIIIIII\n",
    )
    .unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--file1",
            input.to_str().unwrap(),
            "--out1",
            accepted.to_str().unwrap(),
            "--unassigned1",
            rejected.to_str().unwrap(),
            "--threads",
            "2",
            "--summary",
            summary.to_str().unwrap(),
        ])
        .assert()
        .success();

    let accepted_fastq = fs::read_to_string(accepted).unwrap();
    let rejected_fastq = fs::read_to_string(rejected).unwrap();
    assert!(accepted_fastq.contains("@accepted"));
    assert!(!accepted_fastq.contains("@rejected"));
    assert!(rejected_fastq.contains("@rejected"));
    assert!(!rejected_fastq.contains("@accepted"));

    let report: Value = serde_json::from_slice(&fs::read(summary).unwrap()).unwrap();
    assert_eq!(report["total_fragments"], 2);
    assert_eq!(report["accepted_fragments"], 1);
    assert_eq!(report["rejected_fragments"], 1);
    assert_eq!(report["failed_parsing"], 1);
}

#[test]
fn summary_counts_fragments_omitted_by_primary_output() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("filtered.geom");
    let input = directory.path().join("filtered.fastq");
    let output = directory.path().join("output.fastq");
    let summary = directory.path().join("summary.json");
    fs::write(
        &geometry,
        "anchor = f[AAAA]\n1{<anchor>r:}\n-> 1{<anchor>}\n",
    )
    .unwrap();
    fs::write(
        &input,
        b"@accepted\nAAAACC\n+\nIIIIII\n@filtered\nTTTTCC\n+\nIIIIII\n",
    )
    .unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--file1",
            input.to_str().unwrap(),
            "--out1",
            output.to_str().unwrap(),
            "--threads",
            "2",
            "--staged-pipeline",
            "--summary",
            summary.to_str().unwrap(),
        ])
        .assert()
        .success();

    assert_eq!(fs::read_to_string(output).unwrap().lines().count(), 4);
    let report: Value = serde_json::from_slice(&fs::read(summary).unwrap()).unwrap();
    assert_eq!(report["total_fragments"], 2);
    assert_eq!(report["accepted_fragments"], 1);
    assert_eq!(report["rejected_fragments"], 1);
    assert_eq!(report["failed_parsing"], 0);
    assert_eq!(
        report["rejection_reasons"][0]["reason"],
        "not_emitted_by_primary_output"
    );
}

#[test]
fn direct_terminal_rendering_matches_materialized_fastq() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("direct-render.geom");
    let input = directory.path().join("input.fastq");
    let direct = directory.path().join("direct.fastq");
    let materialized = directory.path().join("materialized.fastq");
    fs::write(
        &geometry,
        concat!(
            "header { efgdl = 2 }\n",
            "1{b<bc>[4]r<read>:}\n",
            "-> #[header = append(\" CB:Z:\", <bc>)] ",
            "1{f[AC]<bc><read>}\n",
        ),
    )
    .unwrap();
    fs::write(&input, b"@one\nAAAATGCA\n+\n12345678\n").unwrap();

    let common = [
        "run",
        "--geom",
        geometry.to_str().unwrap(),
        "--file1",
        input.to_str().unwrap(),
        "--threads",
        "1",
        "--staged-pipeline",
    ];
    Command::cargo_bin("seqproc")
        .unwrap()
        .args(common)
        .args(["--out1", direct.to_str().unwrap()])
        .assert()
        .success();
    Command::cargo_bin("seqproc")
        .unwrap()
        .args(common)
        .args([
            "--out1",
            materialized.to_str().unwrap(),
            "--no-direct-output-rendering",
        ])
        .assert()
        .success();

    assert_eq!(fs::read(&direct).unwrap(), fs::read(&materialized).unwrap());
    assert_eq!(
        fs::read_to_string(&direct).unwrap(),
        "@one CB:Z:AAAA\nACAAAATGCA\n+\nII12345678\n"
    );
}

#[test]
fn optimized_and_unoptimized_graphs_emit_identical_fastq() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("optimized.geom");
    let input = directory.path().join("input.fastq");
    let optimized = directory.path().join("optimized.fastq");
    let unoptimized = directory.path().join("unoptimized.fastq");
    let optimized_summary = directory.path().join("optimized.json");
    let unoptimized_summary = directory.path().join("unoptimized.json");
    fs::write(
        &geometry,
        concat!(
            "header { efgdl = 2 }\n",
            "1{b<bc>[4]r<read>:}\n",
            "-> #[header = append(\" CB:Z:\", <bc>)] ",
            "1{f[AC]<bc><read>}\n",
        ),
    )
    .unwrap();
    fs::write(&input, b"@one\nAAAATGCA\n+\n12345678\n").unwrap();

    let common = [
        "run",
        "--geom",
        geometry.to_str().unwrap(),
        "--file1",
        input.to_str().unwrap(),
        "--threads",
        "1",
        "--execution-mode",
        "pipeline",
    ];
    Command::cargo_bin("seqproc")
        .unwrap()
        .args(common)
        .args([
            "--out1",
            optimized.to_str().unwrap(),
            "--summary",
            optimized_summary.to_str().unwrap(),
        ])
        .assert()
        .success();
    Command::cargo_bin("seqproc")
        .unwrap()
        .args(common)
        .args([
            "--out1",
            unoptimized.to_str().unwrap(),
            "--summary",
            unoptimized_summary.to_str().unwrap(),
            "--no-graph-optimization",
            "--no-direct-output-rendering",
        ])
        .assert()
        .success();

    assert_eq!(
        fs::read(&optimized).unwrap(),
        fs::read(&unoptimized).unwrap()
    );
    let optimized_report: Value =
        serde_json::from_slice(&fs::read(optimized_summary).unwrap()).unwrap();
    let unoptimized_report: Value =
        serde_json::from_slice(&fs::read(unoptimized_summary).unwrap()).unwrap();
    assert_eq!(optimized_report["graph_optimization"]["enabled"], true);
    assert_eq!(unoptimized_report["graph_optimization"]["enabled"], false);
    assert_eq!(
        optimized_report["execution_plan"]["backend"],
        "worker_local_pipeline"
    );
    assert_eq!(
        optimized_report["execution_plan"]["requested_mode"],
        "pipeline"
    );
    assert_eq!(
        unoptimized_report["execution_plan"]["direct_output_rendering"],
        false
    );
}

#[test]
fn gzip_level_is_validated_and_preserves_fastq_bytes() {
    let directory = tempdir().unwrap();
    let geometry = fixture("fgdl/match.geom");
    let input1 = fixture("test_data/in/match/match_l.fastq");
    let input2 = fixture("test_data/in/match/match_r.fastq");
    let plain1 = directory.path().join("plain-r1.fastq");
    let plain2 = directory.path().join("plain-r2.fastq");
    let gzip1 = directory.path().join("level3-r1.fastq.gz");
    let gzip2 = directory.path().join("level3-r2.fastq.gz");
    let stream1 = directory.path().join("stream-r1.fastq.gz");
    let stream2 = directory.path().join("stream-r2.fastq.gz");
    let stream_summary = directory.path().join("stream-summary.json");
    let input_gzip1 = directory.path().join("input-r1.fastq.gz");
    let input_gzip2 = directory.path().join("input-r2.fastq.gz");
    let accelerated1 = directory.path().join("accelerated-r1.fastq");
    let accelerated2 = directory.path().join("accelerated-r2.fastq");
    let accelerated_summary = directory.path().join("accelerated-summary.json");
    gzip_copy(&input1, &input_gzip1);
    gzip_copy(&input2, &input_gzip2);

    let common = [
        "run",
        "--geom",
        &geometry,
        "--file1",
        &input1,
        "--file2",
        &input2,
        "--threads",
        "2",
        "--preserve-order",
    ];
    Command::cargo_bin("seqproc")
        .unwrap()
        .args(common)
        .args([
            "--out1",
            plain1.to_str().unwrap(),
            "--out2",
            plain2.to_str().unwrap(),
        ])
        .assert()
        .success();
    Command::cargo_bin("seqproc")
        .unwrap()
        .args(common)
        .args([
            "--out1",
            gzip1.to_str().unwrap(),
            "--out2",
            gzip2.to_str().unwrap(),
            "--gzip-level",
            "3",
            "--parallel-gzip",
        ])
        .assert()
        .success();

    for (plain, compressed) in [(&plain1, &gzip1), (&plain2, &gzip2)] {
        let mut decoded = Vec::new();
        flate2::read::MultiGzDecoder::new(fs::File::open(compressed).unwrap())
            .read_to_end(&mut decoded)
            .unwrap();
        assert_eq!(decoded, fs::read(plain).unwrap());
    }

    Command::cargo_bin("seqproc")
        .unwrap()
        .args(common)
        .args([
            "--out1",
            stream1.to_str().unwrap(),
            "--out2",
            stream2.to_str().unwrap(),
            "--gzip-level",
            "3",
            "--parallel-gzip-stream",
            "--gzip-threads",
            "2",
            "--gzip-block-size",
            "65536",
            "--summary",
            stream_summary.to_str().unwrap(),
        ])
        .assert()
        .success();

    for (plain, compressed) in [(&plain1, &stream1), (&plain2, &stream2)] {
        let mut decoded = Vec::new();
        flate2::read::MultiGzDecoder::new(fs::File::open(compressed).unwrap())
            .read_to_end(&mut decoded)
            .unwrap();
        assert_eq!(decoded, fs::read(plain).unwrap());
    }
    let report: Value = serde_json::from_slice(&fs::read(stream_summary).unwrap()).unwrap();
    assert_eq!(report["schema_version"], "1.4.0");
    assert_eq!(report["parallel_gzip_members"], false);
    assert_eq!(report["parallel_gzip_stream"], true);
    assert_eq!(report["gzip_compression_threads"], 2);
    assert_eq!(report["gzip_block_size"], 65536);

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            &geometry,
            "--file1",
            input_gzip1.to_str().unwrap(),
            "--file2",
            input_gzip2.to_str().unwrap(),
            "--out1",
            accelerated1.to_str().unwrap(),
            "--out2",
            accelerated2.to_str().unwrap(),
            "--threads",
            "2",
            "--preserve-order",
            "--accelerated-gzip-input",
            "--gzip-input-threads",
            "1",
            "--summary",
            accelerated_summary.to_str().unwrap(),
        ])
        .assert()
        .success();
    assert_eq!(fs::read(&accelerated1).unwrap(), fs::read(&plain1).unwrap());
    assert_eq!(fs::read(&accelerated2).unwrap(), fs::read(&plain2).unwrap());
    let report: Value = serde_json::from_slice(&fs::read(accelerated_summary).unwrap()).unwrap();
    assert_eq!(report["gzip_input_backend"], "rapidgzip-core");
    assert_eq!(report["gzip_input_threads"], 1);
    assert_eq!(report["gzip_input_chunk_size"], 256 * 1024);

    Command::cargo_bin("seqproc")
        .unwrap()
        .args(common)
        .args(["--gzip-level", "10"])
        .assert()
        .failure();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args(common)
        .args(["--parallel-gzip", "--parallel-gzip-stream"])
        .assert()
        .failure();
}
