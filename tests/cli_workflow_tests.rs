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
    let schema: Value = serde_json::from_str(include_str!(
        "../schemas/seqproc-summary-1.13.0.schema.json"
    ))
    .unwrap();
    assert_eq!(
        report["schema_version"],
        schema["properties"]["schema_version"]["const"]
    );
    let properties = schema["properties"].as_object().unwrap();
    for key in report.as_object().unwrap().keys() {
        assert!(
            properties.contains_key(key),
            "summary field {key:?} is absent from schema 1.13.0"
        );
    }
    for required in schema["required"].as_array().unwrap() {
        let required = required.as_str().unwrap();
        assert!(
            report.get(required).is_some(),
            "required summary field {required:?} is absent"
        );
    }
    let validator = jsonschema::validator_for(&schema).expect("summary schema must be valid");
    if let Err(error) = validator.validate(report) {
        panic!("summary does not validate against schema 1.13.0: {error}");
    }
}

#[test]
fn verbose_version_reports_cpu_and_backend_provenance() {
    let output = Command::cargo_bin("seqproc")
        .unwrap()
        .args(["--version", "--verbose"])
        .assert()
        .success();
    let stdout = String::from_utf8_lossy(&output.get_output().stdout);
    assert!(stdout.contains(concat!("seqproc ", env!("CARGO_PKG_VERSION"))));
    assert!(stdout.contains("target:"));
    assert!(stdout.contains("compiler CPU target:"));
    assert!(stdout.contains("CPU floor:"));
    assert!(stdout.contains("SIMD backend:"));

    Command::cargo_bin("seqproc")
        .unwrap()
        .arg("-V")
        .assert()
        .success()
        .stdout(predicates::str::starts_with("seqproc "));
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
fn named_resources_match_positional_resources_and_report_provenance() {
    let directory = tempdir().unwrap();
    let named_geometry = directory.path().join("named.geom");
    let positional_geometry = directory.path().join("positional.geom");
    let whitelist = directory.path().join("whitelist.txt");
    let input = directory.path().join("input.fastq");
    let named_output = directory.path().join("named.fastq");
    let positional_output = directory.path().join("positional.fastq");
    let summary = directory.path().join("summary.json");
    fs::write(&whitelist, "AAAA\n").unwrap();
    fs::write(&input, "@kept\nAAAA\n+\nIIII\n@dropped\nCCCC\n+\nIIII\n").unwrap();
    fs::write(
        &named_geometry,
        "header { efgdl = 2 }\nresources { whitelist }\nbc = filter(b[4], $whitelist)\n1{<bc>} -> 1{<bc>}\n",
    )
    .unwrap();
    fs::write(
        &positional_geometry,
        "bc = filter(b[4], $0)\n1{<bc>} -> 1{<bc>}\n",
    )
    .unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            named_geometry.to_str().unwrap(),
            "--bind",
            &format!("whitelist={}", whitelist.display()),
            "--file1",
            input.to_str().unwrap(),
            "--out1",
            named_output.to_str().unwrap(),
            "--summary",
            summary.to_str().unwrap(),
        ])
        .assert()
        .success();
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            positional_geometry.to_str().unwrap(),
            "--additional",
            whitelist.to_str().unwrap(),
            "--file1",
            input.to_str().unwrap(),
            "--out1",
            positional_output.to_str().unwrap(),
        ])
        .assert()
        .success();

    assert_eq!(
        fs::read(named_output).unwrap(),
        fs::read(positional_output).unwrap()
    );
    let report: Value = serde_json::from_slice(&fs::read(summary).unwrap()).unwrap();
    assert_current_summary_shape(&report);
    assert_eq!(
        report["resources"]["resolved"][0]["reference"],
        "$whitelist"
    );
    assert_eq!(report["resources"]["resolved"][0]["source"], "named");
    assert!(report["resources"]["resolved"][0]["content_digest"]
        .as_str()
        .unwrap()
        .starts_with("blake3:"));
}

#[test]
fn named_resource_defaults_and_binding_errors_are_deterministic() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("default.geom");
    let whitelist = directory.path().join("default-whitelist.txt");
    let input = directory.path().join("input.fastq");
    let output = directory.path().join("output.fastq");
    fs::write(&whitelist, "AAAA\n").unwrap();
    fs::write(&input, "@read\nAAAA\n+\nIIII\n").unwrap();
    fs::write(
        &geometry,
        "header { efgdl = 2 }\nresources { whitelist = \"default-whitelist.txt\" }\nbc = filter(b[4], $whitelist)\n1{<bc>} -> 1{<bc>}\n",
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

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--bind",
            "unknown=nowhere",
            "--file1",
            input.to_str().unwrap(),
        ])
        .assert()
        .failure()
        .stderr(predicates::str::contains("not declared"));

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--bind",
            "whitelist=one",
            "--bind",
            "whitelist=two",
            "--file1",
            input.to_str().unwrap(),
        ])
        .assert()
        .failure()
        .stderr(predicates::str::contains("more than once"));
}

#[test]
fn grouped_fastq_shards_match_logical_lane_concatenation() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("paired.geom");
    fs::write(&geometry, "1{r:}2{r:}\n").unwrap();

    let r1a = directory.path().join("r1-a.fastq");
    let r1b_plain = directory.path().join("r1-b.fastq");
    let r1b = directory.path().join("r1-b.fastq.gz");
    let r2a = directory.path().join("r2-a.fastq");
    let r2b_plain = directory.path().join("r2-b.fastq");
    let r2b = directory.path().join("r2-b.fastq.gz");
    fs::write(&r1a, "@a/1\nAAAA\n+\nIIII\n").unwrap();
    fs::write(&r1b_plain, "@b/1\nCCCC\n+\nJJJJ\n").unwrap();
    fs::write(&r2a, "@a/2\nTT\n+\nKK\n").unwrap();
    fs::write(&r2b_plain, "@b/2\nGG\n+\nLL\n").unwrap();
    gzip_copy(r1b_plain.to_str().unwrap(), &r1b);
    gzip_copy(r2b_plain.to_str().unwrap(), &r2b);

    let grouped1 = directory.path().join("grouped-r1.fastq");
    let grouped2 = directory.path().join("grouped-r2.fastq");
    let summary = directory.path().join("grouped-summary.json");
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            &format!("{},{}", r1a.display(), r1b.display()),
            "--read2",
            r2a.to_str().unwrap(),
            "--read2",
            r2b.to_str().unwrap(),
            "--out1",
            grouped1.to_str().unwrap(),
            "--out2",
            grouped2.to_str().unwrap(),
            "--summary",
            summary.to_str().unwrap(),
            "--preserve-order",
        ])
        .assert()
        .success();

    let concatenated1 = directory.path().join("r1-concatenated.fastq");
    let concatenated2 = directory.path().join("r2-concatenated.fastq");
    fs::write(
        &concatenated1,
        [fs::read(&r1a).unwrap(), fs::read(&r1b_plain).unwrap()].concat(),
    )
    .unwrap();
    fs::write(
        &concatenated2,
        [fs::read(&r2a).unwrap(), fs::read(&r2b_plain).unwrap()].concat(),
    )
    .unwrap();
    let baseline1 = directory.path().join("baseline-r1.fastq");
    let baseline2 = directory.path().join("baseline-r2.fastq");
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--file1",
            concatenated1.to_str().unwrap(),
            "--file2",
            concatenated2.to_str().unwrap(),
            "--out1",
            baseline1.to_str().unwrap(),
            "--out2",
            baseline2.to_str().unwrap(),
        ])
        .assert()
        .success();
    assert_eq!(fs::read(grouped1).unwrap(), fs::read(baseline1).unwrap());
    assert_eq!(fs::read(grouped2).unwrap(), fs::read(baseline2).unwrap());

    let report: Value = serde_json::from_slice(&fs::read(summary).unwrap()).unwrap();
    assert_current_summary_shape(&report);
    assert_eq!(
        report["shard_read_counts"],
        serde_json::json!([[1, 1], [1, 1]])
    );

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            r1a.to_str().unwrap(),
            "--read1",
            r1b.to_str().unwrap(),
            "--read2",
            r2a.to_str().unwrap(),
        ])
        .assert()
        .failure()
        .stderr(predicates::str::contains("has 1 shards; expected 2"));
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
    assert_eq!(report["schema_version"], "1.13.0");
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
    assert_eq!(report["execution_plan"]["batch_planning"]["enabled"], true);
    assert_eq!(
        report["execution_plan"]["batch_planning"]["automatic_batch_size"],
        true
    );
    assert_eq!(
        report["execution_plan"]["batch_planning"]["automatic_queue_capacity"],
        false
    );
    assert_eq!(
        report["execution_plan"]["batch_planning"]["automatic_max_in_flight"],
        false
    );
    assert_eq!(
        report["execution_plan"]["batch_planning"]["queue_capacity"],
        1
    );
    assert_eq!(
        report["execution_plan"]["batch_planning"]["max_in_flight_batches"],
        2
    );
    assert_eq!(
        report["execution_plan"]["batch_planning"]["memory_budget_bytes"],
        256 * 1024 * 1024
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
    let pass_ablated = directory.path().join("pass-ablated.fastq");
    let optimized_summary = directory.path().join("optimized.json");
    let unoptimized_summary = directory.path().join("unoptimized.json");
    let pass_ablated_summary = directory.path().join("pass-ablated.json");
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
            pass_ablated.to_str().unwrap(),
            "--summary",
            pass_ablated_summary.to_str().unwrap(),
            "--no-dead-label-elimination",
            "--no-early-filter-placement",
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
    assert_eq!(
        fs::read(&optimized).unwrap(),
        fs::read(&pass_ablated).unwrap()
    );
    let optimized_report: Value =
        serde_json::from_slice(&fs::read(optimized_summary).unwrap()).unwrap();
    let unoptimized_report: Value =
        serde_json::from_slice(&fs::read(unoptimized_summary).unwrap()).unwrap();
    let pass_ablated_report: Value =
        serde_json::from_slice(&fs::read(pass_ablated_summary).unwrap()).unwrap();
    assert_eq!(optimized_report["graph_optimization"]["enabled"], true);
    assert_eq!(unoptimized_report["graph_optimization"]["enabled"], false);
    let passes = pass_ablated_report["graph_optimization"]["passes"]
        .as_array()
        .unwrap();
    for pass_name in ["dead_label_elimination", "early_selective_filter_placement"] {
        let pass = passes
            .iter()
            .find(|pass| pass["pass"] == pass_name)
            .unwrap();
        assert_eq!(pass["changed_nodes"], 0);
    }
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
    assert_eq!(report["schema_version"], "1.13.0");
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

#[test]
fn legacy_single_output_preserves_paired_no_transform_behavior() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("paired.geom");
    let read1 = directory.path().join("r1.fastq");
    let read2 = directory.path().join("r2.fastq");
    let output = directory.path().join("r1-out.fastq");
    fs::write(&geometry, "1{r:}2{r:}\n").unwrap();
    fs::write(&read1, "@pair/1\nAAAA\n+\nIIII\n").unwrap();
    fs::write(&read2, "@pair/2\nTTTT\n+\nIIII\n").unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "-g",
            geometry.to_str().unwrap(),
            "-1",
            read1.to_str().unwrap(),
            "-2",
            read2.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ])
        .assert()
        .success();
    assert_eq!(fs::read(output).unwrap(), fs::read(read1).unwrap());
}

#[test]
fn modern_run_requires_exact_paired_outputs() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("paired.geom");
    let read1 = directory.path().join("r1.fastq");
    let read2 = directory.path().join("r2.fastq");
    let output = directory.path().join("r1-out.fastq");
    fs::write(&geometry, "1{r:}2{r:}\n").unwrap();
    fs::write(&read1, "@pair/1\nAAAA\n+\nIIII\n").unwrap();
    fs::write(&read2, "@pair/2\nTTTT\n+\nIIII\n").unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            read1.to_str().unwrap(),
            "--read2",
            read2.to_str().unwrap(),
            "--out1",
            output.to_str().unwrap(),
        ])
        .assert()
        .failure()
        .stderr(predicates::str::contains(
            "1 primary output targets were supplied, but the geometry emits 2 reads",
        ));
    assert!(!output.exists());
}

#[test]
fn modern_run_rejects_an_all_discard_primary_topology() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("single.geom");
    let input = directory.path().join("input.fastq");
    fs::write(&geometry, "1{r:}\n").unwrap();
    fs::write(&input, "@read\nAAAA\n+\nIIII\n").unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            input.to_str().unwrap(),
        ])
        .assert()
        .failure()
        .stderr(predicates::str::contains(
            "at least one primary FASTQ output target must not be discard",
        ));
}

#[test]
fn unassigned2_on_single_lane_is_rejected_instead_of_ignored() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("single.geom");
    let input = directory.path().join("input.fastq");
    let output = directory.path().join("output.fastq");
    let unassigned2 = directory.path().join("unassigned2.fastq");
    fs::write(&geometry, "1{r:}\n").unwrap();
    fs::write(&input, "@read\nAAAA\n+\nIIII\n").unwrap();
    fs::write(&unassigned2, "sentinel\n").unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            input.to_str().unwrap(),
            "--out1",
            output.to_str().unwrap(),
            "--unassigned2",
            unassigned2.to_str().unwrap(),
        ])
        .assert()
        .failure()
        .stderr(predicates::str::contains(
            "unassigned output arity is 2, but input arity is 1",
        ));
    assert_eq!(fs::read_to_string(unassigned2).unwrap(), "sentinel\n");
    assert!(!output.exists());
}

#[test]
fn out2_on_a_single_output_geometry_is_not_silently_ignored() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("single.geom");
    let input = directory.path().join("input.fastq");
    let output1 = directory.path().join("out1.fastq");
    let output2 = directory.path().join("out2.fastq");
    fs::write(&geometry, "1{r:}\n").unwrap();
    fs::write(&input, "@read\nAAAA\n+\nIIII\n").unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--file1",
            input.to_str().unwrap(),
            "--out1",
            output1.to_str().unwrap(),
            "--out2",
            output2.to_str().unwrap(),
        ])
        .assert()
        .failure()
        .stderr(predicates::str::contains(
            "2 primary output targets were supplied, but the geometry emits 1 reads",
        ));
    assert!(!output2.exists());
}

#[test]
fn empty_plain_and_gzip_inputs_are_valid_zero_record_runs() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("single.geom");
    let plain = directory.path().join("empty.fastq");
    let gzip = directory.path().join("empty.fastq.gz");
    fs::write(&geometry, "1{r:}\n").unwrap();
    fs::write(&plain, []).unwrap();
    gzip_copy(plain.to_str().unwrap(), &gzip);

    for (index, input) in [plain, gzip].iter().enumerate() {
        let output = directory.path().join(format!("empty-{index}.fastq"));
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
        assert_eq!(fs::metadata(output).unwrap().len(), 0);
    }
}

#[cfg(target_os = "linux")]
#[test]
fn final_output_flush_failure_is_a_nonzero_cli_error() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("single.geom");
    let input = directory.path().join("input.fastq");
    fs::write(&geometry, "1{r:}\n").unwrap();
    fs::write(&input, "@read\nAAAA\n+\nIIII\n").unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--file1",
            input.to_str().unwrap(),
            "--out1",
            "/dev/full",
            "--threads",
            "1",
        ])
        .assert()
        .failure();
}

#[test]
fn stdin_stdout_streams_are_clean_typed_and_composable() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("stream.geom");
    let input = directory.path().join("input.fastq");
    let reference = directory.path().join("reference.fastq");
    let from_stdin = directory.path().join("from-stdin.fastq");
    let fastq = b"@r1\nACGT\n+\nIIII\n@r2\nTGCA\n+\nJJJJ\n";
    fs::write(&geometry, "1{r:}\n").unwrap();
    fs::write(&input, fastq).unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            input.to_str().unwrap(),
            "--out1",
            reference.to_str().unwrap(),
        ])
        .assert()
        .success();
    let expected = fs::read(&reference).unwrap();

    let file_to_stdout = Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            input.to_str().unwrap(),
            "--out1",
            "-",
        ])
        .assert()
        .success();
    assert_eq!(file_to_stdout.get_output().stdout, expected);

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            "-",
            "--out1",
            from_stdin.to_str().unwrap(),
        ])
        .write_stdin(fastq.as_slice())
        .assert()
        .success();
    assert_eq!(fs::read(&from_stdin).unwrap(), expected);

    let mut gzip_input = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::new(3));
    use std::io::Write as _;
    gzip_input.write_all(fastq).unwrap();
    let gzip_input = gzip_input.finish().unwrap();
    let gzip_stdin_output = directory.path().join("gzip-stdin.fastq");
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            "-",
            "--out1",
            gzip_stdin_output.to_str().unwrap(),
        ])
        .write_stdin(gzip_input)
        .assert()
        .success();
    assert_eq!(fs::read(gzip_stdin_output).unwrap(), expected);

    let stdin_to_stdout = Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            "-",
            "--out1",
            "-",
            "--summary",
            "-",
        ])
        .write_stdin(fastq.as_slice())
        .assert()
        .success();
    assert_eq!(stdin_to_stdout.get_output().stdout, expected);
    let stderr = String::from_utf8_lossy(&stdin_to_stdout.get_output().stderr);
    assert!(stderr.contains("\"schema_version\": \"1.13.0\""));
    assert!(stderr.contains("\"input_topology\""));
    assert!(stderr.contains("\"stdin\""));
    assert!(stderr.contains("\"stdout\""));
    assert!(stderr.contains("\"accepted_fragments\": 2"));

    let compressed = Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            input.to_str().unwrap(),
            "--out1",
            "-",
            "--stdout-gzip",
        ])
        .assert()
        .success();
    let mut decoded = Vec::new();
    flate2::read::MultiGzDecoder::new(compressed.get_output().stdout.as_slice())
        .read_to_end(&mut decoded)
        .unwrap();
    assert_eq!(decoded, expected);

    let paired_geometry = directory.path().join("paired.geom");
    fs::write(&paired_geometry, "1{r:}\n2{r:}\n").unwrap();
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            paired_geometry.to_str().unwrap(),
            "--read1",
            "-",
            "--read2",
            "-",
            "--out1",
            reference.to_str().unwrap(),
            "--out2",
            from_stdin.to_str().unwrap(),
        ])
        .write_stdin(fastq.as_slice())
        .assert()
        .failure()
        .stderr(predicates::str::contains("at most one FASTQ input source"));
}

#[test]
fn interleaved_shards_stdin_and_rejection_routing_match_separate_lanes() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("paired-filter.geom");
    fs::write(&geometry, "anchor = f[AAAA]\n1{<anchor>r:}\n2{r:}\n").unwrap();
    let r1 = directory.path().join("r1.fastq");
    let r2 = directory.path().join("r2.fastq");
    let shard1 = directory.path().join("interleaved-1.fastq");
    let shard2_plain = directory.path().join("interleaved-2.fastq");
    let shard2 = directory.path().join("interleaved-2.fastq.gz");
    let r1_bytes = b"@accepted/1\nAAAACC\n+\nIIIIII\n@rejected/1\nTTTTCC\n+\nJJJJJJ\n";
    let r2_bytes = b"@accepted/2\nGGGG\n+\nKKKK\n@rejected/2\nCCCC\n+\nLLLL\n";
    let first = b"@accepted/1\nAAAACC\n+\nIIIIII\n@accepted/2\nGGGG\n+\nKKKK\n";
    let second = b"@rejected/1\nTTTTCC\n+\nJJJJJJ\n@rejected/2\nCCCC\n+\nLLLL\n";
    fs::write(&r1, r1_bytes).unwrap();
    fs::write(&r2, r2_bytes).unwrap();
    fs::write(&shard1, first).unwrap();
    fs::write(&shard2_plain, second).unwrap();
    gzip_copy(shard2_plain.to_str().unwrap(), &shard2);

    let separate1 = directory.path().join("separate-r1.fastq");
    let separate2 = directory.path().join("separate-r2.fastq");
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            r1.to_str().unwrap(),
            "--read2",
            r2.to_str().unwrap(),
            "--out1",
            separate1.to_str().unwrap(),
            "--out2",
            separate2.to_str().unwrap(),
        ])
        .assert()
        .success();

    let interleaved1 = directory.path().join("interleaved-r1.fastq");
    let interleaved2 = directory.path().join("interleaved-r2.fastq");
    let rejected1 = directory.path().join("rejected-r1.fastq");
    let rejected2 = directory.path().join("rejected-r2.fastq");
    let summary = directory.path().join("interleaved-summary.json");
    let input_list = format!("{},{}", shard1.display(), shard2.display());
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--interleaved-input",
            &input_list,
            "--out1",
            interleaved1.to_str().unwrap(),
            "--out2",
            interleaved2.to_str().unwrap(),
            "--unassigned1",
            rejected1.to_str().unwrap(),
            "--unassigned2",
            rejected2.to_str().unwrap(),
            "--threads",
            "2",
            "--preserve-order",
            "--accelerated-gzip-input",
            "--summary",
            summary.to_str().unwrap(),
        ])
        .assert()
        .success();
    assert_eq!(
        fs::read(&interleaved1).unwrap(),
        fs::read(&separate1).unwrap()
    );
    assert_eq!(
        fs::read(&interleaved2).unwrap(),
        fs::read(&separate2).unwrap()
    );
    assert!(fs::read_to_string(&rejected1)
        .unwrap()
        .contains("@rejected/1"));
    assert!(fs::read_to_string(&rejected2)
        .unwrap()
        .contains("@rejected/2"));
    let report: Value = serde_json::from_slice(&fs::read(summary).unwrap()).unwrap();
    assert_current_summary_shape(&report);
    assert_eq!(report["input_layout"], "interleaved");
    assert_eq!(report["n_fastqs"], 2);
    assert_eq!(report["total_fragments"], 2);
    assert_eq!(report["accepted_fragments"], 1);
    assert_eq!(report["rejected_fragments"], 1);
    assert_eq!(report["shard_read_counts"][0], serde_json::json!([1, 1]));
    assert_eq!(report["shard_read_counts"][1], serde_json::json!([1, 1]));

    let mut all_interleaved = first.to_vec();
    all_interleaved.extend_from_slice(second);
    let stdin1 = directory.path().join("stdin-r1.fastq");
    let stdin2 = directory.path().join("stdin-r2.fastq");
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--interleaved-input",
            "-",
            "--out1",
            stdin1.to_str().unwrap(),
            "--out2",
            stdin2.to_str().unwrap(),
        ])
        .write_stdin(all_interleaved)
        .assert()
        .success();
    assert_eq!(fs::read(stdin1).unwrap(), fs::read(separate1).unwrap());
    assert_eq!(fs::read(stdin2).unwrap(), fs::read(separate2).unwrap());

    let truncated = directory.path().join("truncated.fastq");
    fs::write(
        &truncated,
        first
            .iter()
            .copied()
            .chain(b"@partial\nAA\n+\nII\n".iter().copied())
            .collect::<Vec<_>>(),
    )
    .unwrap();
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--interleaved-input",
            truncated.to_str().unwrap(),
            "--out1",
            interleaved1.to_str().unwrap(),
            "--out2",
            interleaved2.to_str().unwrap(),
        ])
        .assert()
        .failure()
        .stderr(predicates::str::contains("expected 2 records, observed 1"));
}

#[test]
fn three_segment_scatac_paths_cover_shards_streams_interleaving_and_reports() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("scatac.geom");
    fs::write(
        &geometry,
        "anchor = f[AAAA]\n1{b<g1>[4]r:}\n2{<anchor>r:}\n3{b<g2>[4]r:}\n-> 1{<g1>} 2{<anchor>} 3{<g2>}\n",
    )
    .unwrap();

    let lane_records = [
        [
            b"@accepted/1\nACGT\n+\nIIII\n".as_slice(),
            b"@rejected/1\nTGCA\n+\nJJJJ\n".as_slice(),
        ],
        [
            b"@accepted/2\nAAAA\n+\nKKKK\n".as_slice(),
            b"@rejected/2\nCCCC\n+\nLLLL\n".as_slice(),
        ],
        [
            b"@accepted/3\nGGGG\n+\nMMMM\n".as_slice(),
            b"@rejected/3\nTTTT\n+\nNNNN\n".as_slice(),
        ],
    ];
    let mut separate_args = Vec::new();
    let mut complete_lanes = Vec::new();
    for (lane, records) in lane_records.iter().enumerate() {
        let first = directory.path().join(format!("lane{}-1.fastq", lane + 1));
        let second_plain = directory.path().join(format!("lane{}-2.fastq", lane + 1));
        let second = directory
            .path()
            .join(format!("lane{}-2.fastq.gz", lane + 1));
        fs::write(&first, records[0]).unwrap();
        fs::write(&second_plain, records[1]).unwrap();
        gzip_copy(second_plain.to_str().unwrap(), &second);
        separate_args.push(format!("{},{}", first.display(), second.display()));
        let mut complete = records[0].to_vec();
        complete.extend_from_slice(records[1]);
        let complete_path = directory.path().join(format!("lane{}.fastq", lane + 1));
        fs::write(&complete_path, complete).unwrap();
        complete_lanes.push(complete_path);
    }

    let separate_outputs = [
        directory.path().join("separate-1.fastq"),
        directory.path().join("separate-2.fastq"),
        directory.path().join("separate-3.fastq"),
    ];
    let separate_rejected = [
        directory.path().join("separate-rejected-1.fastq"),
        directory.path().join("separate-rejected-2.fastq"),
        directory.path().join("separate-rejected-3.fastq"),
    ];
    let separate_summary = directory.path().join("separate-summary.json");
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            &separate_args[0],
            "--read2",
            &separate_args[1],
            "--read3",
            &separate_args[2],
            "--out1",
            separate_outputs[0].to_str().unwrap(),
            "--out2",
            separate_outputs[1].to_str().unwrap(),
            "--out3",
            separate_outputs[2].to_str().unwrap(),
            "--unassigned1",
            separate_rejected[0].to_str().unwrap(),
            "--unassigned2",
            separate_rejected[1].to_str().unwrap(),
            "--unassigned3",
            separate_rejected[2].to_str().unwrap(),
            "--threads",
            "2",
            "--preserve-order",
            "--summary",
            separate_summary.to_str().unwrap(),
        ])
        .assert()
        .success();
    for (lane, output) in separate_outputs.iter().enumerate() {
        let text = fs::read_to_string(output).unwrap();
        assert!(text.contains(&format!("@accepted/{}", lane + 1)));
        assert!(!text.contains("@rejected"));
        let rejected = fs::read_to_string(&separate_rejected[lane]).unwrap();
        assert!(rejected.contains(&format!("@rejected/{}", lane + 1)));
    }
    let report: Value = serde_json::from_slice(&fs::read(&separate_summary).unwrap()).unwrap();
    assert_current_summary_shape(&report);
    assert_eq!(report["schema_version"], "1.13.0");
    assert_eq!(report["input_layout"], "separate");
    assert_eq!(report["input_arity"], 3);
    assert_eq!(report["output_arity"], 3);
    assert_eq!(
        report["shard_read_counts"],
        serde_json::json!([[1, 1], [1, 1], [1, 1]])
    );

    let interleaved_first = directory.path().join("interleaved-three-1.fastq");
    let interleaved_second_plain = directory.path().join("interleaved-three-2.fastq");
    let interleaved_second = directory.path().join("interleaved-three-2.fastq.gz");
    let first = [lane_records[0][0], lane_records[1][0], lane_records[2][0]].concat();
    let second = [lane_records[0][1], lane_records[1][1], lane_records[2][1]].concat();
    fs::write(&interleaved_first, &first).unwrap();
    fs::write(&interleaved_second_plain, &second).unwrap();
    gzip_copy(
        interleaved_second_plain.to_str().unwrap(),
        &interleaved_second,
    );
    let interleaved_outputs = [
        directory.path().join("interleaved-three-out-1.fastq"),
        directory.path().join("interleaved-three-out-2.fastq"),
        directory.path().join("interleaved-three-out-3.fastq"),
    ];
    let interleaved_list = format!(
        "{},{}",
        interleaved_first.display(),
        interleaved_second.display()
    );
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--interleaved-input",
            &interleaved_list,
            "--out1",
            interleaved_outputs[0].to_str().unwrap(),
            "--out2",
            interleaved_outputs[1].to_str().unwrap(),
            "--out3",
            interleaved_outputs[2].to_str().unwrap(),
            "--threads",
            "2",
            "--preserve-order",
        ])
        .assert()
        .success();
    for lane in 0..3 {
        assert_eq!(
            fs::read(&interleaved_outputs[lane]).unwrap(),
            fs::read(&separate_outputs[lane]).unwrap()
        );
    }

    let stdin_outputs = [
        directory.path().join("stdin-three-out-1.fastq"),
        directory.path().join("stdin-three-out-2.fastq"),
        directory.path().join("stdin-three-out-3.fastq"),
    ];
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            "-",
            "--read2",
            complete_lanes[1].to_str().unwrap(),
            "--read3",
            complete_lanes[2].to_str().unwrap(),
            "--out1",
            stdin_outputs[0].to_str().unwrap(),
            "--out2",
            stdin_outputs[1].to_str().unwrap(),
            "--out3",
            stdin_outputs[2].to_str().unwrap(),
        ])
        .write_stdin(fs::read(&complete_lanes[0]).unwrap())
        .assert()
        .success();
    for lane in 0..3 {
        assert_eq!(
            fs::read(&stdin_outputs[lane]).unwrap(),
            fs::read(&separate_outputs[lane]).unwrap()
        );
    }

    let stdout_side1 = directory.path().join("stdout-side-1.fastq");
    let stdout_side2 = directory.path().join("stdout-side-2.fastq");
    let stdout = Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            complete_lanes[0].to_str().unwrap(),
            "--read2",
            complete_lanes[1].to_str().unwrap(),
            "--read3",
            complete_lanes[2].to_str().unwrap(),
            "--out1",
            stdout_side1.to_str().unwrap(),
            "--out2",
            stdout_side2.to_str().unwrap(),
            "--out3",
            "-",
        ])
        .assert()
        .success();
    assert_eq!(
        stdout.get_output().stdout,
        fs::read(&separate_outputs[2]).unwrap()
    );

    let four = directory.path().join("four-lane.geom");
    fs::write(&four, "1{r:}\n2{r:}\n3{r:}\n4{r:}\n").unwrap();
    Command::cargo_bin("seqproc")
        .unwrap()
        .args(["validate", four.to_str().unwrap()])
        .assert()
        .failure();
    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            complete_lanes[0].to_str().unwrap(),
            "--read3",
            complete_lanes[2].to_str().unwrap(),
        ])
        .assert()
        .failure()
        .stderr(predicates::str::contains("--read3 requires --read2"));
}

#[test]
fn typed_configuration_errors_have_stable_nonzero_cli_status() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("simple.geom");
    let input = directory.path().join("reads.fastq");
    fs::write(&geometry, "1{r:}\n").unwrap();
    fs::write(&input, "@r1\nACGT\n+\nIIII\n").unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            input.to_str().unwrap(),
            "--threads",
            "0",
        ])
        .assert()
        .code(2)
        .stderr(predicates::str::contains(
            "number of threads must be greater than zero",
        ));

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            input.to_str().unwrap(),
            "--batch-memory-budget-mib",
            "0",
        ])
        .assert()
        .code(2)
        .stderr(predicates::str::contains(
            "dynamic batch planning requires a nonzero memory budget",
        ));
}

#[test]
fn unassigned_outputs_require_exact_input_lane_arity() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("paired.geom");
    let read1 = directory.path().join("r1.fastq");
    let read2 = directory.path().join("r2.fastq");
    let output1 = directory.path().join("out1.fastq");
    let output2 = directory.path().join("out2.fastq");
    let rejected1 = directory.path().join("rejected1.fastq");
    fs::write(&geometry, "1{r:}\n2{r:}\n").unwrap();
    fs::write(&read1, "@r/1\nACGT\n+\nIIII\n").unwrap();
    fs::write(&read2, "@r/2\nTGCA\n+\nIIII\n").unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            read1.to_str().unwrap(),
            "--read2",
            read2.to_str().unwrap(),
            "--out1",
            output1.to_str().unwrap(),
            "--out2",
            output2.to_str().unwrap(),
            "--unassigned1",
            rejected1.to_str().unwrap(),
        ])
        .assert()
        .code(2)
        .stderr(predicates::str::contains(
            "unassigned output arity is 1, but input arity is 2",
        ));

    assert!(!output1.exists());
    assert!(!output2.exists());
    assert!(!rejected1.exists());
}

#[test]
fn demultiplexing_rejects_primary_output_targets_instead_of_ignoring_them() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("simple.geom");
    let input = directory.path().join("reads.fastq");
    let map = directory.path().join("samples.tsv");
    let primary = directory.path().join("silently-lost.fastq");
    let demux_dir = directory.path().join("demux");
    fs::write(&geometry, "1{r:}\n").unwrap();
    fs::write(&input, "@r1\nACGT\n+\nIIII\n").unwrap();
    fs::write(&map, "ACGT\tsample\n").unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            input.to_str().unwrap(),
            "--out1",
            primary.to_str().unwrap(),
            "--demux-map",
            map.to_str().unwrap(),
            "--demux-out-dir",
            demux_dir.to_str().unwrap(),
        ])
        .assert()
        .code(2)
        .stderr(predicates::str::contains(
            "primary FASTQ outputs cannot be combined with demultiplexing",
        ));

    assert!(!primary.exists());
    assert!(!demux_dir.exists());
}

#[test]
fn demultiplexed_summary_reports_one_path_target_per_output_lane() {
    let directory = tempdir().unwrap();
    let geometry = directory.path().join("paired.geom");
    let read1 = directory.path().join("r1.fastq");
    let read2 = directory.path().join("r2.fastq");
    let map = directory.path().join("samples.tsv");
    let summary = directory.path().join("summary.json");
    let demux_dir = directory.path().join("demux");
    fs::write(&geometry, "1{r:}\n2{b<bc1>[4]r:}\n").unwrap();
    fs::write(&read1, "@r/1\nAAAA\n+\nIIII\n").unwrap();
    fs::write(&read2, "@r/2\nACGTTT\n+\nIIIIII\n").unwrap();
    fs::write(&map, "ACGT\tsample\n").unwrap();

    Command::cargo_bin("seqproc")
        .unwrap()
        .args([
            "run",
            "--geom",
            geometry.to_str().unwrap(),
            "--read1",
            read1.to_str().unwrap(),
            "--read2",
            read2.to_str().unwrap(),
            "--demux-map",
            map.to_str().unwrap(),
            "--demux-out-dir",
            demux_dir.to_str().unwrap(),
            "--summary",
            summary.to_str().unwrap(),
        ])
        .assert()
        .success();

    assert!(demux_dir.join("sample_R1.fastq").exists());
    assert!(demux_dir.join("sample_R2.fastq").exists());
    let report: Value = serde_json::from_slice(&fs::read(summary).unwrap()).unwrap();
    assert_current_summary_shape(&report);
    assert_eq!(report["output_arity"], 2);
    assert_eq!(
        report["output_topology"],
        serde_json::json!(["path", "path"])
    );
}
