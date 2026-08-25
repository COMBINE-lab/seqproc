use std::{collections::BTreeMap, fs, io::Cursor};

use antisequence::{
    graph::{CountOp, Graph, InputFastqOp},
    trace::NoTrace,
};
use seqproc::execute::compile_geom_typed;
use seqproc_seqspec_import::{assess, ImportOptions, ImportStatus};

fn variable_onlist_yaml(strand: &str, read_min: usize, read_max: usize) -> String {
    format!(
        r#"seqspec_version: 0.4.0
assay_id: variable-onlist
name: Variable onlist fixture
modalities: [rna]
sequence_spec:
  - read_id: R1
    name: Read 1
    modality: rna
    primer_id: primer
    min_len: {read_min}
    max_len: {read_max}
    strand: {strand}
    files: []
library_spec:
  - region_id: rna
    region_type: rna
    sequence_type: joined
    min_len: 6
    max_len: 8
    regions:
      - region_id: variable_bc
        region_type: barcode
        sequence_type: onlist
        min_len: 3
        max_len: 5
        onlist:
          file_id: whitelist
          filename: whitelist.txt
          urltype: local
      - region_id: primer
        region_type: primer
        sequence_type: fixed
        sequence: ACG
        min_len: 3
        max_len: 3
"#
    )
}

#[test]
fn variable_onlist_import_compiles_and_uses_the_matched_length_as_cut_point() {
    let assessment = assess(
        &variable_onlist_yaml("neg", 3, 5),
        "variable.spec.yaml",
        &ImportOptions::default(),
    )
    .unwrap();
    let geometry = &assessment.geometries[0];
    assert_eq!(geometry.status, ImportStatus::SupportedRequiresBinding);
    assert!(geometry.efgdl.contains("filter(b[3-5]"));
    assert!(geometry.efgdl.contains("#[ambig_policy = no_match]"));
    assert!(geometry.efgdl.contains("#[pattern_orientation = rc]"));

    let directory = tempfile::tempdir().unwrap();
    let whitelist = directory.path().join("whitelist.txt");
    // Negative-strand import reverse-complements these once while building
    // the matcher: ACG -> CGT, TTTTT -> AAAAA.
    fs::write(&whitelist, "ACG\nTTTTT\n").unwrap();
    let source = geometry.render_with_resource_defaults(&BTreeMap::from([(
        geometry.resources[0].name.clone(),
        whitelist.display().to_string(),
    )]));
    let compiled = compile_geom_typed(&source).unwrap();
    let fastq = b"@short\nCGT\n+\nIII\n@long\nAAAAA\n+\nIIIII\n@reject\nCGA\n+\nIII\n";
    let mut graph = Graph::<NoTrace>::new();
    graph.add(InputFastqOp::from_reader(Cursor::new(fastq.to_vec())).unwrap());
    compiled.try_interpret(&mut graph, &[]).unwrap();
    let count = graph.add(CountOp::new([true]));
    graph.try_run_with_threads(1).unwrap();
    assert_eq!(count.counts(), [2]);
}

#[test]
fn clipped_onlist_projects_patterns_without_an_external_preprocessing_step() {
    let yaml = variable_onlist_yaml("neg", 2, 2).replace(
        "min_len: 3\n        max_len: 5",
        "min_len: 4\n        max_len: 4",
    );
    let assessment = assess(&yaml, "clipped.spec.yaml", &ImportOptions::default()).unwrap();
    let geometry = &assessment.geometries[0];
    assert_eq!(geometry.status, ImportStatus::SupportedRequiresBinding);
    assert!(geometry
        .efgdl
        .contains("#[pattern_projection = suffix(max_len = 2)]"));
    let directory = tempfile::tempdir().unwrap();
    let whitelist = directory.path().join("whitelist.txt");
    // The negative-strand two-base observation of AAAC is RC(AC) = GT.
    fs::write(&whitelist, "AAAC\n").unwrap();
    let source = geometry.render_with_resource_defaults(&BTreeMap::from([(
        geometry.resources[0].name.clone(),
        whitelist.display().to_string(),
    )]));
    assert_eq!(retained_count(&source, b"@match\nGT\n+\nII\n"), 1);
    assert_eq!(retained_count(&source, b"@reject\nAC\n+\nII\n"), 0);
}

#[test]
fn eight_input_lanes_compile_as_a_bounded_native_geometry() {
    let reads = (1..=8)
        .map(|lane| format!("#[read_len(1, 1)]\n{lane}{{r[1]}}"))
        .collect::<Vec<_>>()
        .join("\n");
    let source = format!("header {{ efgdl = 2 }}\n{reads}\n");
    let compiled = compile_geom_typed(&source).unwrap();
    assert_eq!(compiled.geometry.len(), 8);
}

#[test]
fn tagged_seqspec_03_fastq_regions_are_accepted() {
    let source = r#"!Assay
seqspec_version: 0.3.0
assay_id: null
name: legacy
modalities: [rna]
sequence_spec:
  - !Read
    read_id: R1.fastq.gz
    name: R1
    modality: generic_prep
    primer_id: generic_primer
    min_len: 1
    max_len: 250
    strand: pos
    files: null
library_spec:
  - !Region
    region_id: rna
    region_type: rna
    sequence_type: joined
    min_len: 4
    max_len: 4
    regions:
      - !Region
        region_id: R1.fastq.gz
        region_type: fastq
        sequence_type: joined
        min_len: 4
        max_len: 4
        regions:
          - !Region
            region_id: barcode
            region_type: barcode
            sequence_type: random
            sequence: null
            min_len: 4
            max_len: 4
            regions: null
"#;
    let assessment = assess(source, "legacy.spec.yaml", &ImportOptions::default()).unwrap();
    assert_eq!(assessment.geometries[0].status, ImportStatus::Supported);
    assert_eq!(assessment.geometries[0].inputs[0].min_len, 4);
    assert_eq!(assessment.geometries[0].inputs[0].max_len, 4);
    assert!(compile_geom_typed(&assessment.geometries[0].efgdl).is_ok());
}

#[test]
fn unsupported_seqspec_versions_block_generated_geometry() {
    let source = variable_onlist_yaml("pos", 3, 5).replace("0.4.0", "1.0.0");
    let assessment = assess(&source, "future.spec.yaml", &ImportOptions::default()).unwrap();
    let geometry = &assessment.geometries[0];
    assert_eq!(geometry.status, ImportStatus::BlockedSeqprocCapability);
    assert!(geometry.efgdl.is_empty());
    assert!(geometry.resources.is_empty());
}

#[test]
fn all_n_fixed_regions_normalize_exactly_but_inconsistent_fixed_lengths_do_not() {
    let all_n = variable_onlist_yaml("neg", 4, 4)
        .replace("sequence_type: onlist", "sequence_type: fixed")
        .replace("min_len: 3\n        max_len: 5", "min_len: 4\n        max_len: 4")
        .replace("        onlist:\n          file_id: whitelist\n          filename: whitelist.txt\n          urltype: local", "        sequence: NNNN");
    let assessment = assess(&all_n, "all-n.spec.yaml", &ImportOptions::default()).unwrap();
    assert_eq!(assessment.geometries[0].status, ImportStatus::Supported);
    assert!(assessment.geometries[0]
        .efgdl
        .contains("b<read1_variable_bc>[4]"));

    let inconsistent = all_n.replace("max_len: 4", "max_len: 5");
    let assessment = assess(
        &inconsistent,
        "bad-fixed.spec.yaml",
        &ImportOptions::default(),
    )
    .unwrap();
    assert_eq!(
        assessment.geometries[0].status,
        ImportStatus::BlockedSourceInvalid
    );
    assert!(assessment.geometries[0]
        .diagnostics
        .iter()
        .any(|diagnostic| diagnostic.code == "fixed_sequence_length_mismatch"));
}

fn retained_count(geometry: &str, fastq: &[u8]) -> usize {
    let compiled = compile_geom_typed(geometry).unwrap();
    let mut graph = Graph::<NoTrace>::new();
    graph.add(InputFastqOp::from_reader(Cursor::new(fastq.to_vec())).unwrap());
    compiled.try_interpret(&mut graph, &[]).unwrap();
    let count = graph.add(CountOp::new([true]));
    graph.try_run_with_threads(1).unwrap();
    count.counts()[0]
}

#[test]
fn matched_boundary_is_explicit_and_legacy_ranged_filter_semantics_are_unchanged() {
    let directory = tempfile::tempdir().unwrap();
    let whitelist = directory.path().join("whitelist.txt");
    fs::write(&whitelist, "ACG\n").unwrap();
    let resource = whitelist.display();
    let legacy = format!(
        "header {{ efgdl = 2 }}\nresources {{ wl = \"{resource}\" }}\nbc = filter(b[3-5], $wl)\n1{{<bc>}}"
    );
    let matched = format!(
        "header {{ efgdl = 2 }}\nresources {{ wl = \"{resource}\" }}\n#[pattern_boundary = matched]\nbc = filter(b[3-5], $wl)\n1{{<bc>}}"
    );
    let fastq = b"@read\nACGTT\n+\nIIIII\n";
    assert_eq!(retained_count(&legacy, fastq), 0);
    assert_eq!(retained_count(&matched, fastq), 1);
}

#[test]
fn matched_boundary_rejects_non_ranged_or_inexact_filters_at_compile_time() {
    let fixed = "header { efgdl = 2 }\nresources { wl }\n#[pattern_boundary = matched]\nbc = filter(b[4], $wl)\n1{<bc>}";
    assert!(compile_geom_typed(fixed).is_err());
    let inexact = "header { efgdl = 2 }\nresources { wl }\n#[pattern_boundary = matched]\nbc = filter(b[3-5], $wl, 1)\n1{<bc>}";
    assert!(compile_geom_typed(inexact).is_err());
}

#[test]
fn variable_onlist_prefix_ambiguity_obeys_the_conservative_policy() {
    let directory = tempfile::tempdir().unwrap();
    let whitelist = directory.path().join("whitelist.txt");
    fs::write(&whitelist, "AAA\nAAAAA\n").unwrap();
    let resource = whitelist.display();
    let geometry = format!(
        "header {{ efgdl = 2 }}\nresources {{ wl = \"{resource}\" }}\n#[ambig_policy = no_match]\n#[pattern_boundary = matched]\nbc = filter(b[3-5], $wl)\n1{{<bc>}}"
    );
    assert_eq!(
        retained_count(&geometry, b"@ambiguous\nAAAAA\n+\nIIIII\n"),
        0
    );
    assert_eq!(retained_count(&geometry, b"@unique\nAAA\n+\nIII\n"), 1);
}
