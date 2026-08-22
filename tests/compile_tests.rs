#[macro_use]
mod common;

use std::collections::HashMap;

use seqproc::{
    compile::{compile, definitions::compile_definitions, reads::compile_reads, utils::Error},
    execute::{compile_geom, compile_geom_typed},
};

use crate::common::utils::{result_with_errs, ParsedInput};

#[test]
fn no_err() -> Result<(), Error> {
    let src = "1{remove(hamming(f[CAG], 1))}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();

    compile_reads(res.reads, HashMap::new())?;

    Ok(())
}

#[test]
fn fail_norm() {
    let src = "1{norm(r:)}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();

    let res = compile_reads(res.reads, HashMap::new());

    assert!(res.is_err());
}

#[test]
fn pass_composition() {
    let src = "1{trunc_to(rev(r:), 1)}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();

    let res = compile_reads(res.reads, HashMap::new());

    assert!(res.is_ok());
}

#[test]
fn fail_remove() {
    let src = "1{rev(remove(r:))}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();

    let res = compile_reads(res.reads, HashMap::new());

    assert!(res.is_err());
}

#[test]
fn discard_as_void() {
    let src = "1{rev(x[10])}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);
    println!("{:?} {:?}", lex_errs, parse_errs);
    let res = parse_res.unwrap();

    let res = compile_reads(res.reads, HashMap::new());

    assert!(res.is_err());
}

#[test]
fn ok_definition() -> Result<(), Error> {
    let src = "
brc = b[10]
brc1 = b[1-4]
1{<brc>}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();

    let (def_map, _warnings) = compile_definitions(res.definitions)?;

    assert_eq!(2, def_map.len());

    Ok(())
}

#[test]
fn duplicate_def() {
    let src = "
brc = b[10]
brc = b[1-4]
1{<brc>}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();
    let def_result = compile_definitions(res.definitions);

    assert!(def_result.is_err());
}

#[test]
fn label_replacement() {
    let src = "test = r:
    1{pad_to(<test>, 5, A)}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();

    let (def_map, _) = compile_definitions(res.definitions).unwrap();

    let res = compile_reads(res.reads, def_map);

    assert!(res.is_err());
}

#[test]
fn no_variable() {
    let src = "testing = r:
    1{pad(<test>, 5, A)}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();

    let (def_map, _) = compile_definitions(res.definitions).unwrap();

    let res = compile_reads(res.reads, def_map);

    assert!(res.is_err());
}

#[test]
fn expr_unwrap() -> Result<(), Error> {
    let src = "1{pad(norm(b[9-10]), 1, A)remove(f[CAGAGC])u[8]remove(b[10])}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();

    compile(res)?;

    Ok(())
}

#[test]
fn fail_reuse_label() {
    let src = "
brc = b[10]
1{<brc><brc>}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();

    let (def_map, _) = compile_definitions(res.definitions).unwrap();

    let res = compile_reads(res.reads, def_map);

    assert!(res.is_err());
}

#[test]
fn def_block_fail() {
    let src = "
brc = b[10]
brc1 = pad(<brc>, 1, A)
1{<brc>}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();

    let def_result = compile_definitions(res.definitions);

    assert!(def_result.is_err());
}

#[test]
fn compile_description() -> Result<(), Error> {
    let src = "
brc = b[10]
umi = pad(u[10], 1, A)
1{<brc>}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();

    compile(res)?;

    Ok(())
}

#[test]
fn fail_description() {
    let src = "
brc = b[10]
umi = pad(u[10], 1, A)
1{<brc><brc>}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();
    let res = compile(res);

    assert!(res.is_err());
}

#[test]
fn fail_label_composition() {
    let src = "
brc = remove(trunc(b[10], 3))
1{pad(<brc>, 1, A)}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();

    let res = compile(res);

    assert!(res.is_err());
}

#[test]
fn valid_geom() -> Result<(), Error> {
    let src = "1{b<brc1>[9-11]remove(f[CAGAGC])u<umi>[8]b<brc2>[10]}2{r<read>:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();
    compile(res)?;

    Ok(())
}

#[test]
fn invalid_geom_one() {
    let src = "1{b[9-11]f[CAGAGC]r:u[8]b[10]}2{r<read>:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();

    let res = compile(res);

    assert!(res.is_err());
}

#[test]
fn invalid_geom_two() {
    let src = "1{f[GAG]b[10-11]b[10]}2{r<read>:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();
    let res = compile(res);

    assert!(res.is_err());
}

#[test]
fn transform_update_map() -> Result<(), Error> {
    let src = "
brc = b[10]
umi = norm(u[9-11])
test = r:
1{pad(<brc>, 1, A)f<read1>[CAGAGC]<umi>f<another>[CAGA]}2{r<read>:}
 -> 1{<brc>remove(<read1>)remove(<umi>)<read>}
";
    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();

    compile(res)?;

    Ok(())
}

#[test]
fn stack_orientation() -> Result<(), Error> {
    let src = "
brc = b[10]
umi = norm(u[9-11])
1{pad(<brc>, 1, A)f<read1>[CAGAGC]<umi>f<another>[CAGA]}2{r<read>:}
 -> 1{<brc>remove(<read1>)remove(pad(<umi>, 1, A))<read>}
";
    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();

    compile(res)?;

    Ok(())
}

#[test]
fn compile_map_arguments() -> Result<(), Error> {
    let src = "1{map(b[10-11], \"file\", norm(self))}2{r<read>:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();
    compile(res)?;

    Ok(())
}

#[test]
fn compile_map_arguments_with_label() -> Result<(), Error> {
    let src = "
brc = b[10-11]
1{map(<brc>, \"file\", norm(self))}2{r<read>:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();
    compile(res)?;

    Ok(())
}

#[test]
fn test_simplified_geom() {
    let geom = String::from("1{b[9-10]f[CAGAGC]u[8]b[10]}2{r:}");

    let res = compile_geom(geom);

    assert!(res.is_ok());
    assert_eq!(
        "1{b[11]u[8]b[10]}2{r:}",
        res.ok().unwrap().get_simplified_description_string()
    );
}

#[test]
fn headerless_geometry_uses_legacy_efgdl_version() {
    let compiled = compile_geom("1{b[16]}2{r:}".to_string()).unwrap();
    assert_eq!(compiled.efgdl_version, 1);
    assert!(compiled.document_header.is_none());
}

#[test]
fn legacy_geometry_can_use_header_as_a_definition_name() {
    let compiled = compile_geom("header = b[2] 1{<header>r:}".to_string()).unwrap();
    assert_eq!(compiled.efgdl_version, 1);
}

#[test]
fn efgdl_two_header_is_retained() {
    let compiled = compile_geom(
        r#"header { efgdl = 2, name = "test protocol" }
        1{b[16]}2{r:}"#
            .to_string(),
    )
    .unwrap();
    assert_eq!(compiled.efgdl_version, 2);
    assert_eq!(compiled.document_header.unwrap().fields.len(), 2);
}

#[test]
fn header_requires_supported_integer_version() {
    for geom in [
        "header { name = test } 1{b[16]}2{r:}",
        "header { efgdl = two } 1{b[16]}2{r:}",
        "header { efgdl = 3 } 1{b[16]}2{r:}",
        "header { efgdl = 2, efgdl = 2 } 1{b[16]}2{r:}",
    ] {
        assert!(compile_geom(geom.to_string()).is_err(), "accepted: {geom}");
    }
}

#[test]
fn efgdl_two_constructs_fixed_output_sequences() {
    let compiled = compile_geom(
        "header { efgdl = 2 } 1{b<bc>[2]r<read>:} -> 1{f[AC]<bc>f[T]<read>}".to_string(),
    )
    .unwrap();
    assert_eq!(
        compiled.get_simplified_description_string(),
        "1{f[AC]b[2]f[T]r:}"
    );
}

#[test]
fn legacy_efgdl_rejects_fixed_output_construction() {
    let result = compile_geom("1{b<bc>[2]r:} -> 1{f[AC]<bc>}".to_string());
    assert!(result.is_err());
    let message = result.unwrap_err()[0].to_string();
    assert!(message.contains("efgdl = 2"), "{message}");
}

#[test]
fn efgdl_two_compiles_output_header_templates() {
    for mode in ["append", "prepend", "replace"] {
        let geometry = format!(
            "header {{ efgdl = 2 }} 1{{b<bc>[2]r<read>:}} -> #[header = {mode}(\" tag:\", <bc>)] 1{{<read>}}"
        );
        let compiled = compile_geom(geometry).unwrap();
        assert!(compiled.transformation.unwrap()[0].header.is_some());
    }
}

#[test]
fn output_header_templates_require_efgdl_two_and_matched_labels() {
    let legacy = "1{b<bc>[2]r<read>:} -> #[header = append(\" tag:\", <bc>)] 1{<read>}";
    assert!(compile_geom(legacy.to_string()).is_err());

    let unmatched = "header { efgdl = 2 } missing = b[2] 1{r<read>:} -> #[header = append(<missing>)] 1{<read>}";
    assert!(compile_geom(unmatched.to_string()).is_err());
}

#[test]
fn test_simplified_geom_with_transformation() {
    let geom = String::from(
        "1{b<brc>[9-10]f[CAGAGC]u<umi>[8]b<brc2>[10]}2{r<read>:} -> 1{<brc><brc2><umi>}2{<read>}",
    );

    let res = compile_geom(geom);

    assert!(res.is_ok());
    assert_eq!(
        "1{b[11]b[10]u[8]}2{r:}",
        res.ok().unwrap().get_simplified_description_string()
    );
}

#[test]
fn test_desc_with_remove() {
    let geom = String::from("1{b[9-10]f[CAGAGC]remove(u[8])b[10]}2{r:}");

    let res = compile_geom(geom);

    assert!(res.is_ok());
    assert_eq!(
        "1{b[11]b[10]}2{r:}",
        res.ok().unwrap().get_simplified_description_string()
    );
}

#[test]
fn test_desc_with_pad() {
    let geom = String::from("1{b[9-10]f[CAGAGC]remove(u[8])pad_to(b[10], 13, A)}2{r:}");

    let res = compile_geom(geom);

    assert!(res.is_ok());
    assert_eq!(
        "1{b[11]b[13]}2{r:}",
        res.ok().unwrap().get_simplified_description_string()
    );
}

#[test]
fn test_desc_with_trunc() {
    let geom = String::from("1{b[9-10]f[CAGAGC]remove(u[8])trunc(b[10], 3)}2{r:}");

    let res = compile_geom(geom);

    assert!(res.is_ok());
    assert_eq!(
        "1{b[11]b[7]}2{r:}",
        res.ok().unwrap().get_simplified_description_string()
    );
}

#[test]
fn test_simplified_geom_from_def() {
    let geom = String::from("brc = b[9-10] 1{<brc>f[CAGAGC]remove(u[8])b[10]}2{r:}");

    let res = compile_geom(geom);

    assert!(res.is_ok());
    assert_eq!(
        "1{b[11]b[10]}2{r:}",
        res.ok().unwrap().get_simplified_description_string()
    );
}

#[test]
fn test_anchor_relative_basic() {
    // Test that search(relative) + hamming annotation compiles successfully
    let geom = String::from(
        "#[search(relative)] #[hamming(3)] l1 = f[GTGGCCGATGTTTCGCATCGGCGTACGACT]\n1{r:}2{u[10]b[8]<l1>b[8]}",
    );

    let res = compile_geom(geom);

    assert!(res.is_ok());
}

#[test]
fn test_anchor_relative_with_label() {
    // Test anchor_relative with a labeled linker
    let geom = String::from(
        "
#[search(relative)] #[hamming(3)] l1 = f[GTGGCCGATGTTTCGCATCGGCGTACGACT]
1{r:}2{u[10]b[8]<l1>b[8]}
",
    );

    let res = compile_geom(geom);

    assert!(res.is_ok());
}

#[test]
fn test_dual_anchor_relative() {
    // Test geometry with two anchor_relative calls (SPLiT-seq style)
    let geom = String::from(
        "
#[search(relative)] #[hamming(3)] l1 = f[GTGGCCGATGTTTCGCATCGGCGTACGACT]
#[search(relative)] #[hamming(3)] l2 = f[ATCCACGTGCTTGAGAGGCCAGAGCATTCG]
1{r:}2{u[10]b[8]<l1>b[8]<l2>b[8]}
",
    );

    let res = compile_geom(geom);

    assert!(res.is_ok());
}

#[test]
fn test_edit_distance_basic() {
    // Test that #[edit(N)] annotation compiles successfully
    let geom =
        String::from("#[edit(2)] anchor = f[GTGGCCGATGTTTCGCATCGGCGTACGACT]\n1{r:}2{<anchor>b[8]}");

    let res = compile_geom(geom);

    assert!(res.is_ok());
}

#[test]
fn test_edit_distance_rejects_barcode() {
    // edit() must only accept a fixed sequence (f[...]), not a barcode (b[...])
    let src = "1{r:}2{edit(b[8], 2)r:}";

    let ParsedInput {
        parse_res,
        lex_errs: _,
        parse_errs: _,
    } = result_with_errs(src);
    let res = parse_res.unwrap();
    let res = compile(res);

    assert!(
        res.is_err(),
        "edit() should reject barcode (non-sequence) arguments"
    );
}

#[test]
fn test_edit_distance_anchor_relative() {
    // Test search(relative) + edit annotation (indel-tolerant anchor search)
    let geom = String::from(
        "#[search(relative)] #[edit(3)] l1 = f[GTGGCCGATGTTTCGCATCGGCGTACGACT]\n1{r:}2{u[10]b[8]<l1>b[8]}",
    );

    let res = compile_geom(geom);

    assert!(res.is_ok());
}

#[test]
fn test_edit_distance_with_label() {
    // Test edit distance with a labeled linker
    let geom = String::from(
        "
#[search(relative)] #[edit(2)] l1 = f[GTGGCCGATGTTTCGCATCGGCGTACGACT]
1{r:}2{u[10]b[8]<l1>b[8]}
",
    );

    let res = compile_geom(geom);

    assert!(res.is_ok());
}

#[test]
fn test_dual_anchor_with_edit() {
    // Test geometry with two anchor_relative calls using edit distance (for indel-tolerant long-read)
    let geom = String::from(
        "
#[search(relative)] #[edit(3)] l1 = f[GTGGCCGATGTTTCGCATCGGCGTACGACT]
#[search(relative)] #[edit(3)] l2 = f[ATCCACGTGCTTGAGAGGCCAGAGCATTCG]
1{r:}2{u[10]b[8]<l1>b[8]<l2>b[8]}
",
    );

    let res = compile_geom(geom);

    assert!(res.is_ok());
}

#[test]
fn hamming_distance_cannot_exceed_sequence_length() {
    let res = compile_geom(String::from("1{hamming(f[ACG], 10)r:}2{r:}"));
    assert!(res.is_err());

    // A distance equal to the sequence length is degenerate but defined.
    let res = compile_geom(String::from("1{hamming(f[ACG], 3)r:}2{r:}"));
    assert!(res.is_ok());
}

#[test]
fn edit_distance_cannot_exceed_sequence_length() {
    let res = compile_geom(String::from("#[edit(10)]\nl1 = f[ACG]\n1{<l1>r:}2{r:}"));
    assert!(res.is_err());
}

#[test]
fn map_mismatch_cannot_exceed_interval_length() {
    let res = compile_geom(String::from(
        "1{map_with_mismatch(b[8], \"wl.txt\", self, 100)r:}2{r:}",
    ));
    assert!(res.is_err());
}

#[test]
fn inverted_range_is_a_compile_error() {
    let res = compile_geom(String::from("1{b[12-8]f[ACGT]r:}2{r:}"));
    assert!(res.is_err());
}

#[test]
fn bare_self_in_read_is_a_compile_error_not_a_panic() {
    let res = compile_geom(String::from("1{self}2{r:}"));
    assert!(res.is_err());
}

#[test]
fn indexed_capture_in_definition_is_a_compile_error_not_a_panic() {
    let res = compile_geom(String::from("foo = <bar[2]>\n1{b[4]r:}2{r:}"));
    assert!(res.is_err());
}

#[test]
fn simplified_description_tolerates_fixed_seq_labels_in_transform() {
    let compiled = compile_geom(String::from(
        "1{b<bc>[4]f<link>[ACGT]r<rd>:}\n-> 1{<bc><link><rd>}",
    ))
    .unwrap();
    // Fixed sequences are normalized away; this must not panic.
    let _ = compiled.get_simplified_description_string();
}

#[test]
fn legacy_and_explicit_ambiguity_policy_syntaxes_compile_equivalently() {
    for annotation in [
        "#[ambig_policy = first]",
        "#[ambig_policy(first)]",
        "#[ambig_policy = random(seed = 42)]",
        "#[ambig_policy(random, 42)]",
    ] {
        let geometry =
            format!("{annotation} bc = filter_within_dist(b[4], \"wl.txt\", 1)\n1{{<bc>r:}}");
        assert!(
            compile_geom(geometry).is_ok(),
            "failed syntax: {annotation}"
        );
    }
}

#[test]
fn unknown_annotations_are_compile_errors() {
    let definition = compile_geom(String::from("#[haming(1)] a = f[ACGT]\n1{<a>r:}"));
    assert!(definition.is_err());
    let read = compile_geom(String::from("#[match_orientation(either)] 1{b[4]r:}"));
    assert!(read.is_err());
}

#[test]
fn input_read_indices_must_be_contiguous_and_ordered() {
    assert!(compile_geom(String::from("2{b[4]r:}")).is_err());
    assert!(compile_geom(String::from("1{b[4]r:}1{r:}")).is_err());
    assert!(compile_geom(String::from("2{b[4]r:}1{r:}")).is_err());
}

#[test]
fn match_blocks_reject_attributes_without_runtime_producers() {
    let geometry = String::from(
        "header { efgdl = 2 }\n#[match_ori(either)] 1{b<bc>[4]r:}\n-> match 1.other { fw => 1{<bc>}, rc => 1{<bc>} }",
    );
    let error = format!("{:?}", compile_geom(geometry).unwrap_err());
    assert!(error.contains("only the 'ori' attribute"), "{error}");
}

#[test]
fn uppercase_nucleotide_prefixed_definition_names_compile() {
    let compiled = compile_geom(String::from("Anchor1 = f[ACGT]\n1{<Anchor1>r:}"));
    assert!(compiled.is_ok(), "{compiled:?}");
}

#[test]
fn excessive_nesting_returns_a_bounded_diagnostic() {
    let geometry = format!(
        "header {{ efgdl = 2 }}\n1{{{}b[1]{}r:}}",
        "(".repeat(129),
        ")".repeat(129)
    );
    let error = format!("{:?}", compile_geom(geometry).unwrap_err());
    assert!(
        error.contains("nesting exceeds the supported depth"),
        "{error}"
    );
}

#[test]
fn malformed_leading_header_has_header_context() {
    let error = format!(
        "{:?}",
        compile_geom_typed("header { efgdl = 2\n1{b[1]r:}").unwrap_err()
    );
    assert!(error.contains("leading `header"), "{error}");
}
