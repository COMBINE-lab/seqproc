mod common;

use seqproc::{
    parser::{Definition, Expr, Function, IntervalKind, IntervalShape, Read, TransformOutput},
    Nucleotide, S,
};

use crate::common::utils::{result_with_errs, ParsedInput};

#[test]
fn definition() {
    let src = "brc = b[10] 1{<brc>}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    let res = match parse_res {
        Some(res) => res,
        None => panic!(),
    };

    let expected_res = S::new(
        vec![S::new(
            Definition {
                annotations: vec![],
                label: S::new("brc".to_string(), 0..3),
                expr: S::new(
                    Expr::GeomPiece(
                        IntervalKind::Barcode,
                        IntervalShape::FixedLen(S::new(10, 8..10)),
                    ),
                    6..11,
                ),
            },
            0..11,
        )],
        0..11,
    );

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());
    assert_eq!(res.definitions, expected_res);
}

#[test]
fn transformation() {
    let src = "1{b[1]}2{r:} -> 1{<t>}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    let res = match parse_res {
        Some(res) => res,
        None => panic!(),
    };

    let expected_res = vec![
        S::new(
            Read {
                annotations: vec![],
                index: S::new(1, 16..17),
                exprs: vec![S::new(Expr::Label(S::new("t".to_string(), 18..21)), 18..21)],
            },
            16..22,
        ),
        S::new(
            Read {
                annotations: vec![],
                index: S::new(2, 22..23),
                exprs: vec![S::new(
                    Expr::GeomPiece(IntervalKind::ReadSeq, IntervalShape::UnboundedLen),
                    24..26,
                )],
            },
            22..27,
        ),
    ];

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());
    match res.transforms.unwrap().0 {
        TransformOutput::Direct(reads) => assert_eq!(reads, expected_res),
        _ => panic!("Expected TransformOutput::Direct"),
    }
}

#[test]
fn valid() {
    let src = "
test = b[10]
another = remove(u[9-11])
1{<test>f[CAGAGC]<another>}2{r:}
    -> 1{<another><test>}
        ";

    let ParsedInput {
        parse_res: _,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());
}

#[test]
fn hamming() {
    let src = "1{hamming(<brc>, 1)}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    let res = match parse_res {
        Some(res) => res,
        None => panic!(),
    };

    let expected_res = Read {
        annotations: vec![],
        index: S::new(1, 0..1),
        exprs: vec![S::new(
            Expr::Function(
                S::new(Function::Hamming(1), 2..9),
                S::new(
                    Box::new(Expr::Label(S::new("brc".to_string(), 10..15))),
                    10..18,
                ),
            ),
            2..19,
        )],
    };

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());
    assert_eq!(res.reads.0[0].0, expected_res);
}

#[test]
fn remove() {
    let src = "1{remove(<brc>)}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    let res = match parse_res {
        Some(res) => res,
        None => panic!(),
    };

    let expected_res = Read {
        annotations: vec![],
        index: S::new(1, 0..1),
        exprs: vec![S::new(
            Expr::Function(
                S::new(Function::Remove, 2..8),
                S::new(
                    Box::new(Expr::Label(S::new("brc".to_string(), 9..14))),
                    9..14,
                ),
            ),
            2..15,
        )],
    };

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());
    assert_eq!(res.reads.0[0].0, expected_res);
}

#[test]
fn illegal_nest() {
    let src = "1{hamming(pad(<brc>>, 1, A), 1)}2{r:}";

    let ParsedInput {
        parse_res: _,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty());
    assert_eq!(1, parse_errs.len());
}

#[test]
fn nested() {
    let src = "1{rev(norm(<brc>))}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    let res = match parse_res {
        Some(res) => res,
        None => panic!(),
    };

    let expected_res = Read {
        annotations: vec![],
        index: S::new(1, 0..1),
        exprs: vec![S::new(
            Expr::Function(
                S::new(Function::Reverse, 2..5),
                S::new(
                    Box::new(Expr::Function(
                        S::new(Function::Normalize, 6..10),
                        S::new(
                            Box::new(Expr::Label(S::new("brc".to_string(), 11..16))),
                            11..16,
                        ),
                    )),
                    6..17,
                ),
            ),
            2..18,
        )],
    };

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());
    assert_eq!(res.reads.0[0].0, expected_res);
}

#[test]
fn labeled_unbounded() {
    let src = "1{b<barcode>:}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    let res = match parse_res {
        Some(res) => res,
        None => panic!(),
    };

    let expected_res = Read {
        annotations: vec![],
        index: S::new(1, 0..1),
        exprs: vec![S::new(
            Expr::LabeledGeomPiece(
                S::new("barcode".to_string(), 3..12),
                S::new(
                    Box::new(Expr::GeomPiece(
                        IntervalKind::Barcode,
                        IntervalShape::UnboundedLen,
                    )),
                    2..13,
                ),
            ),
            2..13,
        )],
    };

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());
    assert_eq!(res.reads.0[0].0, expected_res);
}

#[test]
fn ranged() {
    let src = "1{b[10-11]}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    let res = match parse_res {
        Some(res) => res,
        None => panic!(),
    };

    let expected_res = Read {
        annotations: vec![],
        index: S::new(1, 0..1),
        exprs: vec![S::new(
            Expr::GeomPiece(
                IntervalKind::Barcode,
                IntervalShape::RangedLen(S::new((10, 11), 4..9)),
            ),
            2..10,
        )],
    };

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());
    assert_eq!(res.reads.0[0].0, expected_res);
}

#[test]
fn fixed() {
    let src = "1{r[10]}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    let res = match parse_res {
        Some(res) => res,
        None => panic!(),
    };

    let expected_res = Read {
        annotations: vec![],
        index: S::new(1, 0..1),
        exprs: vec![S::new(
            Expr::GeomPiece(
                IntervalKind::ReadSeq,
                IntervalShape::FixedLen(S::new(10, 4..6)),
            ),
            2..7,
        )],
    };

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());
    assert_eq!(res.reads.0[0].0, expected_res);
}

#[test]
fn fixed_seq() {
    let src = "1{f[GACTU]}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    let res = match parse_res {
        Some(res) => res,
        None => panic!(),
    };

    let expected_res = Read {
        annotations: vec![],
        index: S::new(1, 0..1),
        exprs: vec![S::new(
            Expr::GeomPiece(
                IntervalKind::FixedSeq,
                IntervalShape::FixedSeq(S::new(
                    vec![
                        Nucleotide::G,
                        Nucleotide::A,
                        Nucleotide::C,
                        Nucleotide::T,
                        Nucleotide::U,
                    ],
                    4..9,
                )),
            ),
            2..10,
        )],
    };

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());
    assert_eq!(res.reads.0[0].0, expected_res);
}

#[test]
fn fail_ranged_seq() {
    let src = "1{f[1-2]}2{r:}";

    let ParsedInput {
        parse_res: _,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty());
    assert_eq!(1, parse_errs.len());
}

#[test]
fn allow_expr_arg() {
    let src = "1{map(b[9-10], \"filepath\", norm(self))}2{r:}";

    let ParsedInput {
        parse_res: _,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());
}

#[test]
fn fail_prefix_label_underscore() {
    let src = "_brc = b[10] 1{<brc>}2{r:}";

    let ParsedInput {
        parse_res: _,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty());
    assert_eq!(1, parse_errs.len());
}

#[test]
fn fail_prefix_inlinelabel_underscore() {
    let src = "1{b<_brc>[10]}2{r:}";

    let ParsedInput {
        parse_res: _,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty());
    assert_eq!(1, parse_errs.len());
}

#[test]
fn ok_mid_inlinelabel_underscore() {
    let src = "1{b<b_rc>[10]}2{r:}";

    let ParsedInput {
        parse_res: _,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());
}

#[test]
fn ok_mid_label_underscore() {
    let src = "b_rc = b[10] 1{<brc>}2{r:}";

    let ParsedInput {
        parse_res: _,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());
}

#[test]
fn filter_test() {
    let src = "b_rc = filter(b[10], $0) 1{<brc>}2{r:}";

    let ParsedInput {
        parse_res: _,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());
}

#[test]
fn filter_test_too_many_args() {
    let src = "b_rc = filter(b[10], $0, 1) 1{<brc>}2{r:}";

    let ParsedInput {
        parse_res: _,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty());
    assert_eq!(1, parse_errs.len());
}

#[test]
fn annotation_on_read() {
    // Annotation syntax: #[match_ori(either)] before a read declaration.
    let src = "#[match_ori(either)] 1{b[10]r:}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());

    let res = parse_res.unwrap();
    // First read should have one annotation
    assert_eq!(res.reads.0[0].0.annotations.len(), 1);
    assert_eq!(res.reads.0[0].0.annotations[0].0.name.0, "match_ori");
    assert_eq!(res.reads.0[0].0.annotations[0].0.args.len(), 1);
    assert_eq!(res.reads.0[0].0.annotations[0].0.args[0].0, "either");
    // Second read should have no annotations
    assert_eq!(res.reads.0[1].0.annotations.len(), 0);
}

#[test]
fn annotation_simplified_with_transform() {
    // Simplified form (recommended): annotation + direct transform, no match block.
    let src = "#[match_ori(either)] 1{u[10]b[8]f[CAGAGC]b[8]r:}2{r:} -> 1{<umi><bc1><bc2>}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());

    let res = parse_res.unwrap();
    assert_eq!(res.reads.0[0].0.annotations.len(), 1);
    assert!(res.transforms.is_some());
    match &res.transforms.unwrap().0 {
        TransformOutput::Direct(_) => {}
        _ => panic!("Expected TransformOutput::Direct"),
    }
}

#[test]
fn match_block_transform() {
    // Match block syntax: -> match 1.ori { fw => 1{...}, rc => 1{...} }
    let src = "#[match_ori(either)] 1{u[10]b[8]r:}2{r:} -> match 1.ori { fw => 1{<umi><bc>}2{r:}, rc => 1{<umi><bc>}2{r:} }";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());

    let res = parse_res.unwrap();
    assert!(res.transforms.is_some());
    match &res.transforms.unwrap().0 {
        TransformOutput::Match {
            read_ref,
            attr,
            fw_arm,
            rc_arm,
        } => {
            assert_eq!(read_ref.0, 1);
            assert_eq!(attr.0, "ori");
            assert!(!fw_arm.is_empty());
            assert!(!rc_arm.is_empty());
        }
        _ => panic!("Expected TransformOutput::Match"),
    }
}

#[test]
fn no_annotation_backward_compat() {
    // Existing syntax without annotations should still work.
    let src = "1{b[16]u[10]r:}2{r:} -> 1{<barcode><umi>}2{r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());

    let res = parse_res.unwrap();
    // No annotations
    assert!(res.reads.0[0].0.annotations.is_empty());
    assert!(res.reads.0[1].0.annotations.is_empty());
    // Direct transform
    match &res.transforms.unwrap().0 {
        TransformOutput::Direct(_) => {}
        _ => panic!("Expected TransformOutput::Direct"),
    }
}

#[test]
fn annotation_on_definition() {
    // Single annotation before a definition.
    let src = "#[edit(5)] linker1 = f[CAGAGC]\n1{<linker1>r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty(), "lex errors: {:?}", lex_errs);
    assert!(parse_errs.is_empty(), "parse errors: {:?}", parse_errs);

    let res = parse_res.unwrap();
    // Definition should have one annotation
    assert_eq!(res.definitions.0.len(), 1);
    assert_eq!(res.definitions.0[0].0.annotations.len(), 1);
    assert_eq!(res.definitions.0[0].0.annotations[0].0.name.0, "edit");
    assert_eq!(res.definitions.0[0].0.annotations[0].0.args.len(), 1);
    assert_eq!(res.definitions.0[0].0.annotations[0].0.args[0].0, "5");
    // Label should still be correct
    assert_eq!(res.definitions.0[0].0.label.0, "linker1");
}

#[test]
fn annotation_stacked_on_definition() {
    // Multiple annotations stacked on a single definition.
    let src = "#[search(relative)] #[edit(5)] linker1 = f[CAGAGC]\n1{<linker1>r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty(), "lex errors: {:?}", lex_errs);
    assert!(parse_errs.is_empty(), "parse errors: {:?}", parse_errs);

    let res = parse_res.unwrap();
    assert_eq!(res.definitions.0.len(), 1);
    assert_eq!(res.definitions.0[0].0.annotations.len(), 2);
    assert_eq!(res.definitions.0[0].0.annotations[0].0.name.0, "search");
    assert_eq!(res.definitions.0[0].0.annotations[1].0.name.0, "edit");
}

#[test]
fn annotation_on_definition_and_read() {
    // Annotation on both a definition and a read.
    let src = "#[edit(3)] linker1 = f[CAGAGC]\n#[match_ori(either)] 1{<linker1>r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty(), "lex errors: {:?}", lex_errs);
    assert!(parse_errs.is_empty(), "parse errors: {:?}", parse_errs);

    let res = parse_res.unwrap();
    // Definition has its annotation
    assert_eq!(res.definitions.0[0].0.annotations.len(), 1);
    assert_eq!(res.definitions.0[0].0.annotations[0].0.name.0, "edit");
    // Read has its annotation
    assert_eq!(res.reads.0[0].0.annotations.len(), 1);
    assert_eq!(res.reads.0[0].0.annotations[0].0.name.0, "match_ori");
}

#[test]
fn definition_without_annotation_backward_compat() {
    // Definitions without annotations should still work (no annotations field populated).
    let src = "brc = b[10]\n1{<brc>r:}";

    let ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    } = result_with_errs(src);

    assert!(lex_errs.is_empty());
    assert!(parse_errs.is_empty());

    let res = parse_res.unwrap();
    assert_eq!(res.definitions.0.len(), 1);
    assert!(res.definitions.0[0].0.annotations.is_empty());
}

#[test]
fn annotation_arg_accepts_keyword_tokens() {
    // BUG 3: annotation_arg was missing keyword tokens that annotation_name
    // accepts (rev, revcomp, norm, filter, anchor_relative). An annotation
    // like #[something(rev)] should parse successfully because "rev" is a
    // valid annotation argument, not just a valid annotation name.
    let keywords_as_args = vec![
        ("#[foo(rev)] 1{b[10]r:}", "rev"),
        ("#[foo(revcomp)] 1{b[10]r:}", "revcomp"),
        ("#[foo(norm)] 1{b[10]r:}", "norm"),
        ("#[foo(filter)] 1{b[10]r:}", "filter"),
        ("#[foo(anchor_relative)] 1{b[10]r:}", "anchor_relative"),
    ];

    for (src, expected_arg) in keywords_as_args {
        let ParsedInput {
            parse_res,
            lex_errs,
            parse_errs,
        } = result_with_errs(src);

        assert!(
            lex_errs.is_empty(),
            "lex errors for '{src}': {:?}",
            lex_errs
        );
        assert!(
            parse_errs.is_empty(),
            "parse errors for '{src}': {:?}",
            parse_errs
        );

        let res = parse_res.unwrap();
        assert_eq!(
            res.reads.0[0].0.annotations[0].0.args[0].0, expected_arg,
            "annotation arg mismatch for '{src}'"
        );
    }
}
