mod common;

use seqproc::{
    parser::{Definition, Expr, Function, IntervalKind, IntervalShape, Read},
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
                index: S::new(1, 16..17),
                exprs: vec![S::new(Expr::Label(S::new("t".to_string(), 18..21)), 18..21)],
            },
            16..22,
        ),
        S::new(
            Read {
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
    assert_eq!(res.transforms.unwrap().0, expected_res);
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
