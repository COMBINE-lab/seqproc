use chumsky::{prelude::*, Stream};
use seqproc::{
    lexer::lexer,
    parser::{parser, Definition, Expr, Function, IntervalKind, IntervalShape, Read},
    Nucleotide, S,
};

#[test]
fn definition() {
    let src = "brc = b[10] 1{<brc>}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (Some(res), parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
    else {
        panic!()
    };

    let expected_res = S(
        vec![S(
            Definition {
                label: S("brc".to_string(), 0..3),
                expr: S(
                    Expr::GeomPiece(IntervalKind::Barcode, IntervalShape::FixedLen(S(10, 8..10))),
                    6..11,
                ),
            },
            0..11,
        )],
        0..11,
    );

    assert!(lex_err.is_empty());
    assert!(parser_err.is_empty());
    assert_eq!(res.definitions, expected_res);
}

#[test]
fn transformation() {
    let src = "1{b[1]}2{r:} -> 1{<t>}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (Some(res), parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
    else {
        panic!()
    };

    let expected_res = vec![
        S(
            Read {
                index: S(1, 16..17),
                exprs: vec![S(Expr::Label(S("t".to_string(), 18..21)), 18..21)],
            },
            16..22,
        ),
        S(
            Read {
                index: S(2, 22..23),
                exprs: vec![S(
                    Expr::GeomPiece(IntervalKind::ReadSeq, IntervalShape::UnboundedLen),
                    24..26,
                )],
            },
            22..27,
        ),
    ];

    assert!(lex_err.is_empty());
    assert!(parser_err.is_empty());
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

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (_, parser_err) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    assert!(lex_err.is_empty());
    assert!(parser_err.is_empty());
}

#[test]
fn hamming() {
    let src = "1{hamming(<brc>, 1)}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (Some(res), parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
    else {
        panic!()
    };

    let expected_res = Read {
        index: S(1, 0..1),
        exprs: vec![S(
            Expr::Function(
                S(Function::Hamming(1), 2..9),
                S(Box::new(Expr::Label(S("brc".to_string(), 10..15))), 10..18),
            ),
            2..19,
        )],
    };

    assert!(lex_err.is_empty());
    assert!(parser_err.is_empty());
    assert_eq!(res.reads.0[0].0, expected_res);
}

#[test]
fn remove() {
    let src = "1{remove(<brc>)}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (Some(res), parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
    else {
        panic!()
    };

    let expected_res = Read {
        index: S(1, 0..1),
        exprs: vec![S(
            Expr::Function(
                S(Function::Remove, 2..8),
                S(Box::new(Expr::Label(S("brc".to_string(), 9..14))), 9..14),
            ),
            2..15,
        )],
    };

    assert!(lex_err.is_empty());
    assert!(parser_err.is_empty());
    assert_eq!(res.reads.0[0].0, expected_res);
}

#[test]
fn illegal_nest() {
    let src = "1{hamming(pad(<brc>, 1), 1)}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (_, parser_err) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    assert!(lex_err.is_empty());
    assert_eq!(1, parser_err.len());
}

#[test]
fn nested() {
    let src = "1{rev(norm(<brc>))}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (Some(res), parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
    else {
        panic!()
    };

    let expected_res = Read {
        index: S(1, 0..1),
        exprs: vec![S(
            Expr::Function(
                S(Function::Reverse, 2..5),
                S(
                    Box::new(Expr::Function(
                        S(Function::Normalize, 6..10),
                        S(Box::new(Expr::Label(S("brc".to_string(), 11..16))), 11..16),
                    )),
                    6..17,
                ),
            ),
            2..18,
        )],
    };

    assert!(lex_err.is_empty());
    assert!(parser_err.is_empty());
    assert_eq!(res.reads.0[0].0, expected_res);
}

#[test]
fn labeled_unbounded() {
    let src = "1{b<barcode>:}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (Some(res), parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
    else {
        panic!()
    };

    let expected_res = Read {
        index: S(1, 0..1),
        exprs: vec![S(
            Expr::LabeledGeomPiece(
                S("barcode".to_string(), 3..12),
                S(
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

    assert!(lex_err.is_empty());
    assert!(parser_err.is_empty());
    assert_eq!(res.reads.0[0].0, expected_res);
}

#[test]
fn ranged() {
    let src = "1{b[10-11]}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (Some(res), parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
    else {
        panic!()
    };

    let expected_res = Read {
        index: S(1, 0..1),
        exprs: vec![S(
            Expr::GeomPiece(
                IntervalKind::Barcode,
                IntervalShape::RangedLen(S((10, 11), 4..9)),
            ),
            2..10,
        )],
    };

    assert!(lex_err.is_empty());
    assert!(parser_err.is_empty());
    assert_eq!(res.reads.0[0].0, expected_res);
}

#[test]
fn fixed() {
    let src = "1{r[10]}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (Some(res), parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
    else {
        panic!()
    };

    let expected_res = Read {
        index: S(1, 0..1),
        exprs: vec![S(
            Expr::GeomPiece(IntervalKind::ReadSeq, IntervalShape::FixedLen(S(10, 4..6))),
            2..7,
        )],
    };

    assert!(lex_err.is_empty());
    assert!(parser_err.is_empty());
    assert_eq!(res.reads.0[0].0, expected_res);
}

#[test]
fn fixed_seq() {
    let src = "1{f[GACTU]}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (Some(res), parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
    else {
        panic!()
    };

    let expected_res = Read {
        index: S(1, 0..1),
        exprs: vec![S(
            Expr::GeomPiece(
                IntervalKind::FixedSeq,
                IntervalShape::FixedSeq(S(
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

    assert!(lex_err.is_empty());
    assert!(parser_err.is_empty());
    assert_eq!(res.reads.0[0].0, expected_res);
}

#[test]
fn fail_ranged_seq() {
    let src = "1{f[1-2]}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (_, parser_err) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    assert!(lex_err.is_empty());
    assert_eq!(1, parser_err.len());
}

#[test]
fn allow_expr_arg() {
    let src = "1{map(b[9-10], \"filepath\", norm(self))}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (_, parser_err) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    assert!(lex_err.is_empty());
    assert!(parser_err.is_empty());
}

#[test]
fn fail_map() {
    let src = "1{map(pad(b[9-10], 3), \"filepath\", norm(self))}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (_, parser_err) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    assert!(lex_err.is_empty());
    assert_eq!(1, parser_err.len());
}
