use std::ops::Deref;

use chumsky::{prelude::*, Stream};
use seqproc::{
    lexer::lexer,
    parser::{parser, Expr, Function, Size, Type},
};

#[test]
fn definition() {
    let src = "brc = b[10] 1{<brc>}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let expected_res = (
        Expr::Definitions(vec![(
            Expr::LabeledGeomPiece(
                Box::new(Expr::Label(("brc".to_string(), 0..3))),
                Box::new((
                    Expr::GeomPiece(Type::Barcode, Size::FixedLen((10, 8..10))),
                    6..11,
                )),
            ),
            0..11,
        )]),
        0..11,
    );

    let res = if let Expr::Description(d, _r, _t) = res.clone().unwrap().0 {
        d
    } else {
        unreachable!()
    };

    let res = if let Some(def) = res.deref() {
        def
    } else {
        panic!("No definitions in {}", src.clone())
    };

    assert_eq!(0, lex_err.len());
    assert_eq!(0, parser_err.len());
    assert_eq!(res, &expected_res);
}

#[test]
fn transformation() {
    let src = "1{b[1]}2{r:} -> 1{<t>}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let expected_res = Expr::Transform(vec![Expr::Read(
        (1, 13..17),
        vec![(Expr::Label(("t".to_string(), 18..21)), 18..21)],
    )]);

    let res = if let Expr::Description(_d, _r, t) = res.clone().unwrap().0 {
        t
    } else {
        unreachable!()
    };

    let res = if let (Some(trans), _) = res.deref() {
        trans
    } else {
        panic!("No transformation in {}", src.clone())
    };

    assert_eq!(0, lex_err.len());
    assert_eq!(0, parser_err.len());
    assert_eq!(res, &expected_res);
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

    assert_eq!(0, lex_err.len());
    assert_eq!(0, parser_err.len());
}

#[test]
fn hamming() {
    let src = "1{hamming(<brc>, 1)}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let expected_res = Expr::Read(
        (1, 0..1),
        vec![(
            Expr::Function(
                (Function::Hamming(1), 2..9),
                Box::new((Expr::Label(("brc".to_string(), 10..15)), 10..18)),
            ),
            2..19,
        )],
    );

    let res = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r
    } else {
        unreachable!()
    };

    assert_eq!(0, lex_err.len());
    assert_eq!(0, parser_err.len());
    assert_eq!(res.0.first().unwrap(), &expected_res);
}

#[test]
fn remove() {
    let src = "1{remove(<brc>)}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let expected_res = Expr::Read(
        (1, 0..1),
        vec![(
            Expr::Function(
                (Function::Remove, 2..8),
                Box::new((Expr::Label(("brc".to_string(), 9..14)), 9..14)),
            ),
            2..15,
        )],
    );

    let res = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r
    } else {
        unreachable!()
    };

    let res = res.0.first().unwrap();

    assert_eq!(0, lex_err.len());
    assert_eq!(0, parser_err.len());
    assert_eq!(res, &expected_res);
}

#[test]
fn illegal_nest() {
    let src = "1{hamming(pad(<brc>, 1), 1)}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (_, parser_err) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    assert_eq!(0, lex_err.len());
    assert_eq!(1, parser_err.len());
}

#[test]
fn nested() {
    let src = "1{rev(norm(<brc>))}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let expected_res = Expr::Read(
        (1, 0..1),
        vec![(
            Expr::Function(
                (Function::Reverse, 2..5),
                Box::new((
                    Expr::Function(
                        (Function::Normalize, 6..10),
                        Box::new((Expr::Label(("brc".to_string(), 11..16)), 11..16)),
                    ),
                    6..17,
                )),
            ),
            2..18,
        )],
    );

    let res = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r
    } else {
        unreachable!()
    };

    let res = res.0.first().unwrap();

    assert_eq!(0, lex_err.len());
    assert_eq!(0, parser_err.len());
    assert_eq!(res, &expected_res);
}

#[test]
fn labeled_unbounded() {
    let src = "1{b<barcode>:}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let expected_res = Expr::Read(
        (1, 0..1),
        vec![(
            Expr::LabeledGeomPiece(
                Box::new(Expr::Label(("barcode".to_string(), 3..12))),
                Box::new((Expr::GeomPiece(Type::Barcode, Size::UnboundedLen), 2..13)),
            ),
            2..13,
        )],
    );

    let res = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r
    } else {
        unreachable!()
    };

    let res = res.0.first().unwrap();

    assert_eq!(0, lex_err.len());
    assert_eq!(0, parser_err.len());
    assert_eq!(res, &expected_res);
}

#[test]
fn ranged() {
    let src = "1{b[10-11]}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let expected_res = Expr::Read(
        (1, 0..1),
        vec![(
            Expr::GeomPiece(Type::Barcode, Size::RangedLen(((10, 11), 4..9))),
            2..10,
        )],
    );

    let res = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r
    } else {
        unreachable!()
    };

    let res = res.0.first().unwrap();

    assert_eq!(0, lex_err.len());
    assert_eq!(0, parser_err.len());
    assert_eq!(res, &expected_res);
}

#[test]
fn fixed() {
    let src = "1{r[10]}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let expected_res = Expr::Read(
        (1, 0..1),
        vec![(
            Expr::GeomPiece(Type::ReadSeq, Size::FixedLen((10, 4..6))),
            2..7,
        )],
    );

    let res = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r
    } else {
        unreachable!()
    };

    let res = res.0.first().unwrap();

    assert_eq!(0, lex_err.len());
    assert_eq!(0, parser_err.len());
    assert_eq!(res, &expected_res);
}

#[test]
fn fixed_seq() {
    let src = "1{f[GATCU]}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, parser_err) =
        parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let expected_res = Expr::Read(
        (1, 0..1),
        vec![(
            Expr::GeomPiece(Type::FixedSeq, Size::FixedSeq(("GATCU".to_string(), 4..9))),
            2..10,
        )],
    );

    let res = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r
    } else {
        unreachable!()
    };

    let res = res.0.first().unwrap();

    assert_eq!(0, lex_err.len());
    assert_eq!(0, parser_err.len());
    assert_eq!(res, &expected_res);
}

#[test]
fn fail_ranged_seq() {
    let src = "1{f[1-2]}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (_, parser_err) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    assert_eq!(0, lex_err.len());
    assert_eq!(1, parser_err.len());
}

#[test]
fn allow_expr_arg() {
    let src = "1{map(b[9-10], \"filepath\", norm(self))}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (_, parser_err) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    assert_eq!(0, lex_err.len());
    assert_eq!(0, parser_err.len());
}

#[test]
fn fail_map() {
    let src = "1{map(pad(b[9-10], 3), \"filepath\", norm(self))}2{r:}";

    let (res, lex_err) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (_, parser_err) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    assert_eq!(0, lex_err.len());
    assert_eq!(1, parser_err.len());
}
