//! Defines the parser for EFGDL.

use std::fmt::{self, Write};

use chumsky::prelude::*;

use crate::{lexer::Token, Nucleotide, S};

/// The length of a nucleotide interval,
/// and whether it must match a specific sequence.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IntervalShape {
    /// Interval matches this sequence exactly.
    FixedSeq(S<Vec<Nucleotide>>),
    /// Interval length is exactly this value.
    FixedLen(S<usize>),
    /// Interval length is within this range (inclusive).
    RangedLen(S<(usize, usize)>),
    /// Interval can be of any length.
    UnboundedLen,
}

impl fmt::Display for IntervalShape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use IntervalShape::*;
        match self {
            FixedLen(S(n, _)) => write!(f, "[{n}]"),
            FixedSeq(S(s, _)) => {
                f.write_char('[')?;
                for nuc in s {
                    write!(f, "{nuc}")?;
                }
                f.write_char(']')
            }
            RangedLen(S((a, b), _)) => write!(f, "[{a}-{b}]"),
            UnboundedLen => f.write_char(':'),
        }
    }
}

/// An invocation of an [EFDGL transformation](https://efgdl-spec.readthedocs.io/en/latest/transformations.html).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Function {
    /// `rev(I)`
    Reverse,
    /// `revcomp(I)`
    ReverseComp,
    /// `trunc(I, n)``
    Truncate(usize),
    /// `trunc_left(I, n)`
    TruncateLeft(usize),
    /// `trunc_to(I, n)`
    TruncateTo(usize),
    /// `trunc_to_left(I, n)`
    TruncateToLeft(usize),
    /// `remove(I)`
    Remove,
    /// `pad(I, n, nuc)`
    Pad(usize, Nucleotide),
    /// `pad_left(I, n, nuc)`
    PadLeft(usize, Nucleotide),
    /// `pad_to(I, n, nuc)`
    PadTo(usize, Nucleotide),
    /// `pad_to_left(I, n, nuc)`
    PadToLeft(usize, Nucleotide),
    /// `norm(I)`
    Normalize,
    /// `map(I, A, F)`
    Map(String, S<Box<Expr>>),
    /// `map_with_mismatch(I, A, F, n)`
    MapWithMismatch(String, S<Box<Expr>>, usize),
    /// `map_with_mismatch(I, A, F, n)`
    FilterWithinDist(String, usize),
    /// `hamming(F, n)`
    Hamming(usize),
}

impl Function {
    /// `first` is a formatted representation of the first argment to the function call
    fn fmt(&self, f: &mut fmt::Formatter<'_>, first: fmt::Arguments<'_>) -> fmt::Result {
        use Function::*;
        match self {
            Reverse => write!(f, "rev({first})"),
            ReverseComp => write!(f, "revcomp({first})"),
            Truncate(n) => write!(f, "trunc({first}, {n})"),
            TruncateLeft(n) => write!(f, "trunc_left({first}, {n})"),
            TruncateTo(n) => write!(f, "trunc_to({first}, {n})"),
            TruncateToLeft(n) => write!(f, "trunc_to_left({first}, {n})"),
            Remove => write!(f, "remove({first})"),
            Pad(n, nuc) => write!(f, "pad({first}, {n}, {nuc})"),
            PadLeft(n, nuc) => write!(f, "pad_left({first}, {n}, {nuc})"),
            PadTo(n, nuc) => write!(f, "pad_to({first}, {n}, {nuc})"),
            PadToLeft(n, nuc) => write!(f, "pad_to_left({first}, {n}, {nuc})"),
            Normalize => write!(f, "norm({first})"),
            Map(p, S(b, _)) => {
                write!(f, "map({first}, {p}, {b})")
            }
            MapWithMismatch(p, b, n) => {
                let S(s, _) = b;
                write!(f, "map_with_mismatch({first}, {p}, {s}, {n})")
            }
            FilterWithinDist(p, n) => write!(f, "filter_within_dist({first}, {p}, {n})"),
            Hamming(n) => write!(f, "hamming({first}, {n})"),
        }
    }
}

/// <https://efgdl-spec.readthedocs.io/en/latest/intervals.html>
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum IntervalKind {
    Barcode,
    Umi,
    Discard,
    ReadSeq,
    FixedSeq,
}

impl fmt::Display for IntervalKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use IntervalKind::*;
        match self {
            Barcode => write!(f, "b"),
            Umi => write!(f, "u"),
            Discard => write!(f, "x"),
            ReadSeq => write!(f, "r"),
            FixedSeq => write!(f, "f"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expr {
    /// `self`, as used inside the expression
    /// passed as the third argument to `map` and `map_with_mismatch`.
    Self_,

    /// An inline variable reference/binding: `<my_label>`.
    Label(S<String>),

    /// An interval, with a specifier and a length: `b[10]`, `u[11-13]`, `f[AUCG]`, `r:`.
    GeomPiece(IntervalKind, IntervalShape),

    /// A binding of an interval to an identifier,
    /// either inline (`b<foo>[10]`)
    /// or as a declaration statement (`foo = b[10]`).
    LabeledGeomPiece(Box<Self>, S<Box<Self>>),

    /// A transformation invocation: `hamming(f[CAGAGC], 1)`.
    ///
    /// `.1` is the first argument.
    Function(S<Function>, S<Box<Self>>),

    /// A read, with index and expression: `1{hamming(<brc>, 1)}`.
    Read(S<usize>, Vec<S<Self>>),

    /// The list of definitions at the top of an EFGDL file:
    /// `brc = b[10] foo = f[CAGAGC]`.``
    Definitions(Vec<S<Self>>),

    /// List of reads specifying the output of a transformation:
    /// ` -> 1{<brc>pad(<anchor>, 3, A)<read>}`.
    Transform(Vec<Self>),

    /// A full EFGDL file: 0+ definitions, then input reads, then transformed reads.
    Description(Option<S<Box<Self>>>, S<Vec<Self>>, S<Option<Box<Self>>>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expr::*;
        match self {
            Self_ => write!(f, "self"),
            Label(S(s, _)) => write!(f, "<{s}>"),
            GeomPiece(t, s) => write!(f, "{t}{s}"),
            LabeledGeomPiece(l, S(expr, _)) => {
                write!(f, "{l}={expr}")
            }
            Function(S(fn_, _), S(expr, _)) => fn_.fmt(f, format_args!("{expr}")),
            Read(S(n, _), exprs) => write!(
                f,
                "{n}{{{}}}",
                exprs
                    .iter()
                    .map(|S(x, _)| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Definitions(exprs) => write!(
                f,
                "def(\n{}\n)",
                exprs
                    .iter()
                    .map(|S(x, _)| x.to_string())
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            Transform(exprs) => write!(
                f,
                " -> {}",
                exprs
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Description(d, r, t) => {
                let d_w = if let Some(S(d, _)) = d {
                    Some(format!("{d}"))
                } else {
                    None
                };

                let t_w = if let S(Some(t), _) = t {
                    Some(format!("{t}"))
                } else {
                    None
                };

                let S(r, _) = r;

                let r_w = r
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");

                if let Some(d_s) = d_w {
                    if let Some(t_s) = t_w {
                        write!(f, "{d_s}\n{r_w}\n{t_s}")
                    } else {
                        write!(f, "{d_s}\n{r_w}")
                    }
                } else {
                    write!(f, "{r_w}")
                }
            }
        }
    }
}

pub fn parser() -> impl Parser<Token, S<Expr>, Error = Simple<Token>> + Clone {
    /*
       Start with creating combinators and
       a recursive definition of a geom_piece

       At execution time we will check if it is a valid
       geometry without any ambiguity. Here we will
       restruct some invalid definitions
    */

    let ident = select! { Token::Label(ident) => ident }.labelled("Piece Label");

    let num = select! { Token::Num(n) => n }.labelled("Size Label");

    let file = select! { Token::File(f) => f }.labelled("File Path");

    let argument = select! { Token::Arg(n) => n.to_string() }.labelled("Argument");

    let piece_type = select! {
        Token::Barcode => IntervalKind::Barcode,
        Token::Umi => IntervalKind::Umi,
        Token::Discard => IntervalKind::Discard,
        Token::ReadSeq => IntervalKind::ReadSeq,
    }
    .labelled("Piece Type");

    let nuc = select! {
        Token::U => Nucleotide::U,
        Token::A => Nucleotide::A,
        Token::T => Nucleotide::T,
        Token::G => Nucleotide::G,
        Token::C => Nucleotide::C,
    };

    let label = ident
        .map_with_span(|l, span| Expr::Label(S(l, span)))
        .labelled("Label");

    let self_ = just(Token::Self_).to(Expr::Self_).labelled("Self");

    let range = just(Token::LBracket)
        .ignored()
        .then(
            num.then_ignore(just(Token::Dash))
                .then(num)
                .map_with_span(|(a, b), span| IntervalShape::RangedLen(S((a, b), span)))
                .then_ignore(just(Token::RBracket)),
        )
        .labelled("Range");

    let fixed_len = just(Token::LBracket)
        .ignore_then(num.map_with_span(|n, span| IntervalShape::FixedLen(S(n, span))))
        .then_ignore(just(Token::RBracket))
        .labelled("Fixed Length");

    let seq = nuc.repeated().collect::<Vec<_>>();

    let nucstr = just(Token::LBracket)
        .ignore_then(seq.map_with_span(|nucstr, span| IntervalShape::FixedSeq(S(nucstr, span))))
        .then_ignore(just(Token::RBracket))
        .labelled("Nucleotide String");

    let unbounded = piece_type
        .then(label.or_not())
        .then_ignore(just(Token::Colon))
        .map_with_span(|(type_, label), span| {
            let expr = Expr::GeomPiece(type_, IntervalShape::UnboundedLen);
            if let Some(label) = label {
                Expr::LabeledGeomPiece(Box::new(label), S(Box::new(expr), span))
            } else {
                expr
            }
        })
        .labelled("Unbounded Segment");

    let ranged = piece_type
        .then(label.or_not())
        .then(range)
        .map_with_span(|((type_, label), ((), range)), span| {
            let expr = Expr::GeomPiece(type_, range);
            if let Some(label) = label {
                Expr::LabeledGeomPiece(Box::new(label), S(Box::new(expr), span))
            } else {
                expr
            }
        })
        .labelled("Ranged Segment");

    let fixed = piece_type
        .then(label.or_not())
        .then(fixed_len)
        .map_with_span(|((type_, label), len), span| {
            let expr = Expr::GeomPiece(type_, len);
            if let Some(label) = label {
                Expr::LabeledGeomPiece(Box::new(label), S(Box::new(expr), span))
            } else {
                expr
            }
        })
        .labelled("Fixed Length Segment");

    let fixed_seq = just(Token::FixedSeq)
        .to(IntervalKind::FixedSeq)
        .then(label.or_not())
        .then(nucstr)
        .map_with_span(|((type_, label), nucs), span| {
            let expr = Expr::GeomPiece(type_, nucs);
            if let Some(label) = label {
                Expr::LabeledGeomPiece(Box::new(label), S(Box::new(expr), span))
            } else {
                expr
            }
        })
        .labelled("Fixed Sequence Segment");

    let geom_piece = choice((
        unbounded.clone(),
        ranged.clone(),
        fixed.clone(),
        fixed_seq.clone(),
        label,
        self_,
    ));

    let transformed_pieces = recursive(|transformed_pieces| {
        let recursive_num_arg = transformed_pieces
            .clone()
            .then_ignore(just(Token::Comma))
            .then(num)
            .map_with_span(S)
            .delimited_by(just(Token::LParen), just(Token::RParen));

        let recursive_num_nuc_args = transformed_pieces
            .clone()
            .then_ignore(just(Token::Comma))
            .then(num)
            .then_ignore(just(Token::Comma))
            .then(nuc)
            .map_with_span(S)
            .delimited_by(just(Token::LParen), just(Token::RParen));

        let num_arg = geom_piece
            .clone()
            .then_ignore(just(Token::Comma))
            .then(num)
            .map_with_span(S)
            .delimited_by(just(Token::LParen), just(Token::RParen));

        let recursive_no_arg = transformed_pieces
            .clone()
            .map_with_span(S)
            .delimited_by(just(Token::LParen), just(Token::RParen));

        choice((
            geom_piece.clone(),
            just(Token::Remove)
                .map_with_span(|_, span| S(Function::Remove, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, tok.boxed()))
                .labelled("Remove function"),
            just(Token::Normalize)
                .map_with_span(|_, span| S(Function::Normalize, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, tok.boxed()))
                .labelled("Normalize function"),
            just(Token::Hamming)
                .map_with_span(|_, span| span)
                .then(num_arg)
                .map(|(fn_span, S((geom_p, num), span))| {
                    Expr::Function(
                        S(Function::Hamming(num), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("Hamming function"),
            just(Token::Truncate)
                .map_with_span(|_, span| span)
                .then(recursive_num_arg.clone())
                .map(|(fn_span, S((geom_p, num), span))| {
                    Expr::Function(
                        S(Function::Truncate(num), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("Truncate function"),
            just(Token::TruncateLeft)
                .map_with_span(|_, span| span)
                .then(recursive_num_arg.clone())
                .map(|(fn_span, S((geom_p, num), span))| {
                    Expr::Function(
                        S(Function::TruncateLeft(num), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("Truncate Left function"),
            just(Token::TruncateTo)
                .map_with_span(|_, span| span)
                .then(recursive_num_arg.clone())
                .map(|(fn_span, S((geom_p, num), span))| {
                    Expr::Function(
                        S(Function::TruncateTo(num), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("Truncate To function"),
            just(Token::TruncateToLeft)
                .map_with_span(|_, span| span)
                .then(recursive_num_arg.clone())
                .map(|(fn_span, S((geom_p, num), span))| {
                    Expr::Function(
                        S(Function::TruncateToLeft(num), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("Truncate To Left function"),
            just(Token::Pad)
                .map_with_span(|_, span| span)
                .then(recursive_num_nuc_args.clone())
                .map(|(fn_span, S(((geom_p, num), nuc), span))| {
                    Expr::Function(
                        S(Function::Pad(num, nuc), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("Pad function"),
            just(Token::PadLeft)
                .map_with_span(|_, span| span)
                .then(recursive_num_nuc_args.clone())
                .map(|(fn_span, S(((geom_p, num), nuc), span))| {
                    Expr::Function(
                        S(Function::PadLeft(num, nuc), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("Pad Left function"),
            just(Token::PadTo)
                .map_with_span(|_, span| span)
                .then(recursive_num_nuc_args.clone())
                .map(|(fn_span, S(((geom_p, num), nuc), span))| {
                    Expr::Function(
                        S(Function::PadTo(num, nuc), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("Pad To function"),
            just(Token::PadToLeft)
                .map_with_span(|_, span| span)
                .then(recursive_num_nuc_args)
                .map(|(fn_span, S(((geom_p, num), nuc), span))| {
                    Expr::Function(
                        S(Function::PadToLeft(num, nuc), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("Pad To Left function"),
            just(Token::Reverse)
                .map_with_span(|_, span| S(Function::Reverse, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, tok.boxed()))
                .labelled("Reverse function"),
            just(Token::ReverseComp)
                .map_with_span(|_, span| S(Function::ReverseComp, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, tok.boxed()))
                .labelled("Reverse Compliment function"),
            just(Token::Map)
                .map_with_span(|_, span| span)
                .then(
                    geom_piece
                        .clone()
                        .then_ignore(just(Token::Comma))
                        .then(file.or(argument))
                        .then_ignore(just(Token::Comma))
                        .then(transformed_pieces.clone().map_with_span(S))
                        .map_with_span(S)
                        .delimited_by(just(Token::LParen), just(Token::RParen)),
                )
                .map(|(fn_span, S(((geom_p, path), self_expr), span))| {
                    Expr::Function(
                        S(Function::Map(path, self_expr.boxed()), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("Map function"),
            just(Token::MapWithMismatch)
                .map_with_span(|_, span| span)
                .then(
                    geom_piece
                        .clone()
                        .then_ignore(just(Token::Comma))
                        .then(file.or(argument))
                        .then_ignore(just(Token::Comma))
                        .then(transformed_pieces.clone().map_with_span(S))
                        .then_ignore(just(Token::Comma))
                        .then(num)
                        .map_with_span(S)
                        .delimited_by(just(Token::LParen), just(Token::RParen)),
                )
                .map(|(fn_span, S((((geom_p, path), self_expr), num), span))| {
                    Expr::Function(
                        S(
                            Function::MapWithMismatch(path, self_expr.boxed(), num),
                            fn_span,
                        ),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("Map with mismatch function"),
            just(Token::FilterWithinDist)
                .map_with_span(|_, span| span)
                .then(
                    geom_piece
                        .then_ignore(just(Token::Comma))
                        .then(file.or(argument))
                        .then_ignore(just(Token::Comma))
                        .then(num)
                        .map_with_span(S)
                        .delimited_by(just(Token::LParen), just(Token::RParen)),
                )
                .map(|(fn_span, S(((geom_p, path), num), span))| {
                    Expr::Function(
                        S(Function::FilterWithinDist(path, num), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("Filter within dist function"),
        ))
    })
    .map_with_span(S);

    let definitions = ident
        .map_with_span(|tok, span| Expr::Label(S(tok, span)))
        .then_ignore(just(Token::Equals))
        .then(transformed_pieces.clone())
        .map_with_span(|(label, geom_p), span| {
            S(
                Expr::LabeledGeomPiece(Box::new(label), geom_p.boxed()),
                span,
            )
        })
        .repeated()
        .at_least(1);

    let reads = num
        .map_with_span(S)
        .then(
            transformed_pieces
                .clone()
                .repeated()
                .at_least(1)
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .map(|(n, read)| Expr::Read(n, read))
        .repeated()
        .at_least(2)
        .at_most(2)
        .collect::<Vec<_>>();

    let transform_read = num
        .map_with_span(S)
        .then(
            transformed_pieces
                .clone()
                .repeated()
                .at_least(1)
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .map(|(n, read)| Expr::Read(n, read));

    let transformation = choice((
        end().map_with_span(|(), span| S(None, span)),
        just(Token::TransformTo)
            .then(transform_read.repeated().at_least(1).at_most(2))
            .map_with_span(|(_, val), span| S(Some(Expr::Transform(val)), span)),
    ));

    definitions
        .map_with_span(|tok, span| S(Expr::Definitions(tok), span))
        .or_not()
        .then(reads.map_with_span(S))
        .then(transformation)
        .map(|((d, r), t)| Expr::Description(d.map(S::boxed), r, t.map(|i| i.map(Box::new))))
        .map_with_span(S)
        .recover_with(skip_then_retry_until([]))
}
