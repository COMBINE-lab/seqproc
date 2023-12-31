//! Defines the parser for EFGDL.

use std::{
    fmt::{self, Write},
    ops::Deref,
};

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

/// An invocation of an [EFDGL transformation][1].
///
/// [1]: https://efgdl-spec.readthedocs.io/en/latest/transformations.html
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
    /// `remove`
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
    Map(String, Box<S<Expr>>),
    MapWithMismatch(String, Box<S<Expr>>, usize),
    FilterWithinDist(String, usize),
    Hamming(usize),
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Function::*;
        match self {
            Reverse => write!(f, "rev"),
            ReverseComp => write!(f, "revcomp"),
            Truncate(n) => write!(f, "trunc({n}"),
            TruncateLeft(n) => write!(f, "trunc_left({n}"),
            TruncateTo(n) => write!(f, "trunc_to({n}"),
            TruncateToLeft(n) => write!(f, "trunc_to_left({n}"),
            Remove => write!(f, "remove"),
            Pad(n, nuc) => write!(f, "pad({n}, {nuc}"),
            PadLeft(n, nuc) => write!(f, "pad_left({n}, {nuc}"),
            PadTo(n, nuc) => write!(f, "pad_to({n}, {nuc}"),
            PadToLeft(n, nuc) => write!(f, "pad_to_left({n}, {nuc}"),
            Normalize => write!(f, "norm"),
            Map(p, b) => {
                let S(s, _) = b.deref();
                write!(f, "map({p}, {s}")
            }
            MapWithMismatch(p, b, n) => {
                let S(s, _) = b.deref();
                write!(f, "map_with_mismatch({p}, {s}, {n}")
            }
            FilterWithinDist(p, n) => write!(f, "filter_within_dist({p}, {n}"),
            Hamming(n) => write!(f, "hamming({n}"),
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
            Barcode => write!(f, "Barcode"),
            Umi => write!(f, "Umi"),
            Discard => write!(f, "Discard"),
            ReadSeq => write!(f, "ReadSeq"),
            FixedSeq => write!(f, "FixedSeq"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expr {
    Error,
    Self_,
    Argument(usize),
    Label(S<String>),
    Type(S<IntervalKind>),
    GeomPiece(IntervalKind, IntervalShape),
    LabeledGeomPiece(Box<Self>, Box<S<Self>>),
    Function(S<Function>, Box<S<Self>>),
    Read(S<usize>, Vec<S<Self>>),
    Definitions(Vec<S<Self>>),
    Transform(Vec<Self>),
    Description(Box<Option<S<Self>>>, S<Vec<Self>>, Box<S<Option<Self>>>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expr::*;
        match self {
            Error => write!(f, "Error"),
            Argument(n) => write!(f, "argument {n}"),
            Self_ => write!(f, "self"),
            Label(S(s, _)) => write!(f, "{s}"),
            Type(S(t, _)) => write!(f, "{t}"),
            GeomPiece(t, s) => write!(f, "{t}{s}"),
            LabeledGeomPiece(l, box_) => {
                let S(expr, _) = box_.deref();

                write!(f, "{l}={expr}")
            }
            Function(S(fn_, _), box_) => {
                let S(expr, _) = box_.deref();
                write!(f, "{fn_}({expr}))")
            }
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
                let d_w = if let Some(S(d, _)) = d.deref() {
                    Some(format!("{d}"))
                } else {
                    None
                };

                let t_w = if let S(Some(t), _) = t.deref() {
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
                Expr::LabeledGeomPiece(Box::new(label), Box::new(S(expr, span)))
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
                Expr::LabeledGeomPiece(Box::new(label), Box::new(S(expr, span)))
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
                Expr::LabeledGeomPiece(Box::new(label), Box::new(S(expr, span)))
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
                Expr::LabeledGeomPiece(Box::new(label), Box::new(S(expr, span)))
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
                .map(|(fn_, tok)| Expr::Function(fn_, Box::new(tok)))
                .labelled("Remove function"),
            just(Token::Normalize)
                .map_with_span(|_, span| S(Function::Normalize, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, Box::new(tok)))
                .labelled("Normalize function"),
            just(Token::Hamming)
                .map_with_span(|_, span| span)
                .then(num_arg)
                .map(|(fn_span, S((geom_p, num), span))| {
                    Expr::Function(
                        S(Function::Hamming(num), fn_span),
                        Box::new(S(geom_p, span)),
                    )
                })
                .labelled("Hamming function"),
            just(Token::Truncate)
                .map_with_span(|_, span| span)
                .then(recursive_num_arg.clone())
                .map(|(fn_span, S((geom_p, num), span))| {
                    Expr::Function(
                        S(Function::Truncate(num), fn_span),
                        Box::new(S(geom_p, span)),
                    )
                })
                .labelled("Truncate function"),
            just(Token::TruncateLeft)
                .map_with_span(|_, span| span)
                .then(recursive_num_arg.clone())
                .map(|(fn_span, S((geom_p, num), span))| {
                    Expr::Function(
                        S(Function::TruncateLeft(num), fn_span),
                        Box::new(S(geom_p, span)),
                    )
                })
                .labelled("Truncate Left function"),
            just(Token::TruncateTo)
                .map_with_span(|_, span| span)
                .then(recursive_num_arg.clone())
                .map(|(fn_span, S((geom_p, num), span))| {
                    Expr::Function(
                        S(Function::TruncateTo(num), fn_span),
                        Box::new(S(geom_p, span)),
                    )
                })
                .labelled("Truncate To function"),
            just(Token::TruncateToLeft)
                .map_with_span(|_, span| span)
                .then(recursive_num_arg.clone())
                .map(|(fn_span, S((geom_p, num), span))| {
                    Expr::Function(
                        S(Function::TruncateToLeft(num), fn_span),
                        Box::new(S(geom_p, span)),
                    )
                })
                .labelled("Truncate To Left function"),
            just(Token::Pad)
                .map_with_span(|_, span| span)
                .then(recursive_num_nuc_args.clone())
                .map(|(fn_span, S(((geom_p, num), nuc), span))| {
                    Expr::Function(
                        S(Function::Pad(num, nuc), fn_span),
                        Box::new(S(geom_p, span)),
                    )
                })
                .labelled("Pad function"),
            just(Token::PadLeft)
                .map_with_span(|_, span| span)
                .then(recursive_num_nuc_args.clone())
                .map(|(fn_span, S(((geom_p, num), nuc), span))| {
                    Expr::Function(
                        S(Function::PadLeft(num, nuc), fn_span),
                        Box::new(S(geom_p, span)),
                    )
                })
                .labelled("Pad Left function"),
            just(Token::PadTo)
                .map_with_span(|_, span| span)
                .then(recursive_num_nuc_args.clone())
                .map(|(fn_span, S(((geom_p, num), nuc), span))| {
                    Expr::Function(
                        S(Function::PadTo(num, nuc), fn_span),
                        Box::new(S(geom_p, span)),
                    )
                })
                .labelled("Pad To function"),
            just(Token::PadToLeft)
                .map_with_span(|_, span| span)
                .then(recursive_num_nuc_args)
                .map(|(fn_span, S(((geom_p, num), nuc), span))| {
                    Expr::Function(
                        S(Function::PadToLeft(num, nuc), fn_span),
                        Box::new(S(geom_p, span)),
                    )
                })
                .labelled("Pad To Left function"),
            just(Token::Reverse)
                .map_with_span(|_, span| S(Function::Reverse, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, Box::new(tok)))
                .labelled("Reverse function"),
            just(Token::ReverseComp)
                .map_with_span(|_, span| S(Function::ReverseComp, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, Box::new(tok)))
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
                        S(Function::Map(path, Box::new(self_expr)), fn_span),
                        Box::new(S(geom_p, span)),
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
                            Function::MapWithMismatch(path, Box::new(self_expr), num),
                            fn_span,
                        ),
                        Box::new(S(geom_p, span)),
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
                        Box::new(S(geom_p, span)),
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
                Expr::LabeledGeomPiece(Box::new(label), Box::new(geom_p)),
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
        .map(|((d, r), t)| Expr::Description(Box::new(d), r, Box::new(t)))
        .map_with_span(S)
        .recover_with(skip_then_retry_until([]))
}
