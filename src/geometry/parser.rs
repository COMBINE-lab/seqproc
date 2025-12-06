//! Defines the parser for EFGDL.

use std::fmt::{self, Write};

use chumsky::prelude::*;

use crate::{
    error::{comma, missing_delimiter, throw},
    lexer::Token,
    Nucleotide, S,
};

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
    /// `filter_within_dist(I, A, n)`
    FilterWithinDist(String, usize),
    /// `hamming(F, n)`
    Hamming(usize),
    /// `search(F)` - forces global search for anchor
    Search,
    /// `search_whitelist(I, A, n)` or `search_whitelist(I, A, n, max_pos)` - searches for barcode from whitelist
    SearchWhitelist(String, usize, Option<usize>),
    /// `anchor_relative(F)` - search for anchor and extract preceding elements relative to found position
    AnchorRelative,
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
            Search => write!(f, "search({first})"),
            SearchWhitelist(p, n, None) => write!(f, "search_whitelist({first}, {p}, {n})"),
            SearchWhitelist(p, n, Some(max)) => write!(f, "search_whitelist({first}, {p}, {n}, {max})"),
            AnchorRelative => write!(f, "anchor_relative({first})"),
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
    ///
    /// `.0` is the label, `.1` is the interval.
    LabeledGeomPiece(S<String>, S<Box<Self>>),

    /// A transformation invocation: `hamming(f[CAGAGC], 1)`.
    ///
    /// `.1` is the first argument.
    Function(S<Function>, S<Box<Self>>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expr::*;
        match self {
            Self_ => write!(f, "self"),
            Label(S(s, _)) => write!(f, "<{s}>"),
            GeomPiece(t, s) => write!(f, "{t}{s}"),
            LabeledGeomPiece(S(l, _), S(expr, _)) => {
                write!(f, "{l}={expr}")
            }
            Function(S(fn_, _), S(expr, _)) => fn_.fmt(f, format_args!("{expr}")),
        }
    }
}

/// A variable definition in an EFGDL header: `foo = f[ABC]`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Definition {
    pub label: S<String>,
    pub expr: S<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
/// A read, with index and expression: `1{hamming(<brc>, 1)}`.
pub struct Read {
    pub index: S<usize>,
    pub exprs: Vec<S<Expr>>,
}

/// A full EFGDL file: 0+ definitions, then input reads, then transformed reads.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]

pub struct Description {
    /// The list of definitions at the top of an EFGDL file:
    /// `brc = b[10] foo = f[CAGAGC]`.``
    pub definitions: S<Vec<S<Definition>>>,
    pub reads: S<Vec<S<Read>>>,
    /// List of reads specifying the output of a transformation:
    /// ` -> 1{<brc>pad(<anchor>, 3, A)<read>}`.
    pub transforms: Option<S<Vec<S<Read>>>>,
}

pub fn parser() -> impl Parser<Token, Description, Error = Simple<Token>> + Clone {
    /*
       Start with creating combinators and
       a recursive definition of a geom_piece

       At execution time we will check if it is a valid
       geometry without any ambiguity. Here we will
       restruct some invalid definitions
    */

    let label = select! { Token::Label(ident) => ident };

    let num = select! { Token::Num(n) => n };

    let file = select! { Token::File(f) => f };

    let argument = select! { Token::Arg(n) => n.to_string() };

    let piece_type = select! {
        Token::Barcode => IntervalKind::Barcode,
        Token::Umi => IntervalKind::Umi,
        Token::Discard => IntervalKind::Discard,
        Token::ReadSeq => IntervalKind::ReadSeq,
    }
    .labelled("specifier");

    let nuc = select! {
        Token::U => Nucleotide::U,
        Token::A => Nucleotide::A,
        Token::T => Nucleotide::T,
        Token::G => Nucleotide::G,
        Token::C => Nucleotide::C,
    };

    let inline_label = label
        .map_err_with_span(|t, span| {
            throw(
                t,
                Simple::custom(
                    span,
                    "Found delimiters '<' and '>' which must delimit a label.",
                ),
            )
        })
        .delimited_by(just(Token::LAngle), just(Token::RAngle))
        .map_err_with_span(|t, span| {
            throw(t, missing_delimiter(Token::RAngle, span, Some("label")))
        })
        .map_with_span(|l, span| Expr::Label(S(l, span)))
        .labelled("label");

    let label = label.map_with_span(S).labelled("label");

    let self_ = just(Token::Self_).to(Expr::Self_).labelled("self");

    let range = num
        .then_ignore(just(Token::Dash))
        .then(num)
        .map_err_with_span(|t, span| {
            throw(
                t,
                Simple::custom(
                    span,
                    "Expected a numerical literal after '-' for a ranged length interval.",
                ),
            )
        })
        .map_with_span(|(a, b), span| IntervalShape::RangedLen(S((a, b), span)))
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .map_err_with_span(|t, span| {
            throw(
                t,
                missing_delimiter(Token::RBracket, span, Some("variable length interval")),
            )
        });

    let fixed_len = num
        .map_err_with_span(|t, span| {
            throw(
                t,
                Simple::custom(
                    span,
                    "Expecting a length specifier '[<num>-<num>]', or '[<num>]'.",
                ),
            )
        })
        .map_with_span(|n, span| IntervalShape::FixedLen(S(n, span)))
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .map_err_with_span(|t, span| {
            throw(
                t,
                missing_delimiter(Token::LBracket, span, Some("fixed length interval")),
            )
        })
        .labelled("fixed_len");

    let seq = nuc
        .repeated()
        .at_least(1)
        .map_err_with_span(|t, span| {
            throw(
                t,
                Simple::custom(span, "A fragment must contain at least one ATGCU character"),
            )
        })
        .collect::<Vec<_>>();

    let nucstr = seq
        .map_with_span(|nucstr, span| IntervalShape::FixedSeq(S(nucstr, span)))
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .map_err_with_span(|t, span| {
            throw(
                t,
                missing_delimiter(Token::LBracket, span, Some("fragment specifier")),
            )
        })
        .labelled("nucstr");

    let unbounded = piece_type
        .map_err_with_span(|t, span| {
            throw(
                t,
                Simple::custom(span, "Specify interval with either 'b'/'u'/'f'/'r'/'x'."),
            )
        })
        .then(inline_label.clone().or_not())
        .then_ignore(just(Token::Colon))
        .map_with_span(|(type_, label), span| {
            let expr = Expr::GeomPiece(type_, IntervalShape::UnboundedLen);
            if let Some(Expr::Label(label)) = label {
                Expr::LabeledGeomPiece(label, S(Box::new(expr), span))
            } else {
                expr
            }
        })
        .labelled("unbound_seg");

    let ranged = piece_type
        .map_err_with_span(|t, span| {
            throw(
                t,
                Simple::custom(span, "Specify interval with either 'b'/'u'/'f'/'r'/'x'."),
            )
        })
        .then(inline_label.clone().or_not())
        .then(range)
        .map_err_with_span(|t, span| {
            throw(
                t,
                Simple::custom(
                    span,
                    "Expecting a length specifier either ':', '[<num>-<num>]', or '[<num>]'.",
                ),
            )
        })
        .map_with_span(|((type_, label), range), span| {
            let expr = Expr::GeomPiece(type_, range);
            if let Some(Expr::Label(label)) = label {
                Expr::LabeledGeomPiece(label, S(Box::new(expr), span))
            } else {
                expr
            }
        })
        .labelled("ranged_len_seg");

    let fixed = piece_type
        .map_err_with_span(|t, span| {
            throw(
                t,
                Simple::custom(span, "Specify interval with either 'b'/'u'/'f'/'r'/'x'."),
            )
        })
        .then(inline_label.clone().or_not())
        .then(fixed_len)
        .map_err_with_span(|t, span| {
            throw(
                t,
                Simple::custom(
                    span,
                    "Expecting a length specifier either ':', '[<num>-<num>]', or '[<num>]'.",
                ),
            )
        })
        .map_with_span(|((type_, label), len), span| {
            let expr = Expr::GeomPiece(type_, len);
            if let Some(Expr::Label(label)) = label {
                Expr::LabeledGeomPiece(label, S(Box::new(expr), span))
            } else {
                expr
            }
        })
        .labelled("fixed_len_seg");

    let fixed_seq = just(Token::FixedSeq)
        .to(IntervalKind::FixedSeq)
        .then(inline_label.clone().or_not())
        .then(nucstr)
        .map_err_with_span(|t, span| {
            throw(
                t,
                Simple::custom(span, "Expecting a sequence to match delimited by '[ .. ]'."),
            )
        })
        .map_with_span(|((type_, label), nucs), span| {
            let expr = Expr::GeomPiece(type_, nucs);
            if let Some(Expr::Label(label)) = label {
                Expr::LabeledGeomPiece(label, S(Box::new(expr), span))
            } else {
                expr
            }
        })
        .labelled("seq_seg");

    let geom_piece = choice((
        unbounded.clone(),
        ranged.clone(),
        fixed.clone(),
        fixed_seq.clone(),
        inline_label,
        self_,
    ))
    .labelled("geom_piece");

    let transformed_pieces = recursive(|transformed_pieces| {
        let transformed_pieces = transformed_pieces
            .map_err_with_span(|t, span| throw(t, Simple::custom(span, "Invalid declaration of interval")));

        let recursive_num_arg = transformed_pieces
            .clone()
            .then_ignore(just(Token::Comma))
            .map_err_with_span(|t, span| throw(t, Simple::custom(span, "Expected a ',' to separate arguments.")))
            .then(num)
            .map_err_with_span(|t, span| throw(t, Simple::custom(span, "Expected a numerical literal as a second argument.")))
            .map_with_span(S)
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map_err_with_span(|t, span| throw(t, missing_delimiter(Token::LParen, span, None)));

        let recursive_num_nuc_args = transformed_pieces
            .clone()
            .then_ignore(just(Token::Comma))
            .map_err_with_span(|t, span| throw(t, comma(span)))
            .then(num)
            .map_err_with_span(|t, span| throw(t, Simple::custom(span, "Expected a numerical literal as a second argument.")))
            .then_ignore(just(Token::Comma))
            .map_err_with_span(|t, span| throw(t, comma(span)))
            .then(nuc)
            .map_err_with_span(|t, span| throw(t, Simple::custom(span, "Expected an ATGCU literal as a third argument.")))
            .map_with_span(S)
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map_err_with_span(|t, span| throw(t, missing_delimiter(Token::LParen, span, None)));

        let recursive_no_arg = transformed_pieces
            .clone()
            .map_with_span(S)
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map_err_with_span(|t, span| throw(t, missing_delimiter(Token::LParen, span, None)));

        choice((
            geom_piece.clone()
                .map_err_with_span(|t, span| throw(t, Simple::custom(span, "Unexpected error when creating an interval."))),
            just(Token::Remove)
                .map_with_span(|_, span| S(Function::Remove, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, tok.boxed()))
                .labelled("remove"),
            just(Token::Normalize)
                .map_with_span(|_, span| S(Function::Normalize, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, tok.boxed()))
                .labelled("norm"),
            just(Token::Hamming)
                .map_with_span(|_, span| span)
                .then(
                    geom_piece
                        .clone()
                        .map_err_with_span(|t, span| {
                            throw(t, Simple::custom(span, "Expected a fragment specified interval as the first argument - 'hamming' cannot take a transformed interval."))
                        })
                        .then_ignore(just(Token::Comma))
                        .then(num).map_err_with_span(|t, span| throw(t, Simple::custom(span, "Expected a numeric literal as a second argument.")))
                        .map_with_span(S)
                        .delimited_by(just(Token::LParen), just(Token::RParen))
                        .map_err_with_span(|t, span| throw(t, missing_delimiter(Token::LParen, span, Some("'hamming'")))),
                )
                .map_err_with_span(|t, span| {
                    throw(t, Simple::custom(span, "Missing argument for hamming - "))
                })
                .map(|(fn_span, S((geom_p, num), span))| {
                    Expr::Function(
                        S(Function::Hamming(num), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("hamming"),
            just(Token::Search)
                .map_with_span(|_, span| S(Function::Search, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, tok.boxed()))
                .labelled("search"),
            just(Token::AnchorRelative)
                .map_with_span(|_, span| S(Function::AnchorRelative, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, tok.boxed()))
                .labelled("anchor_relative"),
            just(Token::Truncate)
                .map_with_span(|_, span| span)
                .then(recursive_num_arg.clone())
                .map(|(fn_span, S((geom_p, num), span))| {
                    Expr::Function(
                        S(Function::Truncate(num), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("trunc"),
            just(Token::TruncateLeft)
                .map_with_span(|_, span| span)
                .then(recursive_num_arg.clone())
                .map(|(fn_span, S((geom_p, num), span))| {
                    Expr::Function(
                        S(Function::TruncateLeft(num), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("trunc_left"),
            just(Token::TruncateTo)
                .map_with_span(|_, span| span)
                .then(recursive_num_arg.clone())
                .map(|(fn_span, S((geom_p, num), span))| {
                    Expr::Function(
                        S(Function::TruncateTo(num), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("trunc_to"),
            just(Token::TruncateToLeft)
                .map_with_span(|_, span| span)
                .then(recursive_num_arg.clone())
                .map(|(fn_span, S((geom_p, num), span))| {
                    Expr::Function(
                        S(Function::TruncateToLeft(num), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("trunc_to_left"),
            just(Token::Pad)
                .map_with_span(|_, span| span)
                .then(recursive_num_nuc_args.clone())
                .map(|(fn_span, S(((geom_p, num), nuc), span))| {
                    Expr::Function(
                        S(Function::Pad(num, nuc), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("pad"),
            just(Token::PadLeft)
                .map_with_span(|_, span| span)
                .then(recursive_num_nuc_args.clone())
                .map(|(fn_span, S(((geom_p, num), nuc), span))| {
                    Expr::Function(
                        S(Function::PadLeft(num, nuc), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("pad_left"),
            just(Token::PadTo)
                .map_with_span(|_, span| span)
                .then(recursive_num_nuc_args.clone())
                .map(|(fn_span, S(((geom_p, num), nuc), span))| {
                    Expr::Function(
                        S(Function::PadTo(num, nuc), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("pad_to"),
            just(Token::PadToLeft)
                .map_with_span(|_, span| span)
                .then(recursive_num_nuc_args)
                .map(|(fn_span, S(((geom_p, num), nuc), span))| {
                    Expr::Function(
                        S(Function::PadToLeft(num, nuc), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("pad_to_left"),
            just(Token::Reverse)
                .map_with_span(|_, span| S(Function::Reverse, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, tok.boxed()))
                .labelled("rev"),
            just(Token::ReverseComp)
                .map_with_span(|_, span| S(Function::ReverseComp, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, tok.boxed()))
                .labelled("revcomp"),
            just(Token::Map)
                .map_with_span(|_, span| span)
                .then(
                    transformed_pieces
                        .clone()
                        .then_ignore(just(Token::Comma))
                        .map_err_with_span(|t, span| throw(t, comma(span)))
                        .then(file.or(argument))
                        .map_err_with_span(|t, span| throw(t, Simple::custom(span, "Expected a file or $<num> to be mapped to command line argument as second argument.")))
                        .then_ignore(just(Token::Comma))
                        .map_err_with_span(|t, span| throw(t, comma(span)))
                        .then(transformed_pieces.clone().map_with_span(S))
                        .map_with_span(S)
                        .delimited_by(just(Token::LParen), just(Token::RParen))
                        .map_err_with_span(|t, span| throw(t, missing_delimiter(Token::LParen, span, Some("'map'")))),
                )
                .map(|(fn_span, S(((geom_p, path), self_expr), span))| {
                    Expr::Function(
                        S(Function::Map(path, self_expr.boxed()), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("map"),
            just(Token::MapWithMismatch)
                .map_with_span(|_, span| span)
                .then(
                    transformed_pieces
                        .clone()
                        .then_ignore(just(Token::Comma))
                        .map_err_with_span(|t, span| throw(t, comma(span)))
                        .then(file.or(argument))
                        .map_err_with_span(|t, span| throw(t, Simple::custom(span, "Expected a file or $<num> to be mapped to command line argument as second argument.")))
                        .then_ignore(just(Token::Comma))
                        .map_err_with_span(|t, span| throw(t, comma(span)))
                        .then(transformed_pieces.clone().map_with_span(S))
                        .then_ignore(just(Token::Comma))
                        .map_err_with_span(|t, span| throw(t, comma(span)))
                        .then(num)
                        .map_err_with_span(|t, span| throw(t, Simple::custom(span, "Expected a numerical literal as the allowable mismatch when mapping interval.")))
                        .map_with_span(S)
                        .delimited_by(just(Token::LParen), just(Token::RParen))
                        .map_err_with_span(|t, span| throw(t, missing_delimiter(Token::LParen, span, Some("'map_with_mismatch'")))),
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
                .labelled("map_dist"),
            just(Token::FilterWithinDist)
                .map_with_span(|_, span| span)
                .then(
                    geom_piece
                        .clone()
                        .then_ignore(just(Token::Comma))
                        .map_err_with_span(|t, span| throw(t, comma(span)))
                        .then(file.or(argument))
                        .map_err_with_span(|t, span| throw(t, Simple::custom(span, "Expected a file or $<num> to be mapped to command line argument as second argument.")))
                        .then_ignore(just(Token::Comma))
                        .map_err_with_span(|t, span| throw(t, comma(span)))
                        .then(num)
                        .map_err_with_span(|t, span| throw(t, Simple::custom(span, "Expected a numerical literal as the allowable mismatch when filtering interval.")))
                        .map_with_span(S)
                        .delimited_by(just(Token::LParen), just(Token::RParen)).map_err_with_span(|t, span| throw(t, missing_delimiter(Token::LParen, span, Some("filter_with_mismatch")))),
                )
                .map(|(fn_span, S(((geom_p, path), num), span))| {
                    Expr::Function(
                        S(Function::FilterWithinDist(path, num), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("filter_dist"),
            just(Token::Filter)
                .map_with_span(|_, span| span)
                .then(
                    geom_piece
                        .clone()
                        .then_ignore(just(Token::Comma))
                        .map_err_with_span(|t, span| throw(t, comma(span)))
                        .then(file.or(argument))
                        .map_err_with_span(|t, span| throw(t, Simple::custom(span, "Expected a file or $<num> to be mapped to command line argument as second argument.")))
                        .map_with_span(S)
                        .delimited_by(just(Token::LParen), just(Token::RParen)).map_err_with_span(|t, span| throw(t, missing_delimiter(Token::LParen, span, Some("'filter'")))),
                )
                .map(|(fn_span, S((geom_p, path), span))| {
                    Expr::Function(
                        S(Function::FilterWithinDist(path, 0), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("filter"),
            // search_whitelist with 4 args: (interval, file, dist, max_pos)
            just(Token::SearchWhitelist)
                .map_with_span(|_, span| span)
                .then(
                    geom_piece
                        .clone()
                        .then_ignore(just(Token::Comma))
                        .map_err_with_span(|t, span| throw(t, comma(span)))
                        .then(file.clone().or(argument.clone()))
                        .then_ignore(just(Token::Comma))
                        .map_err_with_span(|t, span| throw(t, comma(span)))
                        .then(num.clone())
                        .then_ignore(just(Token::Comma))
                        .map_err_with_span(|t, span| throw(t, comma(span)))
                        .then(num.clone())
                        .map_err_with_span(|t, span| throw(t, Simple::custom(span, "Expected a numerical literal as max search position for search_whitelist.")))
                        .map_with_span(S)
                        .delimited_by(just(Token::LParen), just(Token::RParen))
                        .map_err_with_span(|t, span| throw(t, missing_delimiter(Token::LParen, span, Some("'search_whitelist'")))),
                )
                .map(|(fn_span, S((((geom_p, path), dist), max_pos), span))| {
                    Expr::Function(
                        S(Function::SearchWhitelist(path, dist, Some(max_pos)), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("search_whitelist_with_max"),
            // search_whitelist with 3 args: (interval, file, dist)
            just(Token::SearchWhitelist)
                .map_with_span(|_, span| span)
                .then(
                    geom_piece
                        .then_ignore(just(Token::Comma))
                        .map_err_with_span(|t, span| throw(t, comma(span)))
                        .then(file.or(argument))
                        .map_err_with_span(|t, span| throw(t, Simple::custom(span, "Expected a file or $<num> as whitelist file for search_whitelist.")))
                        .then_ignore(just(Token::Comma))
                        .map_err_with_span(|t, span| throw(t, comma(span)))
                        .then(num)
                        .map_err_with_span(|t, span| throw(t, Simple::custom(span, "Expected a numerical literal as max Hamming distance for search_whitelist.")))
                        .map_with_span(S)
                        .delimited_by(just(Token::LParen), just(Token::RParen))
                        .map_err_with_span(|t, span| throw(t, missing_delimiter(Token::LParen, span, Some("'search_whitelist'")))),
                )
                .map(|(fn_span, S(((geom_p, path), dist), span))| {
                    Expr::Function(
                        S(Function::SearchWhitelist(path, dist, None), fn_span),
                        S(Box::new(geom_p), span),
                    )
                })
                .labelled("search_whitelist"),
        ))
    })
    .map_err_with_span(|t, span| throw(t, Simple::custom(span, "Invalid construction of an interval")))
    .map_with_span(S);

    let definitions = label
        .map_err_with_span(|t, span| {
            throw(
                t,
                Simple::custom(span, "Expected a label to begin a definition."),
            )
        })
        .then_ignore(just(Token::Equals))
        .then(transformed_pieces.clone())
        .map_err_with_span(|t, span| {
            throw(
                t,
                Simple::custom(span, "Error creating variable declaration"),
            )
        })
        .map_with_span(|(label, geom_p), span| {
            S(
                Definition {
                    label,
                    expr: geom_p,
                },
                span,
            )
        })
        .repeated()
        .map_with_span(S);

    let reads = num
        .map_err_with_span(|t, span| {
            throw(t, Simple::custom(span, "Expected a number to start a read"))
        })
        .map_with_span(S)
        .then(
            transformed_pieces
                .clone()
                .labelled("transformed_pieces_for_reads")
                .repeated()
                .at_least(1)
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map_err_with_span(|t, span| {
                    throw(t, missing_delimiter(Token::LBrace, span, Some("reads")))
                }),
        )
        .map_with_span(|(n, read), span| {
            S(
                Read {
                    index: n,
                    exprs: read,
                },
                span,
            )
        })
        .repeated()
        .exactly(2)
        .map_err_with_span(|t, span| {
            throw(
                t,
                Simple::custom(span, "Must provide two reads - only found one"),
            )
        })
        .collect::<Vec<_>>();

    let transform_read = num
        .map_with_span(S)
        .then(
            transformed_pieces
                .clone()
                .repeated()
                .at_least(1)
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map_err_with_span(|t, span| {
                    throw(
                        t,
                        missing_delimiter(Token::LBrace, span, Some("transformation")),
                    )
                }),
        )
        .map_with_span(|(n, read), span| {
            S(
                Read {
                    index: n,
                    exprs: read,
                },
                span,
            )
        });

    let transformation = choice((
        end().map(|()| None),
        just(Token::TransformTo)
            .then(transform_read.repeated().at_least(1).at_most(2).then(end()))
            .map_with_span(|(_, (val, _)), span| Some(S(val, span))),
    ));

    definitions
        .map_err_with_span(|t, span| {
            throw(
                t,
                Simple::custom(span, "Error while parsing EFGDL specification."),
            )
        })
        .then(reads.map_with_span(S))
        .then(transformation)
        .map_err_with_span(|t, span| {
            throw(
                t,
                Simple::custom(span, "Error while parsing EFGDL specification."),
            )
        })
        .map(|((definitions, reads), transforms)| Description {
            definitions,
            reads,
            transforms,
        })
        .map_err_with_span(|t, span| {
            throw(
                t,
                Simple::custom(span, "Error while parsing EFGDL specification."),
            )
        })
}
