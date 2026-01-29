//! Defines the parser for EFGDL.

use std::fmt::{self, Write};

use chumsky::{extra::Err as ExtraErr, input::MappedInput, prelude::*};

use crate::{lexer::Token, Nucleotide, S};

use super::Span;

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
    /// `filter(I, A)`
    Filter(String),
    /// `filter_within_dist(I, A, n)`
    FilterWithinDist(String, usize),
    /// `hamming(F, n)`
    Hamming(usize),
    /// `edit(F, n)` - edit distance (Levenshtein) matching allowing insertions/deletions
    Edit(usize),
    /// `anchor_relative(F)` - search for anchor from position 0 and extract preceding elements with flexible length
    Anchor,
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
            Filter(p) => write!(f, "filter({first}, {p})"),
            FilterWithinDist(p, n) => write!(f, "filter_within_dist({first}, {p}, {n})"),
            Hamming(n) => write!(f, "hamming({first}, {n})"),
            Edit(n) => write!(f, "edit({first}, {n})"),
            Anchor => write!(f, "anchor_relative({first})"),
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

fn make_geom_piece(
    kind: IntervalKind,
    shape: IntervalShape,
    label: Option<Expr>,
    span: Span,
) -> Expr {
    let expr = Expr::GeomPiece(kind, shape);
    if let Some(Expr::Label(lbl)) = label {
        Expr::LabeledGeomPiece(lbl, S(Box::new(expr), span))
    } else {
        expr
    }
}

type Input<'a> = MappedInput<'a, Token, Span, &'a [Spanned<Token>]>;

macro_rules! function_arguments {
    ($base:expr) => {{
        $base
            .map_with(|res, state| S(res, state.span()))
            .delimited_by(
                just(Token::LParen),
                just(Token::RParen)
            )
    }};

    ($base:expr, $first:expr $(, $rest:expr)* $(,)?) => {{
        $base
            .then_ignore(just(Token::Comma))
            .then($first)
            $(
                .then_ignore(just(Token::Comma)).then($rest)
            )*
            .map_with(|res, state| S(res, state.span()))
            .delimited_by(
                just(Token::LParen),
                just(Token::RParen)
            )
    }}
}

macro_rules! unary_function {
    ($func:tt, $arg:expr) => {{
        just(Token::$func)
            .labelled(stringify!($func))
            .map_with(|_, state| state.span())
            .then($arg)
            .map(move |(fn_span, S(geom_p, span))| {
                Expr::Function(
                    S(Function::$func.clone(), fn_span),
                    S(Box::new(geom_p), span),
                )
            })
            .labelled(concat!("Unary function ", stringify!($func)))
            .as_context()
    }};
}

macro_rules! binary_function {
    ($func:tt, $arg:expr) => {{
        just(Token::$func)
            .labelled(stringify!($func))
            .map_with(|_, state| state.span())
            .then($arg)
            .map(move |(fn_span, S((geom_p, arg), span))| {
                Expr::Function(
                    S(Function::$func.clone()(arg), fn_span),
                    S(Box::new(geom_p), span),
                )
            })
            .labelled(concat!("Binary function ", stringify!($func)))
            .as_context()
    }};
}

macro_rules! ternary_function {
    ($func:tt, $arg:expr) => {{
        just(Token::$func)
            .labelled(stringify!($func))
            .map_with(|_, state| state.span())
            .then($arg)
            .map(move |(fn_span, S(((geom_p, arg_one), arg_two), span))| {
                Expr::Function(
                    S(Function::$func.clone()(arg_one, arg_two), fn_span),
                    S(Box::new(geom_p), span),
                )
            })
            .labelled(concat!("Ternary function ", stringify!($func)))
            .as_context()
    }};
}

macro_rules! quaternary_function {
    ($func:tt, $arg:expr $(,)?) => {{
        just(Token::$func)
            .labelled(stringify!($func))
            .map_with(|_, state| state.span())
            .then($arg)
            .map(
                move |(fn_span, S((((geom_p, arg_one), arg_two), arg_three), span))| {
                    Expr::Function(
                        S(
                            Function::$func.clone()(arg_one, arg_two, arg_three),
                            fn_span,
                        ),
                        S(Box::new(geom_p), span),
                    )
                },
            )
            .labelled(concat!("Quaternary function ", stringify!($func)))
            .as_context()
    }};
}

macro_rules! nary_functions {
    ($helper:ident, $arg:expr, $($func:tt),* $(,)?) => {{
        choice((
            $(
                $helper!($func, $arg.clone()),
            )*
        ))
    }}
}

macro_rules! parse_geometry_piece {
    ($piece_type:expr, $inline_label:expr, $kind:expr) => {{
        $piece_type
            .then($inline_label.or_not())
            .then($kind)
            .map_with(|((kind, label), shape), state| {
                make_geom_piece(kind, shape, label, state.span())
            })
    }};
}

// TODO: label everything to add better errors
pub fn parser<'tokens>(
) -> Box<dyn Parser<'tokens, Input<'tokens>, Description, ExtraErr<Rich<'tokens, Token>>> + 'tokens>
{
    // begin with defining basic token selectors
    let label = select! { Token::Label(x) => x.clone() };
    let num = select! {Token::Num(n) => n };
    let file = select! {Token::File(f) => f.clone() };
    let argument = select! {Token::Arg(n) => n.to_string() };
    let self_ = select! { Token::Self_ => Expr::Self_ };

    let piece_type = select! {
        Token::Barcode => IntervalKind::Barcode,
        Token::Umi => IntervalKind::Umi,
        Token::Discard => IntervalKind::Discard,
        Token::ReadSeq => IntervalKind::ReadSeq,
    };

    let nuc = select! {
        Token::A => Nucleotide::A,
        Token::T => Nucleotide::T,
        Token::G => Nucleotide::G,
        Token::C => Nucleotide::C,
        Token::U => Nucleotide::U,
    };

    let inline_label = label
        .delimited_by(
            just(Token::LAngle).labelled("opening '<'"),
            just(Token::RAngle).labelled("closing '>'"),
        )
        .map_with(|l, span: &mut _| Expr::Label(S(l, span.span())))
        .labelled("inline label");

    // interval shape parsers
    let range = num
        .labelled("number")
        .then_ignore(just(Token::Dash))
        .then(num.labelled("number"))
        .map_with(|(a, b), span| IntervalShape::RangedLen(S((a, b), span.span())))
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .labelled("variable length geometry peice shape: [<num>-<num>]");

    let fixed_len = num
        .labelled("number")
        .map_with(|n, state| IntervalShape::FixedLen(S(n, state.span())))
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .labelled("fixed length geometry piece shape: [<num>]");

    let nuc_seq = nuc
        .labelled("nucleotide")
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .map_with(|seq, span| IntervalShape::FixedSeq(S(seq, span.span())))
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .labelled("nucleotide sequence");

    // geom piece parsers
    let unbounded = piece_type
        .then(inline_label.clone().or_not())
        .then_ignore(just(Token::Colon))
        .map_with(|(kind, label), span| {
            make_geom_piece(kind, IntervalShape::UnboundedLen, label, span.span())
        })
        .labelled("Unbounded geometry peice: e.g. 'r:'")
        .as_context();

    let ranged = parse_geometry_piece!(piece_type, inline_label.clone(), range)
        .labelled("Variable length geometry piece: e.g. 'b[9-10]'")
        .as_context();
    let fixed_seq = parse_geometry_piece!(
        just(Token::FixedSeq).to(IntervalKind::FixedSeq),
        inline_label.clone(),
        nuc_seq
    )
    .labelled("Fixed sequence geometry piece: e.g. 'f[ATGC]'")
    .as_context();
    let fixed = parse_geometry_piece!(piece_type, inline_label.clone(), fixed_len)
        .labelled("Fixed length geometry piece: e.g. 'b[10]'")
        .as_context();

    // what constitutes a valid geometry peice
    let geom_piece = choice((unbounded, ranged, fixed, fixed_seq, inline_label, self_));

    // transformed peices
    let transformed_pieces = recursive(|tp| {
        choice((
            geom_piece.clone(),
            nary_functions!(
                unary_function,
                function_arguments!(tp
                    .clone()
                    .labelled("geometry piece as sole argument to function")),
                ReverseComp,
                Reverse,
                Remove,
                Normalize
            ),
            nary_functions!(
                binary_function,
                function_arguments!(
                    tp.clone()
                        .labelled("geometry peice as argument to binary function"),
                    num.labelled("numerical argument to binary function")
                ),
                Hamming,
                Edit,
                Truncate,
                TruncateLeft,
                TruncateTo,
                TruncateToLeft
            ),
            binary_function!(
                Filter,
                function_arguments!(
                    tp.clone()
                        .labelled("geometry piece as argument to 'filter'"),
                    file.labelled("file name")
                        .or(argument.labelled("argument from commandline"))
                )
            ),
            nary_functions!(
                ternary_function,
                function_arguments!(
                    tp.clone()
                        .labelled("geometry piece as argument to 'pad'-like functions"),
                    num.labelled("numerical argument to 'pad'-like functions"),
                    nuc.labelled("nucleotide to pad with")
                ),
                Pad,
                PadLeft,
                PadTo,
                PadToLeft
            ),
            nary_functions!(
                ternary_function,
                function_arguments!(
                    tp.clone().labelled("geometry piece to 'map'"),
                    file.clone().labelled("file name")
                        .or(argument.clone().labelled("argument from commandline")),
                    tp.clone()
                        .labelled("geometry piece after mapping")
                        .map_with(|transf_p, state| S(Box::new(transf_p), state.span()))
                ),
                Map,
            ),
            ternary_function!(
                FilterWithinDist,
                function_arguments!(
                    tp.clone()
                        .labelled("geometry piece to 'filter_within_dist'"),
                    file.clone().labelled("file name")
                        .or(argument.clone().labelled("argument from commandline")),
                    num.labelled("numerical argument")
                )
            ),
            nary_functions!(
                quaternary_function,
                function_arguments!(
                    tp.clone().labelled("geometry piece to 'map_with_mismatch'"),
                    file.clone().labelled("file name")
                        .or(argument.clone().labelled("argument from commandline")),
                    tp.clone()
                        .labelled("geometry piece after mapping")
                        .map_with(|transf_p, state| S(Box::new(transf_p), state.span())),
                    num.clone().labelled("numerical argument")
                ),
                MapWithMismatch,
            ),
            // Anchor relative function - search for anchor and extract preceding elements
            unary_function!(
                Anchor,
                function_arguments!(tp.clone().labelled("geometry piece for anchor_relative"))
            ),
        ))
    })
    .map_with(|s, state| S(s, state.span()));

    // define the basic peices of an EFGDL description
    let definitions = label
        .labelled("definition identifier")
        .map_with(|l, state| S(l, state.span()))
        .then_ignore(just(Token::Equals))
        .then(transformed_pieces.clone())
        .map_with(|(label, expr), span| S(Definition { label, expr }, span.span()))
        .repeated()
        .collect()
        .map_with(|defs, span| S(defs, span.span()));

    let reads = num
        .labelled("read number")
        .map_with(|n, state| S(n, state.span()))
        .then(
            transformed_pieces
                .clone()
                .repeated()
                .at_least(1)
                .collect()
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .map_with(|(index, exprs), span| S(Read { index, exprs }, span.span()))
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .map_with(|v, span| S(v, span.span()));

    let transform_read = num
        .labelled("read number")
        .map_with(|n, state| S(n, state.span()))
        .then(
            transformed_pieces
                .repeated()
                .at_least(1)
                .collect()
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .map_with(|(index, exprs), state| S(Read { index, exprs }, state.span()));

    let transformations = choice((
        end().map(|_| None),
        just(Token::TransformTo)
            .then(
                transform_read
                    .repeated()
                    .at_least(1)
                    .at_most(2)
                    .collect::<Vec<_>>()
                    .then(end()),
            )
            .map_with(|(_, (val, _)), state| Some(S(val, state.span()))),
    ));

    Box::new(
        definitions
            .then(reads)
            .then(transformations)
            .map(|((defs, reads), transforms)| Description {
                definitions: defs,
                reads,
                transforms,
            }),
    )
}
