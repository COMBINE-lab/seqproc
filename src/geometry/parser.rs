//! Defines the parser for EFGDL.

use std::fmt::{self, Write};

use chumsky::{extra::Err as ExtraErr, input::MappedInput, prelude::*};

use crate::{lexer::Token, Nucleotide, S};

use super::Span;

/// A file-backed resource referenced by a geometry operation.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ResourceRef {
    /// A quoted path embedded in the geometry.
    Literal(String),
    /// A legacy zero-based `$0`, `$1`, ... runtime resource.
    Positional(usize),
    /// A declared `$name` runtime resource.
    Named(String),
}

impl fmt::Display for ResourceRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Literal(path) => write!(f, "\"{path}\""),
            Self::Positional(index) => write!(f, "${index}"),
            Self::Named(name) => write!(f, "${name}"),
        }
    }
}

impl From<String> for ResourceRef {
    fn from(value: String) -> Self {
        Self::Literal(value)
    }
}

impl From<&str> for ResourceRef {
    fn from(value: &str) -> Self {
        Self::Literal(value.to_owned())
    }
}

impl PartialEq<str> for ResourceRef {
    fn eq(&self, other: &str) -> bool {
        matches!(self, Self::Literal(path) if path == other)
    }
}

impl PartialEq<&str> for ResourceRef {
    fn eq(&self, other: &&str) -> bool {
        self == *other
    }
}

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
    Map(ResourceRef, S<Box<Expr>>),
    /// `map_with_mismatch(I, A, F, n)`
    MapWithMismatch(ResourceRef, S<Box<Expr>>, usize),
    /// `filter(I, A)`
    Filter(ResourceRef),
    /// `filter_within_dist(I, A, n)`
    FilterWithinDist(ResourceRef, usize),
    /// `hamming(F, n)`
    Hamming(usize),
    /// `edit(F, n)` - edit distance (Levenshtein) matching
    Edit(usize),
    /// `map_with_edit(I, A, F, n)` - map with edit distance tolerance
    MapWithEdit(ResourceRef, S<Box<Expr>>, usize),
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
            MapWithEdit(p, b, n) => {
                let S(s, _) = b;
                write!(f, "map_with_edit({first}, {p}, {s}, {n})")
            }
            Anchor => write!(f, "anchor_relative({first})"),
        }
    }
}

/// <https://efgdl-spec.readthedocs.io/en/latest/intervals.html>
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum IntervalKind {
    Barcode,
    SampleBarcode,
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
            SampleBarcode => write!(f, "s"),
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

    /// A one-based reference to one occurrence of a statically bounded
    /// repeated capture: `<my_label[2]>`.
    IndexedLabel(S<String>, S<usize>),

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

    /// Ordered concatenation in an EFGDL 2 input layout.
    LayoutConcat(Vec<S<Self>>),

    /// Ordered alternatives in an EFGDL 2 input layout. The first successful
    /// alternative wins at runtime.
    LayoutChoice(Vec<S<Self>>),

    /// An optional EFGDL 2 input-layout term.
    LayoutOptional(S<Box<Self>>),

    /// A fixed-count EFGDL 2 input-layout repetition.
    LayoutRepeat(S<Box<Self>>, S<usize>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expr::*;
        match self {
            Self_ => write!(f, "self"),
            Label(S(s, _)) => write!(f, "<{s}>"),
            IndexedLabel(S(s, _), S(index, _)) => write!(f, "<{s}[{index}]>"),
            GeomPiece(t, s) => write!(f, "{t}{s}"),
            LabeledGeomPiece(S(l, _), S(expr, _)) => {
                write!(f, "{l}={expr}")
            }
            Function(S(fn_, _), S(expr, _)) => fn_.fmt(f, format_args!("{expr}")),
            LayoutConcat(parts) => {
                for S(part, _) in parts {
                    write!(f, "{part}")?;
                }
                Ok(())
            }
            LayoutChoice(arms) => {
                for (index, S(arm, _)) in arms.iter().enumerate() {
                    if index > 0 {
                        f.write_str(" | ")?;
                    }
                    write!(f, "{arm}")?;
                }
                Ok(())
            }
            LayoutOptional(S(expr, _)) => write!(f, "({expr})?"),
            LayoutRepeat(S(expr, _), S(count, _)) => write!(f, "({expr})*{count}"),
        }
    }
}

/// A variable definition in an EFGDL header: `foo = f[ABC]`.
/// Optionally preceded by annotations: `#[edit(5)] foo = f[ABC]`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Definition {
    pub annotations: Vec<S<Annotation>>,
    pub label: S<String>,
    pub expr: S<Expr>,
}

/// An annotation on a read or definition: `#[match_ori(either)]`, `#[edit(5)]`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Annotation {
    pub name: S<String>,
    /// Arguments for operation-style annotations such as `#[hamming(1)]`.
    pub args: Vec<S<String>>,
    /// Value for property-style annotations such as
    /// `#[ambig_policy = quality(min_delta = 2)]`.
    pub value: Option<S<AnnotationValue>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AnnotationValue {
    pub variant: S<String>,
    pub args: Vec<S<AnnotationValueArg>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AnnotationValueArg {
    pub name: Option<S<String>>,
    pub value: S<String>,
}

/// A scalar value in the version-neutral EFGDL document header.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum HeaderValue {
    Number(usize),
    String(String),
    Identifier(String),
}

/// One entry in an EFGDL document header, such as `efgdl = 2`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct HeaderField {
    pub name: S<String>,
    pub value: S<HeaderValue>,
}

/// Version-neutral metadata at the beginning of an EFGDL document.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct DocumentHeader {
    pub fields: Vec<S<HeaderField>>,
}

/// One declaration in an EFGDL 2 `resources` block.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ResourceDeclaration {
    pub name: S<String>,
    /// A quoted default path, resolved relative to the geometry file.
    pub default: Option<S<String>>,
}

/// How an EFGDL 2 output transformation modifies a FASTQ record name.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OutputHeaderMode {
    Append,
    Prepend,
    Replace,
}

/// One component of an output FASTQ-header template.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum OutputHeaderPart {
    Literal(String),
    Label(S<String>),
    IndexedLabel(S<String>, S<usize>),
}

/// An optional output-record header transformation.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct OutputHeader {
    pub mode: S<OutputHeaderMode>,
    pub parts: Vec<S<OutputHeaderPart>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
/// A read, with optional annotations, index, and expressions: `#[match_ori(either)] 1{...}`.
pub struct Read {
    pub annotations: Vec<S<Annotation>>,
    /// Only populated for reads on the output side of `->`.
    pub output_header: Option<S<OutputHeader>>,
    pub index: S<usize>,
    pub exprs: Vec<S<Expr>>,
}

/// Output specification after `->`: either direct reads or a match block.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TransformOutput {
    /// Direct output: `-> 1{<bc>} 2{<read>}`
    Direct(Vec<S<Read>>),
    /// Match block: `-> match 1.ori { fw => 1{...}, rc => 1{...} }`
    Match {
        read_ref: S<usize>,
        attr: S<String>,
        fw_arm: Vec<S<Read>>,
        rc_arm: Vec<S<Read>>,
    },
}

/// A full EFGDL file: 0+ definitions, then input reads, then transformed reads.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Description {
    /// Optional document header. Headerless files retain legacy EFGDL 1
    /// semantics; new EFGDL 2 documents declare `header { efgdl = 2 }`.
    pub header: Option<S<DocumentHeader>>,
    /// Optional named resource declarations for EFGDL 2.
    pub resources: Option<S<Vec<S<ResourceDeclaration>>>>,
    /// The list of definitions at the top of an EFGDL file:
    /// `brc = b[10] foo = f[CAGAGC]`.
    pub definitions: S<Vec<S<Definition>>>,
    pub reads: S<Vec<S<Read>>>,
    /// Output specification after `->`, if present.
    pub transforms: Option<S<TransformOutput>>,
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
    let argument = select! {Token::Arg(n) => format!("${n}") };
    let resource_ref = choice((
        select! { Token::File(path) => ResourceRef::Literal(path.clone()) },
        select! { Token::Arg(index) => ResourceRef::Positional(index) },
        select! { Token::NamedArg(name) => ResourceRef::Named(name.clone()) },
    ))
    .boxed();
    let self_ = select! { Token::Self_ => Expr::Self_ };

    // The document header intentionally uses a small, version-neutral scalar
    // grammar so it can select the grammar/semantics of the body that follows.
    let header_key = select! { Token::Label(key) => key.clone() };
    let header_value = choice((
        select! { Token::Num(value) => HeaderValue::Number(value) },
        select! { Token::File(value) => HeaderValue::String(value.clone()) },
        select! { Token::Label(value) => HeaderValue::Identifier(value.clone()) },
    ));
    let header_field = header_key
        .map_with(|name, state| S(name, state.span()))
        .then_ignore(just(Token::Equals))
        .then(header_value.map_with(|value, state| S(value, state.span())))
        .map_with(|(name, value), state| S(HeaderField { name, value }, state.span()));
    let document_header = select! {
        Token::Label(name) if name == "header" => (),
    }
    .ignore_then(
        header_field
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .at_least(1)
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace)),
    )
    .map_with(|fields, state| S(DocumentHeader { fields }, state.span()))
    .or_not()
    .boxed();

    let resource_declaration = label
        .clone()
        .map_with(|name, state| S(name, state.span()))
        .then(
            just(Token::Equals)
                .ignore_then(file.clone().map_with(|path, state| S(path, state.span())))
                .or_not(),
        )
        .map_with(|(name, default), state| S(ResourceDeclaration { name, default }, state.span()));
    let resources = select! {
        Token::Label(name) if name == "resources" => (),
    }
    .ignore_then(
        resource_declaration
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .at_least(1)
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace)),
    )
    .map_with(|declarations, state| S(declarations, state.span()))
    .or_not()
    .boxed();

    let piece_type = select! {
        Token::Barcode => IntervalKind::Barcode,
        Token::SampleBarcode => IntervalKind::SampleBarcode,
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

    let inline_binding = label
        .delimited_by(
            just(Token::LAngle).labelled("opening '<'"),
            just(Token::RAngle).labelled("closing '>'"),
        )
        .map_with(|l, span: &mut _| Expr::Label(S(l, span.span())))
        .labelled("inline label");

    let capture_reference = label
        .then(
            num.map_with(|index, state| S(index, state.span()))
                .delimited_by(just(Token::LBracket), just(Token::RBracket))
                .or_not(),
        )
        .delimited_by(
            just(Token::LAngle).labelled("opening '<'"),
            just(Token::RAngle).labelled("closing '>'"),
        )
        .map_with(|(label, index), state| match index {
            Some(index) => Expr::IndexedLabel(S(label, state.span()), index),
            None => Expr::Label(S(label, state.span())),
        })
        .labelled("capture reference");

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
        .then(inline_binding.clone().or_not())
        .then_ignore(just(Token::Colon))
        .map_with(|(kind, label), span| {
            make_geom_piece(kind, IntervalShape::UnboundedLen, label, span.span())
        })
        .labelled("Unbounded geometry peice: e.g. 'r:'")
        .as_context();

    let ranged = parse_geometry_piece!(piece_type, inline_binding.clone(), range)
        .labelled("Variable length geometry piece: e.g. 'b[9-10]'")
        .as_context();
    let fixed_seq = parse_geometry_piece!(
        just(Token::FixedSeq).to(IntervalKind::FixedSeq),
        inline_binding.clone(),
        nuc_seq
    )
    .labelled("Fixed sequence geometry piece: e.g. 'f[ATGC]'")
    .as_context();
    let fixed = parse_geometry_piece!(piece_type, inline_binding.clone(), fixed_len)
        .labelled("Fixed length geometry piece: e.g. 'b[10]'")
        .as_context();

    // what constitutes a valid geometry peice
    let geom_piece = choice((
        unbounded,
        ranged,
        fixed,
        fixed_seq,
        capture_reference.clone(),
        self_,
    ));

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
                    resource_ref.clone().labelled("resource reference")
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
                    resource_ref.clone().labelled("resource reference"),
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
                    resource_ref.clone().labelled("resource reference"),
                    num.labelled("numerical argument")
                )
            ),
            nary_functions!(
                quaternary_function,
                function_arguments!(
                    tp.clone().labelled("geometry piece to 'map_with_mismatch'"),
                    resource_ref.clone().labelled("resource reference"),
                    tp.clone()
                        .labelled("geometry piece after mapping")
                        .map_with(|transf_p, state| S(Box::new(transf_p), state.span())),
                    num.labelled("numerical argument")
                ),
                MapWithMismatch,
                MapWithEdit,
            ),
            // Anchor relative function - search for anchor and extract preceding elements
            unary_function!(
                Anchor,
                function_arguments!(tp.clone().labelled("geometry piece for anchor_relative"))
            ),
        ))
    })
    .map_with(|s, state| S(s, state.span()));

    // EFGDL 2 input-layout algebra. Concatenation remains implicit, `|` is
    // an ordered choice, `?` makes a term optional, and `*N` repeats a term a
    // fixed number of times. The compiler rejects these constructs in
    // headerless (legacy EFGDL 1) documents and bounds their normalization.
    let input_layout = recursive(|layout| {
        let grouped = layout
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen));
        let atom = choice((transformed_pieces.clone(), grouped));

        let postfix = atom
            .then(
                choice((
                    just(Token::Question).to(None),
                    just(Token::Star).ignore_then(num).map(Some),
                ))
                .or_not(),
            )
            .map_with(|(term, modifier), state| match modifier {
                None => term,
                Some(None) => S(
                    Expr::LayoutOptional(S(Box::new(term.0), term.1)),
                    state.span(),
                ),
                Some(Some(count)) => S(
                    Expr::LayoutRepeat(S(Box::new(term.0), term.1), S(count, state.span())),
                    state.span(),
                ),
            });

        let concat =
            postfix
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>()
                .map_with(|mut parts, state| {
                    if parts.len() == 1 {
                        parts.pop().expect("one layout part")
                    } else {
                        S(Expr::LayoutConcat(parts), state.span())
                    }
                });

        concat
            .separated_by(just(Token::Pipe))
            .at_least(1)
            .collect::<Vec<_>>()
            .map_with(|mut arms, state| {
                if arms.len() == 1 {
                    arms.pop().expect("one layout arm")
                } else {
                    S(Expr::LayoutChoice(arms), state.span())
                }
            })
    });

    // Annotation name: accepts Label tokens and keyword tokens that may appear
    // as annotation names (e.g., edit, hamming, match).
    let annotation_name = choice((
        label,
        just(Token::Edit).to("edit".to_string()),
        just(Token::Hamming).to("hamming".to_string()),
        just(Token::Match).to("match".to_string()),
        just(Token::Anchor).to("anchor_relative".to_string()),
        just(Token::Filter).to("filter".to_string()),
        just(Token::Normalize).to("norm".to_string()),
        just(Token::Reverse).to("rev".to_string()),
        just(Token::ReverseComp).to("revcomp".to_string()),
    ));

    // Annotation argument: accepts labels, keywords, and numbers.
    // NOTE: Keep in sync with annotation_name above -- every keyword
    // accepted as a name should also be accepted as an argument.
    let annotation_arg = choice((
        label,
        file,
        argument,
        select! { Token::NamedArg(name) => format!("${name}") },
        num.map(|n: usize| n.to_string()),
        just(Token::Edit).to("edit".to_string()),
        just(Token::Hamming).to("hamming".to_string()),
        just(Token::Match).to("match".to_string()),
        just(Token::Anchor).to("anchor_relative".to_string()),
        just(Token::Filter).to("filter".to_string()),
        just(Token::Normalize).to("norm".to_string()),
        just(Token::Reverse).to("rev".to_string()),
        just(Token::ReverseComp).to("revcomp".to_string()),
        just(Token::Fw).to("fw".to_string()),
        just(Token::Rc).to("rc".to_string()),
    ));

    let spanned_annotation_arg = annotation_arg
        .clone()
        .map_with(|arg, state| S(arg, state.span()));

    // Property values use an enum-like variant with optional positional or
    // named scalar arguments: `quality(min_delta = 2)` or `random(42)`.
    let assignment_value_arg = spanned_annotation_arg
        .clone()
        .then(
            just(Token::Equals)
                .ignore_then(spanned_annotation_arg.clone())
                .or_not(),
        )
        .map_with(|(first, assigned), state| {
            let (name, value) = match assigned {
                Some(value) => (Some(first), value),
                None => (None, first),
            };
            S(AnnotationValueArg { name, value }, state.span())
        });

    let assignment_value = spanned_annotation_arg
        .clone()
        .then(
            assignment_value_arg
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen))
                .or_not(),
        )
        .map_with(|(variant, args), state| {
            S(
                AnnotationValue {
                    variant,
                    args: args.unwrap_or_default(),
                },
                state.span(),
            )
        });

    // Parse either an operation annotation (`#[name(args)]`) or a property
    // assignment (`#[name = variant(args)]`). Each annotation name can choose
    // one canonical form during semantic validation.
    let annotation = just(Token::HashBracket)
        .ignore_then(
            annotation_name
                .map_with(|name, state| S(name, state.span()))
                .then(choice((
                    just(Token::Equals)
                        .ignore_then(assignment_value)
                        .map(|value| (Vec::new(), Some(value))),
                    annotation_arg
                        .clone()
                        .map_with(|a, state| S(a, state.span()))
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::LParen), just(Token::RParen))
                        .map(|args| (args, None)),
                )))
                .then_ignore(just(Token::RBracket)),
        )
        .map_with(|(name, (args, value)), state| S(Annotation { name, args, value }, state.span()));

    // Output FASTQ-name templates are parsed separately from general
    // annotations because their arguments are typed literals and captured
    // labels rather than annotation-policy scalars.
    let output_header_mode = select! {
        Token::Label(mode) if mode == "append" => OutputHeaderMode::Append,
        Token::Label(mode) if mode == "prepend" => OutputHeaderMode::Prepend,
        Token::Label(mode) if mode == "replace" => OutputHeaderMode::Replace,
    }
    .map_with(|mode, state| S(mode, state.span()));
    let output_header_capture = label
        .map_with(|name, state| S(name, state.span()))
        .then(
            num.map_with(|index, state| S(index, state.span()))
                .delimited_by(just(Token::LBracket), just(Token::RBracket))
                .or_not(),
        )
        .delimited_by(just(Token::LAngle), just(Token::RAngle))
        .map(|(label, index)| match index {
            Some(index) => OutputHeaderPart::IndexedLabel(label, index),
            None => OutputHeaderPart::Label(label),
        });
    let output_header_part = choice((
        file.map(OutputHeaderPart::Literal)
            .map_with(|part, state| S(part, state.span())),
        output_header_capture.map_with(|part, state| S(part, state.span())),
    ));
    let output_header = just(Token::HashBracket)
        .ignore_then(select! {
            Token::Label(name) if name == "header" => (),
        })
        .then_ignore(just(Token::Equals))
        .ignore_then(
            output_header_mode.then(
                output_header_part
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            ),
        )
        .then_ignore(just(Token::RBracket))
        .map_with(|(mode, parts), state| S(OutputHeader { mode, parts }, state.span()));

    // define the basic peices of an EFGDL description
    // Definitions may be preceded by annotations: #[edit(5)] foo = f[ABC]
    let definitions = annotation
        .clone()
        .repeated()
        .collect::<Vec<_>>()
        .then(
            label
                .labelled("definition identifier")
                .map_with(|l, state| S(l, state.span()))
                .then_ignore(just(Token::Equals))
                .then(transformed_pieces.clone()),
        )
        .map_with(|(annotations, (label, expr)), span| {
            S(
                Definition {
                    annotations,
                    label,
                    expr,
                },
                span.span(),
            )
        })
        .repeated()
        .collect()
        .map_with(|defs, span| S(defs, span.span()));

    let reads = annotation
        .clone()
        .repeated()
        .collect::<Vec<_>>()
        .then(
            num.labelled("read number")
                .map_with(|n, state| S(n, state.span()))
                .then(
                    input_layout
                        .delimited_by(just(Token::LBrace), just(Token::RBrace))
                        .map(|S(expr, span)| match expr {
                            Expr::LayoutConcat(parts) => parts,
                            expr => vec![S(expr, span)],
                        }),
                ),
        )
        .map_with(|(annotations, (index, exprs)), span| {
            S(
                Read {
                    annotations,
                    output_header: None,
                    index,
                    exprs,
                },
                span.span(),
            )
        })
        .repeated()
        .at_least(1)
        .at_most(3)
        .collect::<Vec<_>>()
        .map_with(|v, span| S(v, span.span()));

    let transform_read = output_header
        .or_not()
        .then(
            num.labelled("read number")
                .map_with(|n, state| S(n, state.span()))
                .then(
                    transformed_pieces
                        .repeated()
                        .at_least(1)
                        .collect()
                        .delimited_by(just(Token::LBrace), just(Token::RBrace)),
                ),
        )
        .map_with(|(output_header, (index, exprs)), state| {
            S(
                Read {
                    annotations: vec![],
                    output_header,
                    index,
                    exprs,
                },
                state.span(),
            )
        });

    // Parse match block: match 1.ori { fw => 1{...} 2{...}, rc => 1{...} 2{...} }
    let match_block = just(Token::Match)
        .ignore_then(
            num.labelled("read reference in match")
                .map_with(|n, state| S(n, state.span())),
        )
        .then_ignore(just(Token::Dot))
        .then(
            label
                .labelled("attribute name in match")
                .map_with(|a, state| S(a, state.span())),
        )
        .then(
            just(Token::Fw)
                .ignore_then(just(Token::FatArrow))
                .ignore_then(
                    transform_read
                        .clone()
                        .repeated()
                        .at_least(1)
                        .at_most(3)
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::Comma))
                .then(
                    just(Token::Rc)
                        .ignore_then(just(Token::FatArrow))
                        .ignore_then(
                            transform_read
                                .clone()
                                .repeated()
                                .at_least(1)
                                .at_most(3)
                                .collect::<Vec<_>>(),
                        ),
                )
                .then_ignore(just(Token::Comma).or_not())
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .map(
            |((read_ref, attr), (fw_arm, rc_arm))| TransformOutput::Match {
                read_ref,
                attr,
                fw_arm,
                rc_arm,
            },
        );

    let transformations = choice((
        end().map(|_| None),
        just(Token::TransformTo)
            .ignore_then(choice((
                match_block.then(end()).map(|(m, _)| m),
                transform_read
                    .repeated()
                    .at_least(1)
                    .at_most(3)
                    .collect::<Vec<_>>()
                    .then(end())
                    .map(|(val, _)| TransformOutput::Direct(val)),
            )))
            .map_with(|output, state| Some(S(output, state.span()))),
    ));

    Box::new(
        document_header
            .then(resources)
            .then(definitions)
            .then(reads)
            .then(transformations)
            .map(
                |((((header, resources), defs), reads), transforms)| Description {
                    header,
                    resources,
                    definitions: defs,
                    reads,
                    transforms,
                },
            ),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Nucleotide, S};

    fn span() -> Span {
        (0..1).into()
    }

    #[test]
    fn test_interval_shape_display() {
        let fixed = IntervalShape::FixedLen(S(16, span()));
        assert_eq!(format!("{}", fixed), "[16]");

        let seq = IntervalShape::FixedSeq(S(
            vec![Nucleotide::A, Nucleotide::C, Nucleotide::G, Nucleotide::T],
            span(),
        ));
        assert_eq!(format!("{}", seq), "[ACGT]");

        let ranged = IntervalShape::RangedLen(S((8, 12), span()));
        assert_eq!(format!("{}", ranged), "[8-12]");

        let unbounded = IntervalShape::UnboundedLen;
        assert_eq!(format!("{}", unbounded), ":");
    }

    #[test]
    fn test_interval_kind_display() {
        assert_eq!(format!("{}", IntervalKind::Barcode), "b");
        assert_eq!(format!("{}", IntervalKind::SampleBarcode), "s");
        assert_eq!(format!("{}", IntervalKind::Umi), "u");
        assert_eq!(format!("{}", IntervalKind::Discard), "x");
        assert_eq!(format!("{}", IntervalKind::ReadSeq), "r");
        assert_eq!(format!("{}", IntervalKind::FixedSeq), "f");
    }

    #[test]
    fn test_expr_display_self() {
        assert_eq!(format!("{}", Expr::Self_), "self");
    }

    #[test]
    fn test_expr_display_label() {
        let e = Expr::Label(S("foo".to_string(), span()));
        assert_eq!(format!("{}", e), "<foo>");
    }

    #[test]
    fn test_expr_display_geom_piece() {
        let e = Expr::GeomPiece(
            IntervalKind::Barcode,
            IntervalShape::FixedLen(S(16, span())),
        );
        assert_eq!(format!("{}", e), "b[16]");
    }

    #[test]
    fn test_expr_display_labeled() {
        let inner = Expr::GeomPiece(
            IntervalKind::Barcode,
            IntervalShape::FixedLen(S(16, span())),
        );
        let e = Expr::LabeledGeomPiece(S("bc1".to_string(), span()), S(Box::new(inner), span()));
        assert_eq!(format!("{}", e), "bc1=b[16]");
    }

    #[test]
    fn test_expr_display_function_reverse() {
        let inner = Expr::GeomPiece(
            IntervalKind::Barcode,
            IntervalShape::FixedLen(S(16, span())),
        );
        let e = Expr::Function(S(Function::Reverse, span()), S(Box::new(inner), span()));
        assert_eq!(format!("{}", e), "rev(b[16])");
    }

    #[test]
    fn test_expr_display_function_revcomp() {
        let inner = Expr::GeomPiece(
            IntervalKind::Barcode,
            IntervalShape::FixedLen(S(16, span())),
        );
        let e = Expr::Function(S(Function::ReverseComp, span()), S(Box::new(inner), span()));
        assert_eq!(format!("{}", e), "revcomp(b[16])");
    }

    #[test]
    fn test_function_fmt_variants() {
        let inner = Expr::GeomPiece(
            IntervalKind::Barcode,
            IntervalShape::FixedLen(S(16, span())),
        );

        let trunc = Expr::Function(
            S(Function::Truncate(2), span()),
            S(Box::new(inner.clone()), span()),
        );
        assert_eq!(format!("{}", trunc), "trunc(b[16], 2)");

        let trunc_left = Expr::Function(
            S(Function::TruncateLeft(2), span()),
            S(Box::new(inner.clone()), span()),
        );
        assert_eq!(format!("{}", trunc_left), "trunc_left(b[16], 2)");

        let trunc_to = Expr::Function(
            S(Function::TruncateTo(10), span()),
            S(Box::new(inner.clone()), span()),
        );
        assert_eq!(format!("{}", trunc_to), "trunc_to(b[16], 10)");

        let trunc_to_left = Expr::Function(
            S(Function::TruncateToLeft(10), span()),
            S(Box::new(inner.clone()), span()),
        );
        assert_eq!(format!("{}", trunc_to_left), "trunc_to_left(b[16], 10)");

        let remove = Expr::Function(
            S(Function::Remove, span()),
            S(Box::new(inner.clone()), span()),
        );
        assert_eq!(format!("{}", remove), "remove(b[16])");

        let pad = Expr::Function(
            S(Function::Pad(4, Nucleotide::A), span()),
            S(Box::new(inner.clone()), span()),
        );
        assert_eq!(format!("{}", pad), "pad(b[16], 4, A)");

        let pad_left = Expr::Function(
            S(Function::PadLeft(4, Nucleotide::T), span()),
            S(Box::new(inner.clone()), span()),
        );
        assert_eq!(format!("{}", pad_left), "pad_left(b[16], 4, T)");

        let pad_to = Expr::Function(
            S(Function::PadTo(20, Nucleotide::G), span()),
            S(Box::new(inner.clone()), span()),
        );
        assert_eq!(format!("{}", pad_to), "pad_to(b[16], 20, G)");

        let pad_to_left = Expr::Function(
            S(Function::PadToLeft(20, Nucleotide::C), span()),
            S(Box::new(inner.clone()), span()),
        );
        assert_eq!(format!("{}", pad_to_left), "pad_to_left(b[16], 20, C)");

        let norm = Expr::Function(
            S(Function::Normalize, span()),
            S(Box::new(inner.clone()), span()),
        );
        assert_eq!(format!("{}", norm), "norm(b[16])");

        let hamming = Expr::Function(
            S(Function::Hamming(1), span()),
            S(Box::new(inner.clone()), span()),
        );
        assert_eq!(format!("{}", hamming), "hamming(b[16], 1)");

        let edit = Expr::Function(
            S(Function::Edit(1), span()),
            S(Box::new(inner.clone()), span()),
        );
        assert_eq!(format!("{}", edit), "edit(b[16], 1)");

        let anchor = Expr::Function(
            S(Function::Anchor, span()),
            S(Box::new(inner.clone()), span()),
        );
        assert_eq!(format!("{}", anchor), "anchor_relative(b[16])");

        let filter = Expr::Function(
            S(Function::Filter("test".into()), span()),
            S(Box::new(inner.clone()), span()),
        );
        assert_eq!(format!("{}", filter), "filter(b[16], \"test\")");

        let filter_within = Expr::Function(
            S(Function::FilterWithinDist("test".into(), 2), span()),
            S(Box::new(inner.clone()), span()),
        );
        assert_eq!(
            format!("{}", filter_within),
            "filter_within_dist(b[16], \"test\", 2)"
        );
    }

    #[test]
    fn test_function_fmt_map_variants() {
        let inner = Expr::GeomPiece(
            IntervalKind::Barcode,
            IntervalShape::FixedLen(S(16, span())),
        );
        let self_expr = Expr::Self_;

        let map = Expr::Function(
            S(
                Function::Map("file.tsv".into(), S(Box::new(self_expr.clone()), span())),
                span(),
            ),
            S(Box::new(inner.clone()), span()),
        );
        let s = format!("{}", map);
        assert!(s.contains("map("));
        assert!(s.contains("file.tsv"));

        let map_mm = Expr::Function(
            S(
                Function::MapWithMismatch(
                    "file.tsv".into(),
                    S(Box::new(self_expr.clone()), span()),
                    1,
                ),
                span(),
            ),
            S(Box::new(inner.clone()), span()),
        );
        let s = format!("{}", map_mm);
        assert!(s.contains("map_with_mismatch("));

        let map_edit = Expr::Function(
            S(
                Function::MapWithEdit("file.tsv".into(), S(Box::new(self_expr.clone()), span()), 1),
                span(),
            ),
            S(Box::new(inner.clone()), span()),
        );
        let s = format!("{}", map_edit);
        assert!(s.contains("map_with_edit("));
    }

    #[test]
    fn test_make_geom_piece_no_label() {
        let piece = make_geom_piece(
            IntervalKind::Barcode,
            IntervalShape::FixedLen(S(16, span())),
            None,
            span(),
        );
        assert!(matches!(piece, Expr::GeomPiece(IntervalKind::Barcode, _)));
    }

    #[test]
    fn test_make_geom_piece_with_label() {
        let piece = make_geom_piece(
            IntervalKind::Barcode,
            IntervalShape::FixedLen(S(16, span())),
            Some(Expr::Label(S("bc1".to_string(), span()))),
            span(),
        );
        assert!(matches!(piece, Expr::LabeledGeomPiece(_, _)));
    }

    // ---------------------------------------------------------------
    // SampleBarcode (`s`) tests
    // ---------------------------------------------------------------

    /// Helper: lex and parse a source string through the full pipeline.
    fn parse_full(src: &str) -> Description {
        use chumsky::input::Input;
        use chumsky::Parser as _;

        let tokens = crate::lexer::lexer()
            .parse(src)
            .into_result()
            .expect("lex errors");
        let spanned = tokens
            .into_iter()
            .map(|(tok, span)| chumsky::span::Spanned { inner: tok, span })
            .collect::<Vec<_>>();
        let input = spanned[..].split_spanned((0..src.len()).into());
        let result = parser().parse(input).into_result().expect("parse errors");
        result
    }

    #[test]
    fn test_parse_sample_barcode_fixed_length() {
        let desc = parse_full("1{s[8]r:}");
        let reads = desc.reads.0;
        assert_eq!(reads.len(), 1);
        let first = &reads[0].0.exprs[0].0;
        match first {
            Expr::GeomPiece(IntervalKind::SampleBarcode, IntervalShape::FixedLen(S(n, _))) => {
                assert_eq!(*n, 8);
            }
            other => panic!(
                "expected GeomPiece(SampleBarcode, FixedLen(8)), got {:?}",
                other
            ),
        }
    }

    #[test]
    fn test_parse_sample_barcode_labeled() {
        let desc = parse_full("1{s<sample>[8]r:}");
        let reads = desc.reads.0;
        assert_eq!(reads.len(), 1);
        let first = &reads[0].0.exprs[0].0;
        match first {
            Expr::LabeledGeomPiece(S(label, _), S(inner, _)) => {
                assert_eq!(label, "sample");
                match inner.as_ref() {
                    Expr::GeomPiece(
                        IntervalKind::SampleBarcode,
                        IntervalShape::FixedLen(S(n, _)),
                    ) => {
                        assert_eq!(*n, 8);
                    }
                    other => panic!(
                        "inner expected GeomPiece(SampleBarcode, FixedLen(8)), got {:?}",
                        other
                    ),
                }
            }
            other => panic!("expected LabeledGeomPiece, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_sample_barcode_ranged() {
        let desc = parse_full("1{s[6-8]r:}");
        let first = &desc.reads.0[0].0.exprs[0].0;
        match first {
            Expr::GeomPiece(
                IntervalKind::SampleBarcode,
                IntervalShape::RangedLen(S((a, b), _)),
            ) => {
                assert_eq!(*a, 6);
                assert_eq!(*b, 8);
            }
            other => panic!(
                "expected GeomPiece(SampleBarcode, RangedLen(6,8)), got {:?}",
                other
            ),
        }
    }

    #[test]
    fn test_parse_10x_flex_full_geometry() {
        let desc = parse_full("1{b[16]u[12]}2{r:x[28]s[8]}");
        let reads = desc.reads.0;
        assert_eq!(reads.len(), 2);

        // Read 1: b[16], u[12]
        let r1 = &reads[0].0;
        assert_eq!(r1.index.0, 1);
        assert!(matches!(
            r1.exprs[0].0,
            Expr::GeomPiece(IntervalKind::Barcode, IntervalShape::FixedLen(_))
        ));
        assert!(matches!(
            r1.exprs[1].0,
            Expr::GeomPiece(IntervalKind::Umi, IntervalShape::FixedLen(_))
        ));

        // Read 2: r:, x[28], s[8]
        let r2 = &reads[1].0;
        assert_eq!(r2.index.0, 2);
        assert!(matches!(
            r2.exprs[0].0,
            Expr::GeomPiece(IntervalKind::ReadSeq, IntervalShape::UnboundedLen)
        ));
        assert!(matches!(
            r2.exprs[1].0,
            Expr::GeomPiece(IntervalKind::Discard, IntervalShape::FixedLen(_))
        ));
        match &r2.exprs[2].0 {
            Expr::GeomPiece(IntervalKind::SampleBarcode, IntervalShape::FixedLen(S(n, _))) => {
                assert_eq!(*n, 8);
            }
            other => panic!("expected last expr to be SampleBarcode[8], got {:?}", other),
        }
    }

    #[test]
    fn test_expr_display_sample_barcode_roundtrip() {
        let e = Expr::GeomPiece(
            IntervalKind::SampleBarcode,
            IntervalShape::FixedLen(S(8, span())),
        );
        assert_eq!(format!("{}", e), "s[8]");
    }
}
