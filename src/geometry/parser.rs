use crate::lexer::{Span, Token};
use chumsky::prelude::*;
use std::{fmt, ops::Deref};

pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Size {
    FixedSeq(Spanned<String>),
    FixedLen(Spanned<usize>),
    RangedLen(Spanned<(usize, usize)>),
    UnboundedLen,
}

impl fmt::Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Size::*;
        match self {
            FixedLen((n, _)) => write!(f, "[{}]", n),
            FixedSeq((s, _)) => write!(f, "[{}]", s),
            RangedLen(((a, b), _)) => write!(f, "[{}-{}]", a, b),
            UnboundedLen => write!(f, ":"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Function {
    Reverse,
    ReverseComp,
    Truncate(usize),
    Remove,
    Pad(usize),
    Normalize,
    Map(String, Box<Spanned<Expr>>),
    MapWithMismatch(String, Box<Spanned<Expr>>, usize),
    Hamming(usize),
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Function::*;
        match self {
            Reverse => write!(f, "rev"),
            ReverseComp => write!(f, "revcomp"),
            Truncate(n) => write!(f, "trunc({}", n),
            Remove => write!(f, "remove"),
            Pad(n) => write!(f, "pad({}", n),
            Normalize => write!(f, "norm"),
            Map(p, b) => {
                let (s, _) = b.deref();
                write!(f, "map({}, {}", p, s)
            }
            MapWithMismatch(p, b, n) => {
                let (s, _) = b.deref();
                write!(f, "map({}, {}, {}", p, s, n)
            }
            Hamming(n) => write!(f, "hamming({}", n),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Barcode,
    Umi,
    Discard,
    ReadSeq,
    FixedSeq,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Type::*;
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
    Label(Spanned<String>),
    Type(Spanned<Type>),
    GeomPiece(Type, Size),
    LabeledGeomPiece(Box<Self>, Box<Spanned<Self>>),
    Function(Spanned<Function>, Box<Spanned<Self>>),
    Read(Spanned<usize>, Vec<Spanned<Self>>),
    Definitions(Vec<Spanned<Self>>),
    Transform(Vec<Self>),
    Description(
        Box<Option<Spanned<Self>>>,
        Spanned<Vec<Self>>,
        Box<Spanned<Option<Self>>>,
    ),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expr::*;
        match self {
            Error => write!(f, "Error"),
            Self_ => write!(f, "self"),
            Label((s, _)) => write!(f, "{}", s),
            Type((t, _)) => write!(f, "{}", t),
            GeomPiece(t, s) => write!(f, "{}{}", t, s),
            LabeledGeomPiece(l, box_) => {
                let (expr, _) = box_.deref();

                write!(f, "{}={}", l, expr)
            }
            Function((fn_, _), box_) => {
                let (expr, _) = box_.deref();
                write!(f, "{}({}))", fn_, expr)
            }
            Read((n, _), exprs) => write!(
                f,
                "{n}{{{}}}",
                exprs
                    .iter()
                    .map(|(x, _)| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Definitions(exprs) => write!(
                f,
                "def(\n{}\n)",
                exprs
                    .iter()
                    .map(|(x, _)| x.to_string())
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
                let d_w = if let Some((d, _)) = d.deref() {
                    Some(format!("{}", d))
                } else {
                    None
                };

                let t_w = if let (Some(t), _) = t.deref() {
                    Some(format!("{}", t))
                } else {
                    None
                };

                let (r, _) = r;

                let r_w = r
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");

                if let Some(d_s) = d_w {
                    if let Some(t_s) = t_w {
                        write!(f, "{}\n{}\n{}", d_s, r_w, t_s)
                    } else {
                        write!(f, "{}\n{}", d_s, r_w)
                    }
                } else {
                    write!(f, "{}", r_w)
                }
            }
        }
    }
}

pub fn parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
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

    let piece_type = select! {
        Token::Barcode => Type::Barcode,
        Token::Umi => Type::Umi,
        Token::Discard => Type::Discard,
        Token::ReadSeq => Type::ReadSeq,
    }
    .labelled("Piece Type");

    let nuc = select! {
        Token::U => 'U',
        Token::A => 'A',
        Token::T => 'T',
        Token::G => 'G',
        Token::C => 'C',
    };

    let label = ident
        .map_with_span(|l, span| Expr::Label((l, span)))
        .labelled("Label");

    let self_ = just(Token::Self_).to(Expr::Self_).labelled("Self");

    let range = just(Token::Ctrl('['))
        .ignored()
        .then(
            num.then_ignore(just(Token::Special('-')))
                .then(num)
                .map_with_span(|(a, b), span| Size::RangedLen(((a, b), span)))
                .then_ignore(just(Token::Ctrl(']'))),
        )
        .labelled("Range");

    let fixed_len = just(Token::Ctrl('['))
        .ignore_then(num.map_with_span(|n, span| Size::FixedLen((n, span))))
        .then_ignore(just(Token::Ctrl(']')))
        .labelled("Fixed Length");

    let seq = nuc.repeated().collect::<String>();

    let nucstr = just(Token::Ctrl('['))
        .ignore_then(seq.map_with_span(|nucstr, span| Size::FixedSeq((nucstr, span))))
        .then_ignore(just(Token::Ctrl(']')))
        .labelled("Nucleotide String");

    let unbounded = piece_type
        .then(label.or_not())
        .then_ignore(just(Token::Special(':')))
        .map_with_span(|(type_, label), span| {
            let expr = Expr::GeomPiece(type_, Size::UnboundedLen);
            if let Some(label) = label {
                Expr::LabeledGeomPiece(Box::new(label), Box::new((expr, span)))
            } else {
                expr
            }
        })
        .labelled("Unbounded Segment");

    let ranged = piece_type
        .then(label.or_not())
        .then(range)
        .map_with_span(|((type_, label), (_, range)), span| {
            let expr = Expr::GeomPiece(type_, range);
            if let Some(label) = label {
                Expr::LabeledGeomPiece(Box::new(label), Box::new((expr, span)))
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
                Expr::LabeledGeomPiece(Box::new(label), Box::new((expr, span)))
            } else {
                expr
            }
        })
        .labelled("Fixed Length Segment");

    let fixed_seq = just(Token::FixedSeq)
        .to(Type::FixedSeq)
        .then(label.or_not())
        .then(nucstr)
        .map_with_span(|((type_, label), nucs), span| {
            let expr = Expr::GeomPiece(type_, nucs);
            if let Some(label) = label {
                Expr::LabeledGeomPiece(Box::new(label), Box::new((expr, span)))
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
            .then_ignore(just(Token::Ctrl(',')))
            .then(num)
            .map_with_span(|tok, span| (tok, span))
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

        let num_arg = geom_piece
            .clone()
            .then_ignore(just(Token::Ctrl(',')))
            .then(num)
            .map_with_span(|tok, span| (tok, span))
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

        let recursive_no_arg = transformed_pieces
            .clone()
            .map_with_span(|tok, span| (tok, span))
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

        choice((
            geom_piece.clone(),
            just(Token::Remove)
                .map_with_span(|_, span| (Function::Remove, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, Box::new(tok)))
                .labelled("Remove function"),
            just(Token::Normalize)
                .map_with_span(|_, span| (Function::Normalize, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, Box::new(tok)))
                .labelled("Normalize function"),
            just(Token::Hamming)
                .map_with_span(|_, span| span)
                .then(num_arg)
                .map(|(fn_span, ((geom_p, num), span))| {
                    Expr::Function((Function::Hamming(num), fn_span), Box::new((geom_p, span)))
                })
                .labelled("Hamming function"),
            just(Token::Truncate)
                .map_with_span(|_, span| span)
                .then(recursive_num_arg.clone())
                .map(|(fn_span, ((geom_p, num), span))| {
                    Expr::Function((Function::Truncate(num), fn_span), Box::new((geom_p, span)))
                })
                .labelled("Truncate function"),
            just(Token::Pad)
                .map_with_span(|_, span| span)
                .then(recursive_num_arg)
                .map(|(fn_span, ((geom_p, num), span))| {
                    Expr::Function((Function::Pad(num), fn_span), Box::new((geom_p, span)))
                })
                .labelled("Pad function"),
            just(Token::Reverse)
                .map_with_span(|_, span| (Function::Reverse, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, Box::new(tok)))
                .labelled("Reverse function"),
            just(Token::ReverseComp)
                .map_with_span(|_, span| (Function::ReverseComp, span))
                .then(recursive_no_arg.clone())
                .map(|(fn_, tok)| Expr::Function(fn_, Box::new(tok)))
                .labelled("Reverse Compliment function"),
            just(Token::Map)
                .map_with_span(|_, span| span)
                .then(
                    geom_piece
                        .clone()
                        .then_ignore(just(Token::Ctrl(',')))
                        .then(file)
                        .then_ignore(just(Token::Ctrl(',')))
                        .then(
                            transformed_pieces
                                .clone()
                                .map_with_span(|tok, span| (tok, span)),
                        )
                        .map_with_span(|tok, span| (tok, span))
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
                )
                .map(|(fn_span, (((geom_p, path), self_expr), span))| {
                    Expr::Function(
                        (Function::Map(path, Box::new(self_expr)), fn_span),
                        Box::new((geom_p, span)),
                    )
                })
                .labelled("Map function"),
            just(Token::MapWithMismatch)
                .map_with_span(|_, span| span)
                .then(
                    geom_piece
                        .then_ignore(just(Token::Ctrl(',')))
                        .then(file)
                        .then_ignore(just(Token::Ctrl(',')))
                        .then(
                            transformed_pieces
                                .clone()
                                .map_with_span(|tok, span| (tok, span)),
                        )
                        .then_ignore(just(Token::Ctrl(',')))
                        .then(num)
                        .map_with_span(|tok, span| (tok, span))
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
                )
                .map(|(fn_span, ((((geom_p, path), self_expr), num), span))| {
                    Expr::Function(
                        (
                            Function::MapWithMismatch(path, Box::new(self_expr), num),
                            fn_span,
                        ),
                        Box::new((geom_p, span)),
                    )
                })
                .labelled("Map function"),
        ))
    })
    .map_with_span(|tok, span| (tok, span));

    let definitions = ident
        .map_with_span(|tok, span| Expr::Label((tok, span)))
        .then_ignore(just(Token::Special('=')))
        .then(transformed_pieces.clone())
        .map_with_span(|(label, geom_p), span| {
            (
                Expr::LabeledGeomPiece(Box::new(label), Box::new(geom_p)),
                span,
            )
        })
        .repeated()
        .at_least(1);

    let reads = num
        .map_with_span(|tok, span| (tok, span))
        .then(
            transformed_pieces
                .clone()
                .repeated()
                .at_least(1)
                .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))),
        )
        .map(|(n, read)| Expr::Read(n, read))
        .repeated()
        .at_least(2)
        .at_most(2)
        .collect::<Vec<_>>();

    let transformation = choice((
        end().map_with_span(|_, span| (None, span)),
        just(Token::TransformTo)
            .ignore_then(num)
            .map_with_span(|tok, span| (tok, span))
            .then(
                transformed_pieces
                    .clone()
                    .repeated()
                    .at_least(1)
                    .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))),
            )
            .map(|(n, read)| Expr::Read(n, read))
            .repeated()
            .at_least(1)
            .at_most(2)
            .map_with_span(|val, span| (Some(Expr::Transform(val)), span)),
    ));

    // let transformation = just(Token::TransformTo)
    //     .ignore_then(num)
    //     .map_with_span(|tok, span| (tok, span))
    //     .then(
    //         transformed_pieces
    //             .clone()
    //             .repeated()
    //             .at_least(1)
    //             .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))),
    //     )
    //     .map(|(n, read)| Expr::Read(n, read))
    //     .repeated()
    //     .at_least(1)
    //     .at_most(2)
    //     .map(Expr::Transform);

    definitions
        .map_with_span(|tok, span| (Expr::Definitions(tok), span))
        .or_not()
        .then(reads.map_with_span(|tok, span| (tok, span)))
        .then(transformation)
        .map(|((d, r), t)| Expr::Description(Box::new(d), r, Box::new(t)))
        .map_with_span(|tok, span| (tok, span))
        .recover_with(skip_then_retry_until([]))
}
