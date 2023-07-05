use std::collections::HashMap;

use super::syntax::*;
use chumsky::prelude::*;

pub fn parser() -> impl Parser<char, Expression, Error = Simple<char>> {
    // parse data associated with features: ints, sequences, labels

    // for labeling segments, not supported in enum yet
    let label = text::ident()
        .repeated()
        .collect::<String>()
        .map(Label::Label)
        .delimited_by(just('<'), just('>'));

    let int = text::int(10).map(|s: String| s.parse().unwrap());

    let range = int
        .then_ignore(just('-'))
        .then(int)
        .map(|(from, to)| VariableGeom::Range(from, to));

    let nucleotide_sequence = one_of("ATGCN")
        .repeated()
        .collect::<String>()
        .map(NucStr::Seq);

    // transformed unbounded

    let transformed_unbounded = {
        let unbounded = one_of("burx")
            .then(label.or_not())
            .then_ignore(just(':'))
            .map(|(peice_type, label)| {
                GeomPiece::Variable(VariableGeomPiece::new(
                    peice_type,
                    label,
                    VariableGeom::Unbounded,
                ))
            });

        recursive(|transformed_unbounded| {
            choice((
                unbounded.clone(),
                text::keyword("remove")
                    .ignore_then(unbounded)
                    .map(|unbounded_piece| {
                        GeomPiece::TransformedPiece(Function::Remove, Box::new(unbounded_piece))
                    }),
                text::keyword("revcomp")
                    .ignore_then(
                        transformed_unbounded
                            .clone()
                            .delimited_by(just('('), just(')')),
                    )
                    .map(|unbounded_piece| {
                        GeomPiece::TransformedPiece(Function::RevComp, Box::new(unbounded_piece))
                    }),
                text::keyword("reverse")
                    .ignore_then(transformed_unbounded.delimited_by(just('('), just(')')))
                    .map(|unbounded_piece| {
                        GeomPiece::TransformedPiece(Function::Reverse, Box::new(unbounded_piece))
                    }),
            ))
        })
    };

    // transformed fixed sequence segment

    let transformed_sequence = {
        let fixed_seq_expr = just('f')
            .then(label.or_not())
            .then(
                nucleotide_sequence
                    .clone()
                    .delimited_by(just('['), just(']')),
            )
            .map(|((_, label), seq)| GeomPiece::Fixed(FixedGeomPiece::new_seq(seq, label)));

        let hamming_fixed = text::keyword("hamming")
            .ignore_then(
                fixed_seq_expr
                    .clone()
                    .then_ignore(just(',').padded())
                    .then(int)
                    .delimited_by(just('('), just(')')),
            )
            .map(|(geom_piece, n)| {
                GeomPiece::TransformedPiece(Function::Hamming(n), Box::new(geom_piece))
            });

        let matched_seq = fixed_seq_expr.or(hamming_fixed).padded();

        recursive(|transformed_sequence| {
            choice((
                matched_seq.clone(),
                text::keyword("remove")
                    .ignore_then(matched_seq)
                    .map(|unbounded_piece| {
                        GeomPiece::TransformedPiece(Function::Remove, Box::new(unbounded_piece))
                    }),
                text::keyword("map")
                    .ignore_then(
                        transformed_sequence
                            .clone()
                            .then_ignore(just(',').padded())
                            .then(
                                take_until(just(',').ignored())
                                    .map(|(path, _)| path.into_iter().collect()),
                            )
                            .padded()
                            .then(nucleotide_sequence.clone())
                            .delimited_by(just('('), just(')')),
                    )
                    .map(|((geom_piece, map_file), fallback)| {
                        GeomPiece::TransformedPiece(
                            Function::Map(map_file, fallback),
                            Box::new(geom_piece),
                        )
                    }),
                text::keyword("revcomp")
                    .ignore_then(
                        transformed_sequence
                            .clone()
                            .delimited_by(just('('), just(')')),
                    )
                    .map(|unbounded_piece| {
                        GeomPiece::TransformedPiece(Function::RevComp, Box::new(unbounded_piece))
                    }),
                text::keyword("reverse")
                    .ignore_then(
                        transformed_sequence
                            .clone()
                            .delimited_by(just('('), just(')')),
                    )
                    .map(|unbounded_piece| {
                        GeomPiece::TransformedPiece(Function::Reverse, Box::new(unbounded_piece))
                    }),
                text::keyword("trim")
                    .ignore_then(
                        transformed_sequence
                            .clone()
                            .then_ignore(just(',').padded())
                            .then(int)
                            .delimited_by(just('('), just(')')),
                    )
                    .map(|(geom_piece, n)| {
                        GeomPiece::TransformedPiece(Function::Trim(n), Box::new(geom_piece))
                    }),
                text::keyword("pad")
                    .ignore_then(
                        transformed_sequence
                            .then_ignore(just(',').padded())
                            .then(int)
                            .delimited_by(just('('), just(')')),
                    )
                    .map(|(geom_piece, n)| {
                        GeomPiece::TransformedPiece(Function::Pad(n), Box::new(geom_piece))
                    }),
            ))
        })
    };

    // transformed variable segment

    let transform_ranged = {
        let geom_piece_ranged = one_of("burx")
            .then(label.or_not())
            .then_ignore(just('['))
            .then(range)
            .then_ignore(just(']'))
            .map(|((peice_type, label), range)| {
                GeomPiece::Variable(VariableGeomPiece::new(peice_type, label, range))
            });

        let normalized_ranged = text::keyword("norm")
            .ignore_then(geom_piece_ranged.clone().delimited_by(just('('), just(')')))
            .map(|geom_piece| GeomPiece::TransformedPiece(Function::Norm, Box::new(geom_piece)));

        geom_piece_ranged.or(recursive(|normalized_ranged_transformation| {
            choice((
                normalized_ranged.clone(),
                text::keyword("remove")
                    .ignore_then(normalized_ranged.delimited_by(just('('), just(')')))
                    .map(|geom_p| GeomPiece::TransformedPiece(Function::Remove, Box::new(geom_p))),
                text::keyword("map")
                    .ignore_then(
                        normalized_ranged_transformation
                            .clone()
                            .then_ignore(just(',').padded())
                            .then(
                                take_until(just(',').ignored())
                                    .map(|(path, _)| path.into_iter().collect()),
                            )
                            .padded()
                            .then(nucleotide_sequence.clone())
                            .delimited_by(just('('), just(')')),
                    )
                    .map(|((geom_piece, map_file), fallback)| {
                        GeomPiece::TransformedPiece(
                            Function::Map(map_file, fallback),
                            Box::new(geom_piece),
                        )
                    }),
                text::keyword("revcomp")
                    .ignore_then(
                        normalized_ranged_transformation
                            .clone()
                            .delimited_by(just('('), just(')')),
                    )
                    .map(|unbounded_piece| {
                        GeomPiece::TransformedPiece(Function::RevComp, Box::new(unbounded_piece))
                    }),
                text::keyword("reverse")
                    .ignore_then(
                        normalized_ranged_transformation
                            .clone()
                            .delimited_by(just('('), just(')')),
                    )
                    .map(|unbounded_piece| {
                        GeomPiece::TransformedPiece(Function::Reverse, Box::new(unbounded_piece))
                    }),
                text::keyword("trim")
                    .ignore_then(
                        normalized_ranged_transformation
                            .clone()
                            .then_ignore(just(',').padded())
                            .then(int)
                            .delimited_by(just('('), just(')')),
                    )
                    .map(|(geom_piece, n)| {
                        GeomPiece::TransformedPiece(Function::Trim(n), Box::new(geom_piece))
                    }),
                text::keyword("pad")
                    .ignore_then(
                        normalized_ranged_transformation
                            .then_ignore(just(',').padded())
                            .then(int)
                            .delimited_by(just('('), just(')')),
                    )
                    .map(|(geom_piece, n)| {
                        GeomPiece::TransformedPiece(Function::Pad(n), Box::new(geom_piece))
                    }),
            ))
        }))
    };

    // transformed fixed (non-sequence) segment

    let transform_fixed = {
        let geom_piece_fixed = one_of("burx")
            .then(label.or_not())
            .then_ignore(just('['))
            .then(int)
            .then_ignore(just(']'))
            .map(|((peice_type, label), len)| {
                GeomPiece::Fixed(FixedGeomPiece::new_fixed(
                    peice_type,
                    label,
                    FixedGeom::Len(len),
                ))
            });

        recursive(|transform_fixed| {
            choice((
                geom_piece_fixed.clone(),
                text::keyword("map")
                    .ignore_then(
                        transform_fixed
                            .clone()
                            .then_ignore(just(',').padded())
                            .then(
                                take_until(just(',').ignored())
                                    .map(|(path, _)| path.into_iter().collect()),
                            )
                            .padded()
                            .then(nucleotide_sequence.clone())
                            .delimited_by(just('('), just(')')),
                    )
                    .map(|((geom_piece, map_file), fallback)| {
                        GeomPiece::TransformedPiece(
                            Function::Map(map_file, fallback),
                            Box::new(geom_piece),
                        )
                    }),
                text::keyword("revcomp")
                    .ignore_then(transform_fixed.clone().delimited_by(just('('), just(')')))
                    .map(|geom_piece| {
                        GeomPiece::TransformedPiece(Function::RevComp, Box::new(geom_piece))
                    }),
                text::keyword("remove")
                    .ignore_then(transform_fixed.clone().delimited_by(just('('), just(')')))
                    .map(|geom_piece| {
                        GeomPiece::TransformedPiece(Function::Remove, Box::new(geom_piece))
                    }),
                text::keyword("rev")
                    .ignore_then(transform_fixed.clone().delimited_by(just('('), just(')')))
                    .map(|geom_piece| {
                        GeomPiece::TransformedPiece(Function::Reverse, Box::new(geom_piece))
                    }),
                text::keyword("trim")
                    .ignore_then(
                        transform_fixed
                            .clone()
                            .then_ignore(just(',').padded())
                            .then(int)
                            .delimited_by(just('('), just(')')),
                    )
                    .map(|(geom_piece, n)| {
                        GeomPiece::TransformedPiece(Function::Trim(n), Box::new(geom_piece))
                    }),
                text::keyword("pad")
                    .ignore_then(
                        transform_fixed
                            .then_ignore(just(',').padded())
                            .then(int)
                            .delimited_by(just('('), just(')')),
                    )
                    .map(|(geom_piece, n)| {
                        GeomPiece::TransformedPiece(Function::Pad(n), Box::new(geom_piece))
                    }),
            ))
        })
    };

    // the following are "primitive" segment types

    let fixed = one_of("burx")
        .then(label.or_not())
        .then_ignore(just('['))
        .then(int)
        .then_ignore(just(']'))
        .map(|((peice_type, label), len)| {
            FixedGeomPiece::new_fixed(peice_type, label, FixedGeom::Len(len))
        });

    let fixed_sequence = just('f')
        .then(label.or_not())
        .then(
            nucleotide_sequence
                .clone()
                .delimited_by(just('['), just(']')),
        )
        .map(|((_, label), seq)| FixedGeomPiece::new_seq(seq, label));

    let ranged = one_of("burx")
        .then(label.or_not())
        .then_ignore(just('['))
        .then(range)
        .then_ignore(just(']'))
        .map(|((peice_type, label), range)| VariableGeomPiece::new(peice_type, label, range));

    let unbounded = one_of("burx")
        .then(label.or_not())
        .then_ignore(just(':'))
        .map(|(peice_type, label)| {
            VariableGeomPiece::new(peice_type, label, VariableGeom::Unbounded)
        });

    let bounded = choice((
        fixed
            .clone()
            .or(fixed_sequence.clone())
            .map(|fixed| BoundedGeom {
                variable: None,
                fixed,
            }),
        ranged
            .clone()
            .or(unbounded.clone())
            .then(fixed_sequence.clone())
            .map(|(variable, fixed)| BoundedGeom {
                variable: Some(variable),
                fixed,
            }),
    ))
    .padded();

    let description = choice((
        bounded
            .clone()
            .repeated()
            .at_least(1)
            .then(choice((ranged.clone(), unbounded.clone())).or_not())
            .map(|(bounded, variable)| {
                Description::CompositeGeom(CompositeGeom { bounded, variable })
            }),
        unbounded
            .clone()
            .or(ranged.clone())
            .map(Description::Variable),
    ));

    // def associates labels with geom_pieces
    let definitions = text::ident()
        .padded()
        .map(Label::Label)
        .then_ignore(just('='))
        .padded()
        .then(choice((
            transform_fixed,
            transform_ranged,
            transformed_unbounded,
            transformed_sequence,
        )))
        .map(|(label, geom_piece)| Definition { label, geom_piece })
        .padded()
        .repeated()
        .validate(move |definitions, _emit, _span| {
            let mut label_map: HashMap<Label, GeomPiece> = HashMap::new();

            for Definition { label, geom_piece } in &definitions {
                // validate not already there
                label_map.insert(label.clone(), geom_piece.clone());
            }

            label_map
        });

    let read = int
        .then(description.clone().delimited_by(just('{'), just('}')))
        .map(|(num, description)| Read { num, description })
        .repeated()
        .exactly(2);

    definitions
        .then(read)
        .then_ignore(text::keyword("->").or_not())
        .map(|(definitions, geometry)| Expression {
            definitions,
            geometry,
            transformation: None,
        })
        .then_ignore(end())
}
