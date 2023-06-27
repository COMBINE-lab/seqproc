use super::syntax::*;
use chumsky::prelude::*;

pub fn parser() -> impl Parser<char, Vec<Read>, Error = Simple<char>> {
    // parse data associated with features: ints, sequences, labels

    // for labeling segments, not supported in enum yet
    let ident = text::ident()
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

    // the following are "primitive" segment types

    let fixed = one_of("burx")
        .then(ident.or_not())
        .then_ignore(just('['))
        .then(int)
        .then_ignore(just(']'))
        .map(|((peice_type, label), len)| {
            FixedGeomPiece::new(peice_type, label, Some(FixedGeom::Len(len)), None)
        });

    let fixed_sequence = just('f')
        .then(ident.or_not())
        .then(
            nucleotide_sequence
                .clone()
                .delimited_by(just('['), just(']')),
        )
        .map(|((peice_type, label), seq)| FixedGeomPiece::new(peice_type, label, None, Some(seq)));

    let ranged = one_of("burx")
        .then(ident.or_not())
        .then_ignore(just('['))
        .then(range)
        .then_ignore(just(']'))
        .map(|((peice_type, label), range)| VariableGeomPiece::new(peice_type, label, range));

    let unbounded = one_of("burx")
        .then(ident.or_not())
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

    let read = int
        .then(description.clone().delimited_by(just('{'), just('}')))
        .map(|(num, description)| Read { num, description })
        .repeated()
        .exactly(2);

    read.then_ignore(end())
}
