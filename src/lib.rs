use antisequence::{
    expr::Label,
    *,
};

use chumsky::prelude::*;
use core::panic;

mod processors;

use crate::processors::*;

#[derive(Debug, Clone)]
pub enum SegmentData {
    Num(usize),
    Label(String),
    Sequence(String),
}

// simple representation of range
#[derive(Debug, Clone)]
pub struct Range {
    pub from: usize,
    pub to: usize,
}

// primitive
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum SegmentType {
    Barcode,
    UMI,
    Sequence,
    Read,
    Discard,
}

// simple annotation of primitive
#[derive(Debug, Clone)]
pub enum Segment {
    FixedLength(SegmentType, SegmentData),
    FixedSequence(SegmentType, SegmentData),
    Ranged(SegmentType, Range),
    Unbounded(SegmentType),
}

#[derive(Debug, Clone)]
pub enum BoundedSegment {
    Fixed(Segment),
    VariableLenToSequence(Segment, Segment),
}

#[derive(Debug, Clone)]
pub enum SegmentComposite {
    BoundedToMaybeRangedOrUnbounded(Vec<BoundedSegment>, Option<Segment>),
    VariableLen(Segment),
}

#[derive(Debug, Clone)]
pub struct ReadDescription {
    pub num: SegmentData,
    pub description: SegmentComposite,
}

impl SegmentType {
    pub fn new(segment_type: char) -> Self {
        match segment_type {
            'b' => SegmentType::Barcode,
            'u' => SegmentType::UMI,
            'x' => SegmentType::Discard,
            'r' => SegmentType::Read,
            'f' => SegmentType::Sequence,
            _ => panic!("Unexpected type: {}", segment_type),
        }
    }
}

macro_rules! range {
    ($from: expr, $to: expr) => {
        Range {
            from: $from,
            to: $to,
        }
    };
}

impl BoundedSegment {
    fn interpret(self, pipeline: Box<dyn Reads>, label: &str) -> Box<dyn Reads> {
        let mut starting_label = label.to_string();

        if !label.contains('_') {
            starting_label = format!("{}*", label);
        }

        match self {
            BoundedSegment::Fixed(fixed_seg) => {
                process_fixed_segment_alone(pipeline, fixed_seg, starting_label, label)
            }
            BoundedSegment::VariableLenToSequence(variable_seg, fixed_seg) => {
                process_variable_then_fixed(
                    pipeline,
                    fixed_seg,
                    variable_seg,
                    starting_label,
                    label,
                )
            }
        }
    }
}

fn process_fixed_segment_alone(
    pipeline: Box<dyn antisequence::Reads>,
    fixed_segment: Segment,
    starting_label: String,
    label: &str,
) -> Box<dyn antisequence::Reads> {
    match fixed_segment {
        Segment::FixedLength(segment_type, d) => {
            if let SegmentData::Num(n) = d {
                let pipeline = validate_length(
                    cut(pipeline, starting_label, label, LeftEnd(n)),
                    label,
                    n..=n,
                );

                match segment_type {
                    SegmentType::Discard => trim(pipeline, vec![make_label(label, "l")]),
                    _ => pipeline,
                }
            } else {
                panic!("Expected a number, found: {:?}", d)
            }
        }
        Segment::FixedSequence(_, d) => {
            if let SegmentData::Sequence(s) = d {
                process_sequence(
                    pipeline,
                    s,
                    starting_label,
                    label,
                    PrefixAln {
                        identity: 1.0,
                        overlap: 1.0,
                    },
                )
            } else {
                panic!("Expected a number, found: {:?}", d)
            }
        }
        _ => panic!("Expected a fixed segment, found: {:?}", fixed_segment),
    }
}

fn process_variable_then_fixed(
    pipeline: Box<dyn Reads>,
    seq_segment: Segment,
    variable_segment: Segment,
    starting_label: String,
    label: &str,
) -> Box<dyn Reads> {
    if let Segment::FixedSequence(_, SegmentData::Sequence(sequence)) = seq_segment {
        match variable_segment {
            Segment::Ranged(segment_type, r) => {
                let Range { from, to } = r;

                let pipeline = process_sequence(
                    pipeline,
                    sequence.clone(),
                    starting_label,
                    label,
                    BoundedAln {
                        identity: 1.0,
                        overlap: 1.0,
                        from,
                        to: to + sequence.len(),
                    },
                );

                let pipeline = validate_length(pipeline, label, from..to + 1);

                let labels = vec![make_label(label, "l")];

                match segment_type {
                    SegmentType::Discard => trim(pipeline, labels),
                    _ => pad(pipeline, labels, to + 1),
                }
            }
            // trim the unbounded segment in this case, otherwise not
            Segment::Unbounded(segment_type) => {
                let pipeline = process_sequence(
                    pipeline,
                    sequence,
                    starting_label,
                    label,
                    LocalAln {
                        identity: 1.0,
                        overlap: 1.0,
                    },
                );

                match segment_type {
                    SegmentType::Discard => trim(pipeline, vec![make_label(label, "l")]),
                    _ => pipeline,
                }
            }
            _ => panic!(
                "Expected a ranged or unbounded segment, found: {:?}",
                variable_segment
            ),
        }
    } else {
        panic!(
            "Expected a fixed sequence segment, found: {:?}",
            seq_segment
        )
    }
}

fn process_ending_variable_segment(
    pipeline: Box<dyn antisequence::Reads>,
    segment: Segment,
    label: &str,
) -> Box<dyn antisequence::Reads> {
    let mut starting_label = label.to_string();

    if !label.contains('_') {
        starting_label = format!("{}*", label);
    }

    match segment {
        Segment::Unbounded(segment_type) => match segment_type {
            SegmentType::Discard => trim(pipeline, vec![Label::new(label.as_bytes()).unwrap()]),
            _ => pipeline,
        },
        Segment::Ranged(segement_type, r) => match segement_type {
            SegmentType::Discard => trim(pipeline, vec![Label::new(label.as_bytes()).unwrap()]),
            _ => {
                let Range { from, to } = r;

                let pipeline = validate_length(
                    cut(pipeline, starting_label, label, LeftEnd(to)),
                    label,
                    from..to + 1,
                );

                trim(
                    pad(pipeline, vec![make_label(label, "l")], to + 1),
                    vec![make_label(label, "r")],
                )
            }
        },
        _ => panic!("Expected a variable segment, recieved: {:?}", segment),
    }
}

// this method will create the antisequence pipeline
impl ReadDescription {
    pub fn build_pipeline(self, fastq_read: Box<dyn antisequence::Reads>) -> Box<dyn Reads> {
        let mut pipeline: Box<dyn Reads> = fastq_read;

        let mut seq_identifier = String::new();
        if let SegmentData::Num(num) = self.num {
            seq_identifier.push_str(format!("seq{}.", num).as_str());
        } else {
            panic!("Expected a number, found: {:?}", self.num)
        }

        let mut label: Vec<&str> = vec![seq_identifier.as_str()];

        match self.description {
            SegmentComposite::BoundedToMaybeRangedOrUnbounded(
                bounded_segments,
                variable_segment,
            ) => {
                for bounded_segment in bounded_segments {
                    pipeline = BoundedSegment::interpret(
                        bounded_segment,
                        pipeline,
                        label.join("_").as_str(),
                    );
                    label.push("r");
                }

                if let Some(segment) = variable_segment {
                    pipeline = process_ending_variable_segment(
                        pipeline,
                        segment,
                        label.join("_").as_str(),
                    );
                } else {
                    pipeline = trim(
                        pipeline,
                        vec![Label::new(label.join("_").as_bytes()).unwrap()],
                    )
                }
            }
            SegmentComposite::VariableLen(segment) => {
                pipeline =
                    process_ending_variable_segment(pipeline, segment, label.join("_").as_str());
            }
        };

        pipeline.boxed()
    }
}

impl Segment {
    // easier for inline decleration of ranged segment
    pub fn new_ranged(segment_type: char, from: SegmentData, to: SegmentData) -> Self {
        match (from, to) {
            (SegmentData::Num(from), SegmentData::Num(to)) => {
                Self::Ranged(SegmentType::new(segment_type), range!(from, to))
            }
            (_, _) => panic!("expected valid range"),
        }
    }
}

pub fn parser() -> impl Parser<char, Vec<ReadDescription>, Error = Simple<char>> {
    // parse data associated with features: ints, sequences, labels

    // for labeling segments, not supported in enum yet
    let ident = text::ident()
        .repeated()
        .collect::<String>()
        .map(SegmentData::Label)
        .delimited_by(just('<'), just('>'));

    let int = text::int(10).map(|s: String| SegmentData::Num(s.parse().unwrap()));

    let nucleotide_sequence = one_of("ATGC")
        .repeated()
        .collect::<String>()
        .map(SegmentData::Sequence);

    // the following are "primitive" segment types

    let fixed = one_of("burx")
        .then(ident.or_not())
        .then_ignore(just('['))
        .then(int)
        .then_ignore(just(']'))
        .map(|((segment_type, _label), size)| {
            Segment::FixedLength(SegmentType::new(segment_type), size)
        });

    let fixed_sequence = just('f')
        .then(ident.or_not())
        .then(
            nucleotide_sequence
                .clone()
                .delimited_by(just('['), just(']')),
        )
        .map(|((_, _label), s)| Segment::FixedSequence(SegmentType::Sequence, s));

    let ranged = one_of("burx")
        .then(ident.or_not())
        .then_ignore(just('['))
        .then(int)
        .then_ignore(just('-'))
        .then(int)
        .then_ignore(just(']'))
        .map(|(((segment_type, _label), from), to)| Segment::new_ranged(segment_type, from, to));

    let unbounded = one_of("burx")
        .then(ident.or_not())
        .then_ignore(just(':'))
        .map(|(segment_type, _label)| Segment::Unbounded(SegmentType::new(segment_type)));

    // composition of primitives

    let bounded = choice((
        fixed
            .clone()
            .or(fixed_sequence.clone())
            .map(BoundedSegment::Fixed),
        ranged
            .clone()
            .or(unbounded.clone())
            .then(fixed_sequence.clone())
            .map(|(variable_length, fixed_sequence)| {
                BoundedSegment::VariableLenToSequence(variable_length, fixed_sequence)
            }),
    ))
    .padded();

    // composition of composition of primitives or just primitives

    let composite_read = choice((
        bounded
            .clone()
            .repeated()
            .at_least(1)
            .then(choice((ranged.clone(), unbounded.clone())).or_not())
            .map(|(bounded_segments, segment)| {
                SegmentComposite::BoundedToMaybeRangedOrUnbounded(bounded_segments, segment)
            }),
        unbounded
            .clone()
            .or(ranged.clone())
            .map(SegmentComposite::VariableLen),
    ));

    // cannot have nested read descriptions (simply specify how many with .at_most())

    let read_description = int
        .then(composite_read.clone().delimited_by(just('{'), just('}')))
        .map(|(num, segments)| ReadDescription {
            num,
            description: segments,
        })
        .repeated()
        .exactly(2);

    read_description.then_ignore(end())
}
