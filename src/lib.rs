use chumsky::prelude::*;
use core::panic;
use std::ops::RangeBounds;

use antisequence::{
    expr::{SelectorExpr, TransformExpr},
    *,
};

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

fn process_composite_segment(
    pipeline: Box<dyn antisequence::Reads>,
    variable_segment: Option<Segment>,
    fixed_segment: Segment,
    label: &str,
) -> Box<dyn antisequence::Reads> {
    let mut starting_label = label.to_string();

    if !label.contains('_') {
        starting_label = format!("{}*", label);
    }

    if let Some(segment) = variable_segment {
        // align fixed to the right
        // variable at the left
        match fixed_segment {
            Segment::FixedSequence(_, sequence) => {
                if let SegmentData::Sequence(s) = sequence {
                    let pattern =
                        format!("\n    name: _anchor\n    patterns:\n        - pattern: \"{s}\"\n");

                    let match_transform_expression = TransformExpr::new(
                        format!("{0} -> {1}_l, {1}_anchor, {1}_r", starting_label, label)
                            .as_bytes(),
                    )
                    .unwrap();
                    let match_selector_expression =
                        SelectorExpr::new(format!("{label}_anchor").as_bytes()).unwrap();
                    let pipeline = match_pattern(
                        pipeline,
                        match_transform_expression,
                        match_selector_expression,
                        pattern,
                        LocalAln {
                            identity: 0.83,
                            overlap: 1.0,
                        },
                    );

                    match segment {
                        Segment::Ranged(_, r) => {
                            let Range { from, to } = r;
                            let length_transform_expression = TransformExpr::new(
                                format!("{0}_l -> {0}_l.v_len", label).as_bytes(),
                            )
                            .unwrap();

                            let length_selected_expression =
                                SelectorExpr::new(format!("{label}_l.v_len").as_bytes()).unwrap();

                            validate_length(
                                pipeline,
                                length_transform_expression,
                                length_selected_expression,
                                from..to + 1,
                            )
                        }
                        Segment::Unbounded(_) => pipeline,
                        _ => panic!(
                            "Expected a ranged or unbounded segment, found: {:?}",
                            segment
                        ),
                    }
                } else {
                    panic!("Expected a sequence to match, found: {:?}", sequence)
                }
            }
            _ => panic!(
                "Expected a fixed sequence segment, found: {:?}",
                fixed_segment
            ),
        }
    } else {
        // align fixed to the left
        // rest on the right
        match fixed_segment {
            Segment::FixedLength(_, d) => {
                if let SegmentData::Num(n) = d {
                    let cut_expression = TransformExpr::new(
                        format!("{0} -> {1}_l, {1}_r", starting_label, label).as_bytes(),
                    )
                    .unwrap();
                    let length_expression =
                        TransformExpr::new(format!("{0}_l -> {0}_l.v_len", label).as_bytes())
                            .unwrap();
                    let selector_expression =
                        SelectorExpr::new(format!("{}_l.v_len", label).as_bytes()).unwrap();

                    cut_validate_length(
                        pipeline,
                        cut_expression,
                        length_expression,
                        selector_expression,
                        LeftEnd(n),
                        n..n + 1,
                    )
                } else {
                    panic!("Expected a number, found: {:?}", d)
                }
            }
            Segment::FixedSequence(_, d) => {
                if let SegmentData::Sequence(s) = d {
                    let pattern =
                        format!("\n    name: _anchor\n    patterns:\n        - pattern: \"{s}\"\n");
                    let transform_expression = TransformExpr::new(
                        format!("{0} -> {1}_anchor, {1}_r", starting_label, label).as_bytes(),
                    )
                    .unwrap();
                    let selector_expression =
                        SelectorExpr::new(format!("{label}_anchor").as_bytes()).unwrap();

                    match_pattern(
                        pipeline,
                        transform_expression,
                        selector_expression,
                        pattern,
                        PrefixAln {
                            identity: 0.83,
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
}

fn process_variable_segment(
    pipeline: Box<dyn antisequence::Reads>,
    segment: Segment,
    label: &str,
) -> Box<dyn antisequence::Reads> {
    let mut starting_label = label.to_string();

    if !label.contains('_') {
        starting_label = format!("{}*", label);
    }

    match segment {
        Segment::Ranged(_, r) => {
            let Range { from, to } = r;

            let cut_expression = TransformExpr::new(
                format!("{0} -> {1}_l, {1}_r", starting_label, label).as_bytes(),
            )
            .unwrap();
            let length_expression =
                TransformExpr::new(format!("{0}_l -> {0}_l.v_len", label).as_bytes()).unwrap();
            let selector_expression =
                SelectorExpr::new(format!("{}_l.v_len", label).as_bytes()).unwrap();

            cut_validate_length(
                pipeline,
                cut_expression,
                length_expression,
                selector_expression,
                LeftEnd(to),
                from..to + 1,
            )
        }
        Segment::Unbounded(_) => pipeline,
        _ => panic!("Expected a variable segment, recieved: {:?}", segment),
    }
}

// this method will create the antisequence pipeline
impl SegmentComposite {
    fn build_pipeline(self, fastq_read: Box<dyn antisequence::Reads>, outfile: String) {
        let mut pipeline: Box<dyn Reads> = fastq_read;

        // need to keep this for the first call then remove the * for the next
        let mut label = vec!["seq1."];

        match self {
            Self::BoundedToMaybeRangedOrUnbounded(bounded_segments, variable_segment) => {
                for bounded_segment in bounded_segments {
                    match bounded_segment {
                        BoundedSegment::Fixed(fixed_segment) => {
                            pipeline = process_composite_segment(
                                pipeline,
                                None,
                                fixed_segment,
                                label.join("_").as_str(),
                            );
                            label.push("r");
                        }
                        BoundedSegment::VariableLenToSequence(
                            variable_segment,
                            sequence_segment,
                        ) => {
                            pipeline = process_composite_segment(
                                pipeline,
                                Some(variable_segment),
                                sequence_segment,
                                label.join("_").as_str(),
                            );
                            label.push("r");
                        }
                    }
                }
                if let Some(segment) = variable_segment {
                    pipeline =
                        process_variable_segment(pipeline, segment, label.join("_").as_str());
                    label.push("l");
                }
            }
            Self::VariableLen(segment) => {
                pipeline = process_variable_segment(pipeline, segment, label.join("_").as_str());
                label.push("l");
            }
        };

        pipeline
            .collect_fastq1(sel!(), outfile)
            .dbg(sel!())
            .run()
            .unwrap_or_else(|e| panic!("{e}"));
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
        .repeated();

    read_description.then_ignore(end())
}

pub fn interpret(infile: String, outfile: String, read_descriptions: Vec<ReadDescription>) {
    for read_description in read_descriptions {
        let ReadDescription {
            num: _num,
            description,
        } = read_description;

        let iter_read: Box<dyn Reads> = iter_fastq1(infile.clone(), 256)
            .unwrap_or_else(|e| panic!("{e}"))
            .boxed();

        description.build_pipeline(iter_read, outfile.clone());
    }
}

fn cut_validate_length<B>(
    read: Box<dyn antisequence::Reads>,
    cut_transform_expression: TransformExpr,
    length_transform_expression: TransformExpr,
    selector_expression: SelectorExpr,
    cut_index: EndIdx,
    bound: B,
) -> Box<dyn antisequence::Reads>
where
    B: RangeBounds<usize> + Send + Sync + 'static,
{
    read.cut(sel!(), cut_transform_expression, cut_index)
        .length_in_bounds(sel!(), length_transform_expression, bound)
        .retain(selector_expression)
        .boxed()
}

fn match_pattern(
    read: Box<dyn antisequence::Reads>,
    transform_expression: TransformExpr,
    selector_expression: SelectorExpr,
    pattern: String,
    match_type: MatchType,
) -> Box<dyn antisequence::Reads> {
    read.match_any(sel!(), transform_expression, pattern, match_type)
        .retain(selector_expression)
        .boxed()
}

fn validate_length<B>(
    read: Box<dyn antisequence::Reads>,
    length_transform_expression: TransformExpr,
    selector_expression: SelectorExpr,
    bound: B,
) -> Box<dyn antisequence::Reads>
where
    B: RangeBounds<usize> + Send + Sync + 'static,
{
    read.length_in_bounds(sel!(), length_transform_expression, bound)
        .retain(selector_expression)
        .boxed()
}
