use std::fmt;

use crate::{
    parser::{IntervalKind, IntervalShape},
    Span, S,
};

use super::functions::{ChangeAs, CompiledFunction};

pub type Geometry = Vec<Vec<(Interval, usize)>>;

pub type Transformation = Vec<Vec<String>>;

fn log4_roundup(n: usize) -> usize {
    (n.ilog2() + 1).div_ceil(2) as usize
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ReturnType {
    Ranged,
    FixedLen,
    Unbounded,
    FixedSeq,
    Void,
}

impl fmt::Display for ReturnType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ReturnType::*;
        match self {
            Ranged => write!(f, "Ranged"),
            FixedLen => write!(f, "Fixed Length"),
            Unbounded => write!(f, "Unbounded"),
            FixedSeq => write!(f, "Fixed Sequence"),
            Void => write!(f, "Void"),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub msg: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Error: {}, at {}-{}",
            self.msg, self.span.start, self.span.end
        )
    }
}

impl std::error::Error for Error {}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Interval {
    Named(String),
    Temporary(GeometryMeta),
}

impl fmt::Display for Interval {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Interval::*;
        match self {
            Named(s) => write!(f, "Label: {s}"),
            Temporary(gp) => write!(f, "{gp}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GeometryMeta {
    pub expr: S<GeometryPiece>,
    pub stack: Vec<S<CompiledFunction>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GeometryPiece {
    pub type_: IntervalKind,
    pub size: IntervalShape,
    pub label: Option<String>,
}

impl fmt::Display for GeometryMeta {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Geometry Meta: {}, {:?}", self.expr.0, self.stack)
    }
}

impl fmt::Display for GeometryPiece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {}, {}", self.label, self.type_, self.size)
    }
}

impl GeometryMeta {
    pub fn validate_expr(&self) -> Result<(), Error> {
        let S(expr, expr_span) = &self.expr;

        let expr_type = {
            if let IntervalKind::Discard = expr.type_ {
                ReturnType::Void
            } else {
                match expr.size {
                    IntervalShape::FixedSeq(_) => ReturnType::FixedSeq,
                    IntervalShape::FixedLen(_) => ReturnType::FixedLen,
                    IntervalShape::RangedLen(_) => ReturnType::Ranged,
                    IntervalShape::UnboundedLen => ReturnType::Unbounded,
                }
            }
        };

        let mut return_type = S(expr_type, *expr_span);

        for S(fn_, span) in self.stack.iter().rev() {
            return_type = validate_composition(S(fn_, *span), return_type, &expr.size)?;
        }

        Ok(())
    }
}

pub fn validate_composition(
    S(fn_, fn_span): S<&CompiledFunction>,
    S(return_type, return_type_span): S<ReturnType>,
    size: &IntervalShape,
) -> Result<S<ReturnType>, Error> {
    let (min, max) = match size {
        IntervalShape::FixedSeq(S(seq, _)) => (0, seq.len()),
        &IntervalShape::FixedLen(S(n, _)) => (0, n),
        &IntervalShape::RangedLen(S((a, b), _)) => (a, b),
        IntervalShape::UnboundedLen => (100, 100),
    };

    match *fn_ {
        CompiledFunction::ReverseComp => match return_type {
            ReturnType::Void => Err(Error {
                span: return_type_span,
                msg: "Function Reverse Complement cannot take void element as an argument"
                    .to_string(),
            }),
            _ => Ok(S(return_type, fn_span)),
        },
        CompiledFunction::Reverse => match return_type {
            ReturnType::Void => Err(Error {
                span: return_type_span,
                msg: "Function Reverse cannot take void element as an argument".to_string(),
            }),
            _ => Ok(S(return_type, fn_span)),
        },
        CompiledFunction::Truncate(by) | CompiledFunction::TruncateLeft(by) => {
            if min <= by && max <= by {
                return Err(Error {
                    span: return_type_span,
                    msg: "Cannot truncate by more than the length of the segment".to_string(),
                });
            }

            match return_type {
                ReturnType::Void => Err(Error {
                    span: return_type_span,
                    msg:
                        "Function Truncate and TruncateLeft cannot take void element as an argument"
                            .to_string(),
                }),
                _ => Ok(S(return_type, fn_span)),
            }
        }
        CompiledFunction::TruncateTo(to) | CompiledFunction::TruncateToLeft(to) => {
            if to > max {
                return Err(Error {
                    span: return_type_span,
                    msg: "Cannot truncate to a length greater than the elements length."
                        .to_string(),
                });
            }

            match return_type {
                ReturnType::FixedLen | ReturnType::Unbounded | ReturnType::Ranged => Ok(S(ReturnType::FixedLen, fn_span)),
                ReturnType::FixedSeq => Ok(S(ReturnType::FixedSeq, fn_span)),
                ReturnType::Void => Err(Error {
                    span: return_type_span,
                    msg: "Function TruncateTo and TruncateToLeft cannot take void element as an argument".to_string(),
                }),
            }
        }
        CompiledFunction::Remove => match return_type {
            ReturnType::Void => Err(Error {
                span: return_type_span,
                msg: "Function Remove cannot recieve a void element as an argument".to_string(),
            }),
            _ => Ok(S(ReturnType::Void, fn_span)),
        },
        CompiledFunction::Pad(..) | CompiledFunction::PadLeft(..) => match return_type {
            ReturnType::Void => Err(Error {
                span: return_type_span,
                msg: "Function Pad and PadLeft cannot take void element as an argument".to_string(),
            }),
            _ => Ok(S(return_type, fn_span)),
        },
        CompiledFunction::PadTo(to, ..) | CompiledFunction::PadToLeft(to, ..) => {
            if to < max {
                return Err(Error {
                    span: return_type_span,
                    msg: "Cannot pad to a length less than the elements length.".to_string(),
                });
            }

            match return_type {
                ReturnType::FixedLen |  ReturnType::Unbounded | ReturnType::Ranged  => Ok(S(ReturnType::FixedLen, fn_span)),
                ReturnType::FixedSeq => Ok(S(ReturnType::FixedSeq, fn_span)),
                ReturnType::Void => Err(Error {
                    span: return_type_span,
                    msg: "Function PadTo and PadToLeft cannot take a void element as an argument"
                        .to_string(),
                }),
            }
        }
        CompiledFunction::Normalize => match return_type {
            ReturnType::Ranged => Ok(S(ReturnType::FixedLen, fn_span)),
            _ => Err(Error {
                span: return_type_span,
                msg: format!(
                    "Function Normalize must take ranged element an argument, found: {return_type}"
                ),
            }),
        },
        CompiledFunction::Map(..) | CompiledFunction::MapWithMismatch(..) | CompiledFunction::MapWithEdit(..) => match return_type {
            ReturnType::Ranged | ReturnType::FixedLen | ReturnType::FixedSeq => {
                Ok(S(ReturnType::FixedLen, fn_span))
            }
            _ => Err(Error {
                span: return_type_span,
                msg: format!(
                    "Function Map can recieve a Ranged or Fixed piece as an argument, found: {return_type}"
                ),
            }),
        },
        CompiledFunction::FilterWithinDist(..) => match return_type {
            ReturnType::FixedLen => Ok(S(ReturnType::FixedLen, fn_span)),
            ReturnType::Ranged => Ok(S(ReturnType::Ranged, fn_span)),
            ReturnType::FixedSeq => Ok(S(ReturnType::FixedSeq, fn_span)),
            _ => Err(Error {
                span: return_type_span,
                msg: format!(
                    "Function Filter can recieve a Ranged or Fixed piece as an argument, found: {return_type}"
                ),
            }),
        },
        CompiledFunction::AmbiguityPolicy(_) => Ok(S(return_type, fn_span)),
        CompiledFunction::Hamming(_) => match return_type {
            ReturnType::FixedSeq => Ok(S(ReturnType::FixedSeq, fn_span)),
            _ => Err(Error {
                span: return_type_span,
                msg: format!(
                    "Function Hamming must take Sequence element an argument, found: {return_type}"
                ),
            }),
        },
        CompiledFunction::Edit(_) => match return_type {
            ReturnType::FixedSeq => Ok(S(ReturnType::FixedSeq, fn_span)),
            _ => Err(Error {
                span: return_type_span,
                msg: format!(
                    "Function Edit must take Sequence element an argument, found: {return_type}"
                ),
            }),
        },
        // anchor_relative searches for anchor from position 0 and extracts preceding elements with flexible length
        CompiledFunction::Anchor => match return_type {
            ReturnType::FixedSeq => Ok(S(ReturnType::FixedSeq, fn_span)),
            _ => Err(Error {
                span: return_type_span,
                msg: format!(
                    "Function anchor_relative must wrap a FixedSeq anchor (or hamming-wrapped), found: {return_type}"
                ),
            }),
        },
    }
}

impl GeometryMeta {
    pub fn is_complex(&self) -> bool {
        self.expr.0.is_complex()
    }

    pub fn get_label(&self) -> Option<String> {
        self.expr.0.label.clone()
    }

    // show normalize when seen, then at the end if something is still variable then normalize
    pub fn get_simplified_description_string(&self) -> String {
        if self.expr.0.is_seq() {
            return String::from("");
        }

        let mut size: Option<IntervalShape> = Some(self.expr.0.clone().size);

        for (n, change_as) in self
            .stack
            .iter()
            .map(|S(f, _)| f.clone().get_change_in_len())
        {
            size = match change_as {
                ChangeAs::TO => Some(size.unwrap().update_size_to(n)),
                ChangeAs::ADD => Some(size.unwrap().update_size_add(n)),
                ChangeAs::SUB => Some(size.unwrap().update_size_sub(n)),
                ChangeAs::REMOVE => None,
            }
        }

        match size {
            Some(s) => self
                .expr
                .0
                .get_simplified_description_string(s.get_normalized()),
            None => String::from(""),
        }
    }
}

impl IntervalShape {
    pub fn update_size_to(&self, n: usize) -> Self {
        IntervalShape::FixedLen(S(n, (0..1).into()))
    }

    pub fn update_size_add(self, n: usize) -> Self {
        match self {
            IntervalShape::FixedLen(S(l, s)) => IntervalShape::FixedLen(S(n + l, s)),
            IntervalShape::RangedLen(S((a, b), s)) => {
                IntervalShape::RangedLen(S((a + n, b + n), s))
            }
            _ => self,
        }
    }

    pub fn update_size_sub(self, n: usize) -> Self {
        match self {
            IntervalShape::FixedLen(S(l, s)) => IntervalShape::FixedLen(S(l - n, s)),
            _ => self,
        }
    }

    pub fn get_normalized(self) -> Self {
        match self {
            IntervalShape::RangedLen(S((a, b), s)) => {
                IntervalShape::FixedLen(S(b + log4_roundup(b - a + 1), s))
            }
            _ => self,
        }
    }
}

impl GeometryPiece {
    pub fn is_complex(&self) -> bool {
        matches!(
            self.size,
            IntervalShape::RangedLen(..) | IntervalShape::FixedSeq(..)
        )
    }

    pub fn is_seq(&self) -> bool {
        matches!(self.size, IntervalShape::FixedSeq(..))
    }

    pub fn get_simplified_description_string(&self, size: IntervalShape) -> String {
        let type_ = match self.type_ {
            IntervalKind::Barcode => "b",
            IntervalKind::SampleBarcode => "s",
            IntervalKind::Umi => "u",
            IntervalKind::Discard => "x",
            IntervalKind::ReadSeq => "r",
            _ => unreachable!(),
        };

        match size {
            IntervalShape::FixedLen(S(n, ..)) => format!("{type_}[{n}]"),
            IntervalShape::UnboundedLen => format!("{type_}:"),
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parser::IntervalKind, Nucleotide, S};

    fn span() -> crate::Span {
        (0..1).into()
    }

    #[test]
    fn test_return_type_display() {
        assert_eq!(format!("{}", ReturnType::Ranged), "Ranged");
        assert_eq!(format!("{}", ReturnType::FixedLen), "Fixed Length");
        assert_eq!(format!("{}", ReturnType::Unbounded), "Unbounded");
        assert_eq!(format!("{}", ReturnType::FixedSeq), "Fixed Sequence");
        assert_eq!(format!("{}", ReturnType::Void), "Void");
    }

    #[test]
    fn test_error_display() {
        let e = Error {
            span: (5..10).into(),
            msg: "test error".to_string(),
        };
        let s = format!("{}", e);
        assert!(s.contains("test error"));
        assert!(s.contains("5"));
        assert!(s.contains("10"));
    }

    #[test]
    fn test_interval_display() {
        let named = Interval::Named("foo".to_string());
        assert_eq!(format!("{}", named), "Label: foo");
    }

    #[test]
    fn test_geometry_piece_is_complex() {
        let fixed = GeometryPiece {
            type_: IntervalKind::Barcode,
            size: IntervalShape::FixedLen(S(16, span())),
            label: None,
        };
        assert!(!fixed.is_complex());

        let ranged = GeometryPiece {
            type_: IntervalKind::Barcode,
            size: IntervalShape::RangedLen(S((8, 12), span())),
            label: None,
        };
        assert!(ranged.is_complex());

        let seq = GeometryPiece {
            type_: IntervalKind::FixedSeq,
            size: IntervalShape::FixedSeq(S(vec![Nucleotide::A], span())),
            label: None,
        };
        assert!(seq.is_complex());
    }

    #[test]
    fn test_geometry_piece_is_seq() {
        let fixed = GeometryPiece {
            type_: IntervalKind::Barcode,
            size: IntervalShape::FixedLen(S(16, span())),
            label: None,
        };
        assert!(!fixed.is_seq());

        let seq = GeometryPiece {
            type_: IntervalKind::FixedSeq,
            size: IntervalShape::FixedSeq(S(vec![Nucleotide::A], span())),
            label: None,
        };
        assert!(seq.is_seq());
    }

    #[test]
    fn test_geometry_piece_simplified_description() {
        let bc = GeometryPiece {
            type_: IntervalKind::Barcode,
            size: IntervalShape::FixedLen(S(16, span())),
            label: None,
        };
        assert_eq!(
            bc.get_simplified_description_string(IntervalShape::FixedLen(S(16, span()))),
            "b[16]"
        );

        let umi = GeometryPiece {
            type_: IntervalKind::Umi,
            size: IntervalShape::FixedLen(S(10, span())),
            label: None,
        };
        assert_eq!(
            umi.get_simplified_description_string(IntervalShape::FixedLen(S(10, span()))),
            "u[10]"
        );

        let read = GeometryPiece {
            type_: IntervalKind::ReadSeq,
            size: IntervalShape::UnboundedLen,
            label: None,
        };
        assert_eq!(
            read.get_simplified_description_string(IntervalShape::UnboundedLen),
            "r:"
        );

        let discard = GeometryPiece {
            type_: IntervalKind::Discard,
            size: IntervalShape::FixedLen(S(5, span())),
            label: None,
        };
        assert_eq!(
            discard.get_simplified_description_string(IntervalShape::FixedLen(S(5, span()))),
            "x[5]"
        );
    }

    #[test]
    fn test_interval_shape_update_size_to() {
        let shape = IntervalShape::FixedLen(S(16, span()));
        let updated = shape.update_size_to(10);
        assert!(matches!(updated, IntervalShape::FixedLen(S(10, _))));
    }

    #[test]
    fn test_interval_shape_update_size_add() {
        let shape = IntervalShape::FixedLen(S(16, span()));
        let updated = shape.update_size_add(4);
        assert!(matches!(updated, IntervalShape::FixedLen(S(20, _))));

        let ranged = IntervalShape::RangedLen(S((8, 12), span()));
        let updated = ranged.update_size_add(4);
        assert!(matches!(updated, IntervalShape::RangedLen(S((12, 16), _))));

        let unbounded = IntervalShape::UnboundedLen;
        let updated = unbounded.update_size_add(4);
        assert!(matches!(updated, IntervalShape::UnboundedLen));
    }

    #[test]
    fn test_interval_shape_update_size_sub() {
        let shape = IntervalShape::FixedLen(S(16, span()));
        let updated = shape.update_size_sub(4);
        assert!(matches!(updated, IntervalShape::FixedLen(S(12, _))));

        let unbounded = IntervalShape::UnboundedLen;
        let updated = unbounded.update_size_sub(4);
        assert!(matches!(updated, IntervalShape::UnboundedLen));
    }

    #[test]
    fn test_interval_shape_get_normalized() {
        let fixed = IntervalShape::FixedLen(S(16, span()));
        let normalized = fixed.get_normalized();
        assert!(matches!(normalized, IntervalShape::FixedLen(S(16, _))));

        let ranged = IntervalShape::RangedLen(S((8, 12), span()));
        let normalized = ranged.get_normalized();
        // Should normalize: b + log4_roundup(b - a + 1) = 12 + log4_roundup(5) = 12 + 2 = 14
        assert!(matches!(normalized, IntervalShape::FixedLen(_)));
    }

    #[test]
    fn test_geometry_meta_display() {
        let gm = GeometryMeta {
            expr: S(
                GeometryPiece {
                    type_: IntervalKind::Barcode,
                    size: IntervalShape::FixedLen(S(16, span())),
                    label: Some("bc1".to_string()),
                },
                span(),
            ),
            stack: vec![],
        };
        let s = format!("{}", gm);
        assert!(s.contains("Geometry Meta"));
    }

    #[test]
    fn test_geometry_piece_display() {
        let gp = GeometryPiece {
            type_: IntervalKind::Barcode,
            size: IntervalShape::FixedLen(S(16, span())),
            label: Some("bc1".to_string()),
        };
        let s = format!("{}", gp);
        assert!(s.contains("bc1"));
        assert!(s.contains("b"));
    }

    #[test]
    fn test_geometry_meta_get_label() {
        let gm = GeometryMeta {
            expr: S(
                GeometryPiece {
                    type_: IntervalKind::Barcode,
                    size: IntervalShape::FixedLen(S(16, span())),
                    label: Some("bc1".to_string()),
                },
                span(),
            ),
            stack: vec![],
        };
        assert_eq!(gm.get_label(), Some("bc1".to_string()));

        let gm_no_label = GeometryMeta {
            expr: S(
                GeometryPiece {
                    type_: IntervalKind::Barcode,
                    size: IntervalShape::FixedLen(S(16, span())),
                    label: None,
                },
                span(),
            ),
            stack: vec![],
        };
        assert_eq!(gm_no_label.get_label(), None);
    }

    #[test]
    fn test_geometry_meta_validate_expr() {
        let gm = GeometryMeta {
            expr: S(
                GeometryPiece {
                    type_: IntervalKind::Barcode,
                    size: IntervalShape::FixedLen(S(16, span())),
                    label: None,
                },
                span(),
            ),
            stack: vec![],
        };
        assert!(gm.validate_expr().is_ok());
    }

    #[test]
    fn test_validate_composition_reverse_void() {
        let result = validate_composition(
            S(&CompiledFunction::Reverse, span()),
            S(ReturnType::Void, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_composition_reverse_ok() {
        let result = validate_composition(
            S(&CompiledFunction::Reverse, span()),
            S(ReturnType::FixedLen, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_composition_revcomp_void() {
        let result = validate_composition(
            S(&CompiledFunction::ReverseComp, span()),
            S(ReturnType::Void, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_composition_truncate_void() {
        let result = validate_composition(
            S(&CompiledFunction::Truncate(2), span()),
            S(ReturnType::Void, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_composition_truncate_too_much() {
        let result = validate_composition(
            S(&CompiledFunction::Truncate(20), span()),
            S(ReturnType::FixedLen, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_composition_truncate_to_ok() {
        let result = validate_composition(
            S(&CompiledFunction::TruncateTo(10), span()),
            S(ReturnType::FixedLen, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_composition_truncate_to_too_much() {
        let result = validate_composition(
            S(&CompiledFunction::TruncateTo(20), span()),
            S(ReturnType::FixedLen, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_composition_remove_void() {
        let result = validate_composition(
            S(&CompiledFunction::Remove, span()),
            S(ReturnType::Void, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_composition_remove_ok() {
        let result = validate_composition(
            S(&CompiledFunction::Remove, span()),
            S(ReturnType::FixedLen, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_ok());
        let ret = result.unwrap();
        assert_eq!(ret.0, ReturnType::Void);
    }

    #[test]
    fn test_validate_composition_pad_void() {
        let result = validate_composition(
            S(&CompiledFunction::Pad(4, Nucleotide::A), span()),
            S(ReturnType::Void, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_composition_pad_to_too_small() {
        let result = validate_composition(
            S(&CompiledFunction::PadTo(10, Nucleotide::A), span()),
            S(ReturnType::FixedLen, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_composition_normalize_ranged() {
        let result = validate_composition(
            S(&CompiledFunction::Normalize, span()),
            S(ReturnType::Ranged, span()),
            &IntervalShape::RangedLen(S((8, 12), span())),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_composition_normalize_fixed_err() {
        let result = validate_composition(
            S(&CompiledFunction::Normalize, span()),
            S(ReturnType::FixedLen, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_composition_hamming_fixedseq() {
        let result = validate_composition(
            S(&CompiledFunction::Hamming(1), span()),
            S(ReturnType::FixedSeq, span()),
            &IntervalShape::FixedSeq(S(vec![Nucleotide::A, Nucleotide::C], span())),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_composition_hamming_non_seq() {
        let result = validate_composition(
            S(&CompiledFunction::Hamming(1), span()),
            S(ReturnType::FixedLen, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_composition_edit_fixedseq() {
        let result = validate_composition(
            S(&CompiledFunction::Edit(1), span()),
            S(ReturnType::FixedSeq, span()),
            &IntervalShape::FixedSeq(S(vec![Nucleotide::A, Nucleotide::C], span())),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_composition_edit_non_seq() {
        let result = validate_composition(
            S(&CompiledFunction::Edit(1), span()),
            S(ReturnType::FixedLen, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_composition_anchor_fixedseq() {
        let result = validate_composition(
            S(&CompiledFunction::Anchor, span()),
            S(ReturnType::FixedSeq, span()),
            &IntervalShape::FixedSeq(S(vec![Nucleotide::A], span())),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_composition_anchor_non_seq() {
        let result = validate_composition(
            S(&CompiledFunction::Anchor, span()),
            S(ReturnType::FixedLen, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_composition_map_ok() {
        let result = validate_composition(
            S(&CompiledFunction::Map("test".into(), vec![]), span()),
            S(ReturnType::FixedLen, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_composition_map_unbounded() {
        let result = validate_composition(
            S(&CompiledFunction::Map("test".into(), vec![]), span()),
            S(ReturnType::Unbounded, span()),
            &IntervalShape::UnboundedLen,
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_composition_filter_within_dist() {
        let result = validate_composition(
            S(
                &CompiledFunction::FilterWithinDist("test".into(), 1),
                span(),
            ),
            S(ReturnType::FixedLen, span()),
            &IntervalShape::FixedLen(S(16, span())),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_compiled_function_get_change_in_len() {
        let (n, _) = CompiledFunction::Truncate(2).get_change_in_len();
        assert_eq!(n, 2);
        let (n, _) = CompiledFunction::TruncateLeft(3).get_change_in_len();
        assert_eq!(n, 3);
        let (n, _) = CompiledFunction::PadTo(20, Nucleotide::A).get_change_in_len();
        assert_eq!(n, 20);
        let (n, _) = CompiledFunction::TruncateTo(10).get_change_in_len();
        assert_eq!(n, 10);
        let (n, _) = CompiledFunction::Pad(4, Nucleotide::A).get_change_in_len();
        assert_eq!(n, 4);
        let (n, _) = CompiledFunction::PadLeft(4, Nucleotide::T).get_change_in_len();
        assert_eq!(n, 4);
        let (n, _) = CompiledFunction::Remove.get_change_in_len();
        assert_eq!(n, 0);
        let (n, _) = CompiledFunction::Reverse.get_change_in_len();
        assert_eq!(n, 0);
    }

    #[test]
    fn test_geometry_meta_get_simplified_with_stack() {
        let gm = GeometryMeta {
            expr: S(
                GeometryPiece {
                    type_: IntervalKind::Barcode,
                    size: IntervalShape::FixedLen(S(16, span())),
                    label: None,
                },
                span(),
            ),
            stack: vec![S(CompiledFunction::Truncate(2), span())],
        };
        let desc = gm.get_simplified_description_string();
        assert_eq!(desc, "b[14]");
    }

    #[test]
    fn test_geometry_meta_get_simplified_seq_returns_empty() {
        let gm = GeometryMeta {
            expr: S(
                GeometryPiece {
                    type_: IntervalKind::FixedSeq,
                    size: IntervalShape::FixedSeq(S(vec![Nucleotide::A, Nucleotide::C], span())),
                    label: None,
                },
                span(),
            ),
            stack: vec![],
        };
        assert_eq!(gm.get_simplified_description_string(), "");
    }

    #[test]
    fn test_geometry_meta_get_simplified_remove() {
        let gm = GeometryMeta {
            expr: S(
                GeometryPiece {
                    type_: IntervalKind::Barcode,
                    size: IntervalShape::FixedLen(S(16, span())),
                    label: None,
                },
                span(),
            ),
            stack: vec![S(CompiledFunction::Remove, span())],
        };
        assert_eq!(gm.get_simplified_description_string(), "");
    }

    // ---------------------------------------------------------------
    // SampleBarcode (`s`) tests
    // ---------------------------------------------------------------

    #[test]
    fn test_geometry_piece_simplified_description_sample_barcode() {
        let sb = GeometryPiece {
            type_: IntervalKind::SampleBarcode,
            size: IntervalShape::FixedLen(S(8, span())),
            label: None,
        };
        assert_eq!(
            sb.get_simplified_description_string(IntervalShape::FixedLen(S(8, span()))),
            "s[8]"
        );
    }

    #[test]
    fn test_geometry_meta_get_simplified_sample_barcode_with_trunc() {
        // Mirror of `test_geometry_meta_get_simplified_with_stack`: starting
        // from s[8] and truncating by 2 should yield s[6].
        let gm = GeometryMeta {
            expr: S(
                GeometryPiece {
                    type_: IntervalKind::SampleBarcode,
                    size: IntervalShape::FixedLen(S(8, span())),
                    label: None,
                },
                span(),
            ),
            stack: vec![S(CompiledFunction::Truncate(2), span())],
        };
        let desc = gm.get_simplified_description_string();
        assert_eq!(desc, "s[6]");
    }

    #[test]
    fn test_validate_expr_sample_barcode_is_not_void() {
        // SampleBarcode is a "real" extraction kind (like Barcode/Umi), NOT
        // Void-like the way Discard is. validate_expr should succeed cleanly
        // for a bare s[8] with no transformations.
        let gm = GeometryMeta {
            expr: S(
                GeometryPiece {
                    type_: IntervalKind::SampleBarcode,
                    size: IntervalShape::FixedLen(S(8, span())),
                    label: None,
                },
                span(),
            ),
            stack: vec![],
        };
        assert!(gm.validate_expr().is_ok());

        // And composing a Reverse on top should still be fine -- Reverse
        // rejects Void but should accept FixedLen. This confirms that
        // SampleBarcode is treated as a real (non-void) length-bearing kind.
        let gm_rev = GeometryMeta {
            expr: S(
                GeometryPiece {
                    type_: IntervalKind::SampleBarcode,
                    size: IntervalShape::FixedLen(S(8, span())),
                    label: None,
                },
                span(),
            ),
            stack: vec![S(CompiledFunction::Reverse, span())],
        };
        assert!(gm_rev.validate_expr().is_ok());
    }
}
