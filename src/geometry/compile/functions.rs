//! Compile functions read from parser
//! This is specifically for map as this point
//! but in the future when functions can accept
//! expressions with label references a seperate
//! place for this to happen will be useful

use crate::{
    compile::utils::{Error, GeometryMeta, GeometryPiece},
    parser::{Expr, Function, ResourceRef},
    Nucleotide, S,
};
use antisequence::{AmbiguityPolicy, PositionAmbiguityPolicy};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PatternOrientation {
    Forward,
    ReverseComplement,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PatternProjection {
    Prefix { max_len: usize },
    Suffix { max_len: usize },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompiledFunction {
    Reverse,
    ReverseComp,
    Truncate(usize),
    TruncateLeft(usize),
    TruncateTo(usize),
    TruncateToLeft(usize),
    Remove,
    Pad(usize, Nucleotide),
    PadLeft(usize, Nucleotide),
    PadTo(usize, Nucleotide),
    PadToLeft(usize, Nucleotide),
    Normalize,
    Map(ResourceRef, Vec<S<CompiledFunction>>),
    MapWithMismatch(ResourceRef, Vec<S<CompiledFunction>>, usize),
    MapWithEdit(ResourceRef, Vec<S<CompiledFunction>>, usize),
    FilterWithinDist(ResourceRef, usize),
    /// Property-style ambiguity resolution modifier applied to the next
    /// map/filter operation in this definition's stack.
    AmbiguityPolicy(AmbiguityPolicy),
    /// Whitelist-backed patterns for a fixed anchor definition.
    AnchorSet(ResourceRef),
    /// Equal-best placement policy for search-style anchors.
    PositionAmbiguityPolicy(PositionAmbiguityPolicy),
    /// Transform whitelist patterns once while constructing a matcher.
    PatternOrientation(PatternOrientation),
    /// Project a prefix or suffix of every whitelist pattern once while
    /// constructing a matcher.
    PatternProjection(PatternProjection),
    /// Let an exact variable-length pattern determine the interval boundary.
    PatternBoundaryMatched,
    Hamming(usize),
    Edit(usize),
    /// `anchor_relative` - search for anchor from position 0 and extract preceding elements with flexible length
    Anchor,
}

pub enum ChangeAs {
    TO,
    ADD,
    SUB,
    REMOVE,
}

impl CompiledFunction {
    pub fn get_change_in_len(&self) -> (usize, ChangeAs) {
        match self {
            CompiledFunction::Truncate(n) | CompiledFunction::TruncateLeft(n) => {
                (*n, ChangeAs::SUB)
            }
            CompiledFunction::PadTo(n, _)
            | CompiledFunction::PadToLeft(n, _)
            | CompiledFunction::TruncateTo(n)
            | CompiledFunction::TruncateToLeft(n) => (*n, ChangeAs::TO),
            CompiledFunction::Pad(n, _) | CompiledFunction::PadLeft(n, _) => (*n, ChangeAs::ADD),
            CompiledFunction::Remove => (0, ChangeAs::REMOVE),
            _ => (0, ChangeAs::ADD),
        }
    }
}

pub fn compile_fn(
    S(fn_, span): S<Function>,
    S(parent_expr, expr_span): S<Expr>,
) -> Result<S<CompiledFunction>, Error> {
    let comp_fn = match fn_ {
        Function::Reverse => CompiledFunction::Reverse,
        Function::ReverseComp => CompiledFunction::ReverseComp,
        Function::Truncate(n) => CompiledFunction::Truncate(n),
        Function::TruncateLeft(n) => CompiledFunction::TruncateLeft(n),
        Function::TruncateTo(n) => CompiledFunction::TruncateTo(n),
        Function::TruncateToLeft(n) => CompiledFunction::TruncateToLeft(n),
        Function::Remove => CompiledFunction::Remove,
        Function::Pad(n, nuc) => CompiledFunction::Pad(n, nuc),
        Function::PadLeft(n, nuc) => CompiledFunction::PadLeft(n, nuc),
        Function::PadTo(n, nuc) => CompiledFunction::PadTo(n, nuc),
        Function::PadToLeft(n, nuc) => CompiledFunction::PadToLeft(n, nuc),
        Function::Normalize => CompiledFunction::Normalize,
        Function::MapWithMismatch(path, expr, mismatch) => CompiledFunction::MapWithMismatch(
            path,
            compile_inner_expr(expr.unboxed(), S(parent_expr, expr_span))?,
            mismatch,
        ),
        Function::Map(path, expr) => CompiledFunction::Map(
            path,
            compile_inner_expr(expr.unboxed(), S(parent_expr, expr_span))?,
        ),
        Function::Filter(path) => CompiledFunction::FilterWithinDist(path, 0),
        Function::FilterWithinDist(path, mismatch) => {
            CompiledFunction::FilterWithinDist(path, mismatch)
        }
        Function::Hamming(n) => CompiledFunction::Hamming(n),
        Function::Edit(n) => CompiledFunction::Edit(n),
        Function::MapWithEdit(path, expr, edit_dist) => CompiledFunction::MapWithEdit(
            path,
            compile_inner_expr(expr.unboxed(), S(parent_expr, expr_span))?,
            edit_dist,
        ),
        Function::Anchor => CompiledFunction::Anchor,
    };

    Ok(S(comp_fn, span))
}

/// expr: transformed interval with reference to 'self'
/// parent_expr: outside interval which will acted on first
fn compile_inner_expr(
    mut expr: S<Expr>,
    parent_expr: S<Expr>,
) -> Result<Vec<S<CompiledFunction>>, Error> {
    // if we are here in a map then the expr passed into the expr should be a geom_piece or labeled geom_piece
    // either way we can extract the size and type of it
    let mut self_stack: Vec<S<CompiledFunction>> = Vec::new();
    let mut inner_stack: Vec<S<CompiledFunction>> = Vec::new();

    loop {
        match expr.0 {
            Expr::Self_ => break,
            Expr::Function(inner_fn, inner_expr) => {
                expr = inner_expr.unboxed();
                let inner_fn = compile_fn(inner_fn.clone(), expr.clone());
                if inner_fn.is_ok() {
                    self_stack.push(inner_fn.ok().unwrap());
                } else {
                    return Err(Error {
                        span: expr.1,
                        msg: "Invalid function composition".to_string(),
                    });
                }
            }
            _ => {
                return Err(Error {
                    span: expr.1,
                    msg: "Must refernce only `self` in `map` fallback argument".to_string(),
                })
            }
        }
    }

    let geom_piece = {
        let S(mut expr, mut span) = parent_expr;
        loop {
            match expr {
                Expr::Function(fn_, fn_expr) => {
                    expr = fn_expr.unboxed().0;
                    span = fn_.1;
                    let compiled_fn = compile_fn(fn_.clone(), S(expr.clone(), span));
                    if compiled_fn.is_ok() {
                        inner_stack.push(compiled_fn.ok().unwrap());
                    } else {
                        return Err(Error {
                            span,
                            msg: "Invalid function composition".to_string(),
                        });
                    }
                }
                Expr::LabeledGeomPiece(_, b) => {
                    let S(gp, _) = b.unboxed();
                    expr = gp.clone();
                }
                Expr::GeomPiece(_, _) => break,
                _ => return Err(Error {
                    span,
                    msg: "Something internal went wrong -- please post EFGDL specification on Github issues".to_string(),
                })
            };
        }

        if let Expr::GeomPiece(type_, size) = expr {
            GeometryPiece {
                type_,
                size,
                label: None,
            }
        } else {
            return Err(Error {
                span,
                msg: format!("Expected geometry peice found: {expr}"),
            });
        }
    };

    GeometryMeta {
        expr: S(geom_piece, expr.1),
        stack: self_stack
            .clone()
            .into_iter()
            .chain(inner_stack)
            .collect::<Vec<_>>(),
    }
    .validate_expr()?;

    Ok(self_stack)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parser::IntervalKind, parser::IntervalShape, Nucleotide};

    fn span() -> crate::Span {
        (0..1).into()
    }

    fn make_barcode_expr() -> S<Expr> {
        S(
            Expr::GeomPiece(
                IntervalKind::Barcode,
                IntervalShape::FixedLen(S(16, span())),
            ),
            span(),
        )
    }

    #[test]
    fn test_compile_fn_reverse() {
        let result = compile_fn(S(Function::Reverse, span()), make_barcode_expr());
        assert!(result.is_ok());
        assert!(matches!(result.unwrap().0, CompiledFunction::Reverse));
    }

    #[test]
    fn test_compile_fn_reverse_comp() {
        let result = compile_fn(S(Function::ReverseComp, span()), make_barcode_expr());
        assert!(result.is_ok());
        assert!(matches!(result.unwrap().0, CompiledFunction::ReverseComp));
    }

    #[test]
    fn test_compile_fn_truncate() {
        let result = compile_fn(S(Function::Truncate(2), span()), make_barcode_expr());
        assert!(result.is_ok());
        assert!(matches!(result.unwrap().0, CompiledFunction::Truncate(2)));
    }

    #[test]
    fn test_compile_fn_truncate_left() {
        let result = compile_fn(S(Function::TruncateLeft(3), span()), make_barcode_expr());
        assert!(result.is_ok());
        assert!(matches!(
            result.unwrap().0,
            CompiledFunction::TruncateLeft(3)
        ));
    }

    #[test]
    fn test_compile_fn_truncate_to() {
        let result = compile_fn(S(Function::TruncateTo(10), span()), make_barcode_expr());
        assert!(result.is_ok());
        assert!(matches!(
            result.unwrap().0,
            CompiledFunction::TruncateTo(10)
        ));
    }

    #[test]
    fn test_compile_fn_truncate_to_left() {
        let result = compile_fn(S(Function::TruncateToLeft(10), span()), make_barcode_expr());
        assert!(result.is_ok());
        assert!(matches!(
            result.unwrap().0,
            CompiledFunction::TruncateToLeft(10)
        ));
    }

    #[test]
    fn test_compile_fn_remove() {
        let result = compile_fn(S(Function::Remove, span()), make_barcode_expr());
        assert!(result.is_ok());
        assert!(matches!(result.unwrap().0, CompiledFunction::Remove));
    }

    #[test]
    fn test_compile_fn_pad() {
        let result = compile_fn(
            S(Function::Pad(4, Nucleotide::A), span()),
            make_barcode_expr(),
        );
        assert!(result.is_ok());
        assert!(matches!(
            result.unwrap().0,
            CompiledFunction::Pad(4, Nucleotide::A)
        ));
    }

    #[test]
    fn test_compile_fn_pad_left() {
        let result = compile_fn(
            S(Function::PadLeft(4, Nucleotide::T), span()),
            make_barcode_expr(),
        );
        assert!(result.is_ok());
        assert!(matches!(
            result.unwrap().0,
            CompiledFunction::PadLeft(4, Nucleotide::T)
        ));
    }

    #[test]
    fn test_compile_fn_pad_to() {
        let result = compile_fn(
            S(Function::PadTo(20, Nucleotide::G), span()),
            make_barcode_expr(),
        );
        assert!(result.is_ok());
        assert!(matches!(
            result.unwrap().0,
            CompiledFunction::PadTo(20, Nucleotide::G)
        ));
    }

    #[test]
    fn test_compile_fn_pad_to_left() {
        let result = compile_fn(
            S(Function::PadToLeft(20, Nucleotide::C), span()),
            make_barcode_expr(),
        );
        assert!(result.is_ok());
        assert!(matches!(
            result.unwrap().0,
            CompiledFunction::PadToLeft(20, Nucleotide::C)
        ));
    }

    #[test]
    fn test_compile_fn_normalize() {
        let result = compile_fn(S(Function::Normalize, span()), make_barcode_expr());
        assert!(result.is_ok());
        assert!(matches!(result.unwrap().0, CompiledFunction::Normalize));
    }

    #[test]
    fn test_compile_fn_hamming() {
        let result = compile_fn(S(Function::Hamming(1), span()), make_barcode_expr());
        assert!(result.is_ok());
        assert!(matches!(result.unwrap().0, CompiledFunction::Hamming(1)));
    }

    #[test]
    fn test_compile_fn_edit() {
        let result = compile_fn(S(Function::Edit(2), span()), make_barcode_expr());
        assert!(result.is_ok());
        assert!(matches!(result.unwrap().0, CompiledFunction::Edit(2)));
    }

    #[test]
    fn test_compile_fn_filter() {
        let result = compile_fn(
            S(Function::Filter("test".into()), span()),
            make_barcode_expr(),
        );
        assert!(result.is_ok());
        assert!(matches!(
            result.unwrap().0,
            CompiledFunction::FilterWithinDist(_, 0)
        ));
    }

    #[test]
    fn test_compile_fn_filter_within_dist() {
        let result = compile_fn(
            S(Function::FilterWithinDist("test".into(), 2), span()),
            make_barcode_expr(),
        );
        assert!(result.is_ok());
        assert!(matches!(
            result.unwrap().0,
            CompiledFunction::FilterWithinDist(_, 2)
        ));
    }

    #[test]
    fn test_compile_fn_anchor() {
        let result = compile_fn(S(Function::Anchor, span()), make_barcode_expr());
        assert!(result.is_ok());
        assert!(matches!(result.unwrap().0, CompiledFunction::Anchor));
    }

    #[test]
    fn test_compile_fn_map() {
        let self_expr = S(Box::new(Expr::Self_), span());
        let result = compile_fn(
            S(Function::Map("file.tsv".into(), self_expr), span()),
            make_barcode_expr(),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_fn_map_with_mismatch() {
        let self_expr = S(Box::new(Expr::Self_), span());
        let result = compile_fn(
            S(
                Function::MapWithMismatch("file.tsv".into(), self_expr, 1),
                span(),
            ),
            make_barcode_expr(),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_fn_map_with_edit() {
        let self_expr = S(Box::new(Expr::Self_), span());
        let result = compile_fn(
            S(
                Function::MapWithEdit("file.tsv".into(), self_expr, 1),
                span(),
            ),
            make_barcode_expr(),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_inner_expr_simple_self() {
        let self_expr = S(Expr::Self_, span());
        let parent = make_barcode_expr();
        let result = compile_inner_expr(self_expr, parent);
        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());
    }

    #[test]
    fn test_compile_inner_expr_with_function() {
        let self_expr = S(
            Expr::Function(
                S(Function::Truncate(2), span()),
                S(Box::new(Expr::Self_), span()),
            ),
            span(),
        );
        let parent = make_barcode_expr();
        let result = compile_inner_expr(self_expr, parent);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 1);
    }

    #[test]
    fn test_compile_inner_expr_invalid_no_self() {
        let bad_expr = S(Expr::Label(S("foo".into(), span())), span());
        let parent = make_barcode_expr();
        let result = compile_inner_expr(bad_expr, parent);
        assert!(result.is_err());
    }
}
