//! Bounded normalization for EFGDL 2 input-layout algebra.

use crate::{parser::Expr, S};

use super::utils::Error;

/// Maximum number of normalized alternatives for one input read.
pub const MAX_LAYOUT_ALTERNATIVES: usize = 64;
/// Maximum fixed repetition accepted by the surface language.
pub const MAX_LAYOUT_REPEAT: usize = 64;
/// Maximum number of segments in one normalized alternative.
pub const MAX_LAYOUT_SEGMENTS: usize = 4096;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NormalizedLayout {
    /// Alternatives in source order. Runtime selection is left-to-right.
    pub alternatives: Vec<Vec<S<Expr>>>,
    /// Whether the source used any EFGDL 2 layout operator.
    pub uses_algebra: bool,
}

fn error(span: crate::Span, msg: impl Into<String>) -> Error {
    Error {
        span,
        msg: msg.into(),
    }
}

fn check_bounds(alternatives: &[Vec<S<Expr>>], span: crate::Span) -> Result<(), Error> {
    if alternatives.len() > MAX_LAYOUT_ALTERNATIVES {
        return Err(error(
            span,
            format!(
                "layout expands to {} alternatives; the limit is {MAX_LAYOUT_ALTERNATIVES}",
                alternatives.len()
            ),
        ));
    }
    if let Some(segments) = alternatives
        .iter()
        .map(Vec::len)
        .find(|segments| *segments > MAX_LAYOUT_SEGMENTS)
    {
        return Err(error(
            span,
            format!(
                "layout alternative expands to {segments} segments; the limit is {MAX_LAYOUT_SEGMENTS}"
            ),
        ));
    }
    Ok(())
}

fn concatenate(
    left: Vec<Vec<S<Expr>>>,
    right: Vec<Vec<S<Expr>>>,
    span: crate::Span,
) -> Result<Vec<Vec<S<Expr>>>, Error> {
    let count = left.len().checked_mul(right.len()).ok_or_else(|| {
        error(
            span,
            "layout alternative count overflowed during normalization",
        )
    })?;
    if count > MAX_LAYOUT_ALTERNATIVES {
        return Err(error(
            span,
            format!(
                "layout expands to at least {count} alternatives; the limit is {MAX_LAYOUT_ALTERNATIVES}"
            ),
        ));
    }

    let mut result = Vec::with_capacity(count);
    for lhs in &left {
        for rhs in &right {
            let len = lhs.len().checked_add(rhs.len()).ok_or_else(|| {
                error(span, "layout segment count overflowed during normalization")
            })?;
            if len > MAX_LAYOUT_SEGMENTS {
                return Err(error(
                    span,
                    format!(
                        "layout alternative expands to {len} segments; the limit is {MAX_LAYOUT_SEGMENTS}"
                    ),
                ));
            }
            let mut alternative = Vec::with_capacity(len);
            alternative.extend(lhs.iter().cloned());
            alternative.extend(rhs.iter().cloned());
            result.push(alternative);
        }
    }
    Ok(result)
}

fn expand(expr: S<Expr>, uses_algebra: &mut bool) -> Result<Vec<Vec<S<Expr>>>, Error> {
    let span = expr.1;
    let alternatives = match expr.0 {
        Expr::LayoutConcat(parts) => {
            *uses_algebra = true;
            let mut result = vec![Vec::new()];
            for part in parts {
                result = concatenate(result, expand(part, uses_algebra)?, span)?;
            }
            result
        }
        Expr::LayoutChoice(arms) => {
            *uses_algebra = true;
            let mut result = Vec::new();
            for arm in arms {
                result.extend(expand(arm, uses_algebra)?);
                check_bounds(&result, span)?;
            }
            result
        }
        Expr::LayoutOptional(inner) => {
            *uses_algebra = true;
            let mut result = expand(inner.unboxed(), uses_algebra)?;
            // Prefer the present form, then fall back to the absent form.
            result.push(Vec::new());
            result
        }
        Expr::LayoutRepeat(inner, S(count, count_span)) => {
            *uses_algebra = true;
            if count > MAX_LAYOUT_REPEAT {
                return Err(error(
                    count_span,
                    format!(
                        "layout repetition count {count} exceeds the limit of {MAX_LAYOUT_REPEAT}"
                    ),
                ));
            }
            let body = expand(inner.unboxed(), uses_algebra)?;
            let mut result = vec![Vec::new()];
            for _ in 0..count {
                result = concatenate(result, body.clone(), span)?;
            }
            result
        }
        expr => vec![vec![S(expr, span)]],
    };
    check_bounds(&alternatives, span)?;
    Ok(alternatives)
}

/// Normalize one parsed read into a bounded list of linear geometries.
pub fn normalize_layout(
    exprs: Vec<S<Expr>>,
    efgdl_version: usize,
) -> Result<NormalizedLayout, Error> {
    let span = exprs.first().map(|expr| expr.1).unwrap_or_default();
    let mut uses_algebra = false;
    let mut alternatives = vec![Vec::new()];
    for expr in exprs {
        alternatives = concatenate(alternatives, expand(expr, &mut uses_algebra)?, span)?;
    }

    if uses_algebra && efgdl_version < 2 {
        return Err(error(
            span,
            "layout algebra requires an EFGDL 2 header: `header { efgdl = 2 }`",
        ));
    }
    if alternatives.iter().any(Vec::is_empty) {
        return Err(error(
            span,
            "an input-read layout alternative cannot be empty",
        ));
    }

    Ok(NormalizedLayout {
        alternatives,
        uses_algebra,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{IntervalKind, IntervalShape};

    fn span() -> crate::Span {
        (0..1).into()
    }

    fn fixed(length: usize) -> S<Expr> {
        S(
            Expr::GeomPiece(
                IntervalKind::Barcode,
                IntervalShape::FixedLen(S(length, span())),
            ),
            span(),
        )
    }

    #[test]
    fn normalizes_choice_concat_optional_and_repeat_in_source_order() {
        let choice = S(Expr::LayoutChoice(vec![fixed(1), fixed(2)]), span());
        let optional = S(
            Expr::LayoutOptional(S(Box::new(fixed(3).0), span())),
            span(),
        );
        let repeat = S(
            Expr::LayoutRepeat(S(Box::new(fixed(4).0), span()), S(2, span())),
            span(),
        );
        let normalized = normalize_layout(vec![choice, optional, repeat], 2).unwrap();
        assert!(normalized.uses_algebra);
        assert_eq!(normalized.alternatives.len(), 4);
        assert_eq!(normalized.alternatives[0].len(), 4);
        assert_eq!(normalized.alternatives[1].len(), 3);
    }

    #[test]
    fn rejects_algebra_without_v2_header() {
        let choice = S(Expr::LayoutChoice(vec![fixed(1), fixed(2)]), span());
        let error = normalize_layout(vec![choice], 1).unwrap_err();
        assert!(error.msg.contains("EFGDL 2"));
    }

    #[test]
    fn rejects_empty_input_alternative() {
        let optional = S(
            Expr::LayoutOptional(S(Box::new(fixed(1).0), span())),
            span(),
        );
        let error = normalize_layout(vec![optional], 2).unwrap_err();
        assert!(error.msg.contains("cannot be empty"));
    }
}
