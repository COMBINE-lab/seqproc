pub mod compile;
pub mod interpret;
pub mod lexer;
pub mod parser;

use std::hash::{Hash, Hasher};

use std::{
    fmt::{self, Write},
    ops::Range,
    slice,
};

use chumsky::span::SimpleSpan;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)] // Necessary for by-ref conversion to `str`
pub enum Nucleotide {
    A = b'A',
    C = b'C',
    G = b'G',
    T = b'T',
    U = b'U',
}

impl fmt::Display for Nucleotide {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char(*self as u8 as char)
    }
}

impl Nucleotide {
    /// View this sequence of nucleotides as an ACGTU string.
    #[must_use]
    #[inline]
    pub fn as_str(nuc: &[Nucleotide]) -> &str {
        // SAFETY: `Nucleotide` and `u8` are layout-compatible,
        // and `Nucleotide` byte values are all valid ASCII.
        unsafe {
            std::str::from_utf8_unchecked(slice::from_raw_parts(
                nuc.as_ptr().cast::<u8>(),
                nuc.len(),
            ))
        }
    }

    pub fn as_string(nuc: Nucleotide) -> String {
        match nuc {
            Nucleotide::A => "A",
            Nucleotide::C => "C",
            Nucleotide::G => "G",
            Nucleotide::T => "T",
            Nucleotide::U => "U",
        }
        .to_owned()
    }
}

/// A range of characters in the input file.
pub type Span = SimpleSpan;

/// Associates a `T` with a corresponding span in the source file.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct S<T>(pub T, pub Span);

impl<T> S<T> {
    pub fn new(t: T, s: Range<usize>) -> Self {
        S(t, SimpleSpan::from(s))
    }
}

impl<T> Hash for S<T>
where
    T: PartialEq + Eq + Hash,
{
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.0.hash(hasher);
    }
}

impl<T> S<T> {
    pub fn boxed(self) -> S<Box<T>> {
        S(Box::new(self.0), self.1)
    }
}

impl<T> S<Box<T>> {
    pub fn unboxed(self) -> S<T> {
        S(*self.0, self.1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::hash_map::DefaultHasher;

    #[test]
    fn test_nucleotide_display() {
        assert_eq!(format!("{}", Nucleotide::A), "A");
        assert_eq!(format!("{}", Nucleotide::C), "C");
        assert_eq!(format!("{}", Nucleotide::G), "G");
        assert_eq!(format!("{}", Nucleotide::T), "T");
        assert_eq!(format!("{}", Nucleotide::U), "U");
    }

    #[test]
    fn test_nucleotide_as_str() {
        let nucs = vec![Nucleotide::A, Nucleotide::C, Nucleotide::G, Nucleotide::T];
        assert_eq!(Nucleotide::as_str(&nucs), "ACGT");
        assert_eq!(Nucleotide::as_str(&[]), "");
    }

    #[test]
    fn test_nucleotide_as_string() {
        assert_eq!(Nucleotide::as_string(Nucleotide::A), "A");
        assert_eq!(Nucleotide::as_string(Nucleotide::C), "C");
        assert_eq!(Nucleotide::as_string(Nucleotide::G), "G");
        assert_eq!(Nucleotide::as_string(Nucleotide::T), "T");
        assert_eq!(Nucleotide::as_string(Nucleotide::U), "U");
    }

    #[test]
    fn test_s_new() {
        let s = S::new(42, 0..5);
        assert_eq!(s.0, 42);
        assert_eq!(s.1.start, 0);
        assert_eq!(s.1.end, 5);
    }

    #[test]
    fn test_s_hash() {
        let s1 = S::new(42, 0..5);
        let s2 = S::new(42, 10..20);
        let mut h1 = DefaultHasher::new();
        let mut h2 = DefaultHasher::new();
        s1.hash(&mut h1);
        s2.hash(&mut h2);
        // Hash should be the same since it only hashes the value, not the span
        assert_eq!(h1.finish(), h2.finish());
    }

    #[test]
    fn test_s_boxed_unboxed() {
        let s = S::new(42, 0..5);
        let boxed = s.boxed();
        assert_eq!(*boxed.0, 42);
        let unboxed = boxed.unboxed();
        assert_eq!(unboxed.0, 42);
    }
}
