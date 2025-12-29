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
