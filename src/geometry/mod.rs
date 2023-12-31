pub mod compile;
pub mod interpret;
pub mod lexer;
pub mod parser;

use std::{
    fmt::{self, Write},
    ops::Range,
    slice,
};

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
}

/// A range of characters in the input file.
pub type Span = Range<usize>;

/// Associates a `T` with a corresponding span in the source file.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct S<T>(pub T, pub Span);

impl<T> S<T> {
    pub fn as_ref(&self) -> S<&T> {
        S(&self.0, self.1.clone())
    }

    pub fn as_mut(&mut self) -> S<&mut T> {
        S(&mut self.0, self.1.clone())
    }
}
