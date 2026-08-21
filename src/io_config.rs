//! Typed, bounded input topology used by [`crate::execute::RunConfig`].

use std::path::{Path, PathBuf};

/// One FASTQ source.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InputSource {
    Path(PathBuf),
    Stdin,
}

impl InputSource {
    pub fn path(&self) -> Option<&Path> {
        match self {
            Self::Path(path) => Some(path),
            Self::Stdin => None,
        }
    }

    /// Interpret the conventional CLI spelling `-` as stdin.
    pub fn from_cli_path(path: impl Into<PathBuf>) -> Self {
        let path = path.into();
        if path == Path::new("-") {
            Self::Stdin
        } else {
            Self::Path(path)
        }
    }

    pub fn kind(&self) -> &'static str {
        match self {
            Self::Path(_) => "path",
            Self::Stdin => "stdin",
        }
    }
}

impl<P: Into<PathBuf>> From<P> for InputSource {
    fn from(path: P) -> Self {
        Self::Path(path.into())
    }
}

/// One FASTQ output destination.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OutputTarget {
    Path(PathBuf),
    Stdout,
    Discard,
}

impl OutputTarget {
    /// Interpret the conventional CLI spelling `-` as stdout.
    pub fn from_cli_path(path: impl Into<PathBuf>) -> Self {
        let path = path.into();
        if path == Path::new("-") {
            Self::Stdout
        } else {
            Self::Path(path)
        }
    }

    pub fn kind(&self) -> &'static str {
        match self {
            Self::Path(_) => "path",
            Self::Stdout => "stdout",
            Self::Discard => "discard",
        }
    }
}

/// One biological read lane, split across ordered file shards.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InputLane {
    pub shards: Vec<InputSource>,
}

impl InputLane {
    pub fn new(shards: impl IntoIterator<Item = impl Into<InputSource>>) -> Self {
        Self {
            shards: shards.into_iter().map(Into::into).collect(),
        }
    }

    pub fn single(source: impl Into<InputSource>) -> Self {
        Self {
            shards: vec![source.into()],
        }
    }
}
