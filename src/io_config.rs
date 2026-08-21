//! Typed, bounded input topology used by [`crate::execute::RunConfig`].

use std::path::PathBuf;

/// One FASTQ source. Stream sources are added in the stdin/stdout milestone.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InputSource {
    Path(PathBuf),
}

impl InputSource {
    pub fn path(&self) -> &std::path::Path {
        match self {
            Self::Path(path) => path,
        }
    }
}

impl<P: Into<PathBuf>> From<P> for InputSource {
    fn from(path: P) -> Self {
        Self::Path(path.into())
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
