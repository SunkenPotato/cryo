use std::sync::{LazyLock, RwLock, RwLockReadGuard, RwLockWriteGuard};

use thiserror::Error;

static SOURCE_MAP: LazyLock<RwLock<SourceMap>> = LazyLock::new(Default::default);

#[derive(Default)]
pub struct SourceMap {
    pub(crate) files: Vec<SourceFile>,
}

impl SourceMap {
    pub fn instance() -> RwLockReadGuard<'static, SourceMap> {
        SOURCE_MAP.read().unwrap()
    }

    pub fn instance_mut() -> RwLockWriteGuard<'static, SourceMap> {
        SOURCE_MAP.write().unwrap()
    }

    #[must_use]
    pub fn get(&self, idx: usize) -> Option<&SourceFile> {
        self.files.get(idx)
    }

    pub fn push(file: SourceFile) {
        Self::instance_mut().files.push(file);
    }
}

#[derive(Debug)]
pub struct SourceFile {
    pub(crate) file: String,
    pub(crate) contents: String,
    pub(crate) line_starts: Vec<usize>,
}

impl SourceFile {
    #[must_use]
    pub fn contents(&self) -> &str {
        &self.contents
    }
}

#[derive(Error, Debug)]
pub enum SourceFileError {
    #[error("{0}")]
    IoError(#[from] std::io::Error),
    #[error("expected ASCII")]
    InvalidContents,
}

impl SourceFile {
    pub fn new(file: String) -> Result<Self, SourceFileError> {
        let contents = std::fs::read(&file)?;
        let contents = String::from_utf8(contents).map_err(|_| SourceFileError::InvalidContents)?;

        Self::from_string(contents)
    }

    pub fn from_string(string: String) -> Result<Self, SourceFileError> {
        let mut line_starts = vec![0];
        for (idx, c) in string.char_indices() {
            if !c.is_ascii() {
                return Err(SourceFileError::InvalidContents);
            }

            if c == '\n' {
                line_starts.push(idx);
            }
        }

        Ok(Self {
            file: "<unknown>".into(),
            contents: string,
            line_starts,
        })
    }

    #[must_use]
    pub fn line_col(&self, offset: usize) -> Option<(usize, usize)> {
        let line = match self.line_starts.binary_search(&offset) {
            Ok(l) => l,
            Err(l) => l.saturating_sub(1),
        };

        let line_start = self.line_starts.get(line)?;
        Some((line + 1, offset - *line_start + 1))
    }
}
