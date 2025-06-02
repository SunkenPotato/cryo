use std::sync::{OnceLock, RwLock, RwLockReadGuard, RwLockWriteGuard};

use thiserror::Error;

use crate::lexer::{INITIAL_FILE, tokens::Token};

static SOURCE_MAP: OnceLock<RwLock<SourceMap>> = OnceLock::new();

#[derive(Clone, Copy, Debug, PartialEq, Eq, Error)]
pub struct Span {
    file: usize,
    start: usize,
    stop: usize,
}

impl Span {
    pub const EMPTY: Self = Self::new_unchecked(INITIAL_FILE, 0, 0);
    pub const ONE: Self = Self::new(INITIAL_FILE, 0, 1);

    #[expect(unused)]
    fn line_col(&self) -> Option<((usize, usize), (usize, usize))> {
        Some((self.start()?, self.stop()?))
    }

    fn start(&self) -> Option<(usize, usize)> {
        let map = SourceMap::instance();
        let file = &map.files.get(self.file)?;

        file.line_col(self.start)
    }

    fn stop(&self) -> Option<(usize, usize)> {
        let map = SourceMap::instance();
        let file = map.files.get(self.file)?;

        file.line_col(self.stop)
    }

    fn slice(&self) -> String {
        let map = SourceMap::instance();
        let Some(file) = map.files.get(self.file) else {
            return String::new();
        };

        file.contents[self.start..self.stop - 1].to_string()
    }

    fn file_name(&self) -> String {
        let map = SourceMap::instance();
        let Some(file) = map.files.get(self.file) else {
            return String::from("<unknown file>");
        };

        file.file.clone()
    }

    #[must_use]
    #[track_caller]
    pub const fn new(file: usize, start: usize, stop: usize) -> Self {
        if start >= stop {
            panic!("invalid indices");
        }

        Self::new_unchecked(file, start, stop)
    }

    const fn new_unchecked(file: usize, start: usize, stop: usize) -> Self {
        Self { file, start, stop }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Some((line_n, col)) = self.start() else {
            write!(f, "<couldn't format span>")?;
            return Ok(());
        };

        writeln!(f, "{}:{}:{}", self.file_name(), line_n, col)?;

        for (idx, line) in self.slice().lines().enumerate() {
            write!(f, "\n{} |\t{}", idx + line_n, line)?;
        }

        Ok(())
    }
}

#[derive(Default)]
pub struct SourceMap {
    files: Vec<SourceFile>,
}

impl SourceMap {
    #[allow(clippy::missing_panics_doc)]
    pub fn instance() -> RwLockReadGuard<'static, SourceMap> {
        SOURCE_MAP.get_or_init(Default::default).read().unwrap()
    }

    fn instance_mut() -> RwLockWriteGuard<'static, SourceMap> {
        SOURCE_MAP.get_or_init(Default::default).write().unwrap()
    }
    #[must_use]
    pub fn get(&self, idx: usize) -> Option<&SourceFile> {
        self.files.get(idx)
    }

    pub fn insert(file: SourceFile) {
        Self::instance_mut().files.push(file);
    }
}

#[derive(Debug)]
pub struct SourceFile {
    file: String,
    contents: String,
    line_starts: Vec<usize>,
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

pub trait LexResultExt {
    #[must_use]
    fn map_span(self, orig_len: usize) -> Self;
}

impl LexResultExt for Result<(Token, &str), Span> {
    fn map_span(self, offset: usize) -> Self {
        self.map(|(mut v, s)| {
            v.span.start += offset;
            v.span.stop += offset;
            (v, s)
        })
        .map_err(|mut v| {
            v.start += offset;
            v.stop += offset;
            v
        })
    }
}

// in:
//
// let x = 5;
// ^^^
//
// Span: (0, 2)
