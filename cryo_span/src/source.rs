use std::{
    borrow::Cow,
    fs::File,
    io::{Read, Seek, SeekFrom},
    path::{Path, PathBuf},
    sync::{LazyLock, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use thiserror::Error;

static SOURCE_MAP: LazyLock<RwLock<SourceMap>> = LazyLock::new(Default::default);

/// A map storing all loaded source files.
///
/// This struct cannot be created and should only be interacted with through the functions [`SourceMap::instance`] and [`SourceMap::instance_mut`].
#[derive(Default)]
pub struct SourceMap {
    pub(crate) files: Vec<SourceFile>,
}

impl SourceMap {
    /// Get a read-only reference to the global [`SourceMap`] instance.
    ///
    /// # Panics
    /// Panics if the [`RwLock`] containing the instance has been poisoned.
    pub fn instance() -> RwLockReadGuard<'static, SourceMap> {
        SOURCE_MAP.read().unwrap()
    }

    /// Get a mutable reference to the global [`SourceMap`] instance.
    ///
    /// # Panics
    /// Panics if the [`RwLock`] containing the instance has been poisoned.
    pub fn instance_mut() -> RwLockWriteGuard<'static, SourceMap> {
        SOURCE_MAP.write().unwrap()
    }

    /// Get a possible reference to the [`SourceFile`] at the provided index.
    #[must_use]
    pub fn get(&self, idx: usize) -> Option<&SourceFile> {
        self.files.get(idx)
    }

    /// Add a [`SourceFile`] to the global instance.
    pub fn push(file: SourceFile) {
        Self::instance_mut().files.push(file);
    }
}

/// Represents an input file, with line beginnings pre-computed.
#[derive(Debug)]
pub struct SourceFile {
    pub(crate) file: SourceFileLocation,
    pub(crate) line_starts: Vec<usize>,
}

impl SourceFile {
    /// Retrieve the contents of this source file.
    pub fn contents<'a>(&'a self) -> Cow<'a, str> {
        match &self.file {
            SourceFileLocation::Fs(v) => {
                Cow::Owned(std::fs::read_to_string(v).expect("file should be readable"))
            }
            SourceFileLocation::Direct(v) => Cow::Borrowed(&v),
        }
    }
}

/// Where the contents of a [`SourceFile`] live.
#[derive(Debug)]
pub enum SourceFileLocation {
    /// The contents are located on the filesystem.
    Fs(PathBuf),
    /// The contents are stored in memory.
    Direct(String),
}

/// The input passed to [`SourceFile::from_string`]. Similar to [`SourceFileLocation`],
/// except that the [`Fs`] variant additionally contains a reference to the contents of the file.
///
/// [`Fs`]: self::SourceFileInput#variant.Fs
#[derive(Debug)]
pub enum SourceFileInput<'c> {
    /// The contents are located on the filesystem.
    Fs(&'c str, PathBuf),
    /// The contents are stored in memory.
    Direct(String),
}

impl SourceFileInput<'_> {
    /// Retrieve the contents of `self`, regardless of the variant.
    pub fn contents(&self) -> &str {
        match &self {
            Self::Fs(s, _) => s,
            Self::Direct(s) => s,
        }
    }
}

impl Into<SourceFileLocation> for SourceFileInput<'_> {
    fn into(self) -> SourceFileLocation {
        match self {
            Self::Fs(_, path) => SourceFileLocation::Fs(path),
            Self::Direct(contents) => SourceFileLocation::Direct(contents),
        }
    }
}

/// The errors that can occur creating a [`SourceFile`].
#[derive(Error, Debug)]
pub enum SourceFileError {
    /// Errors that can occur while interacting with the filesystem.
    ///
    /// The inner type is [`std::io::Error`].
    #[error("{0}")]
    IoError(#[from] std::io::Error),
    /// Occurs if the file/input contains characters that are *not* ASCII.
    #[error("expected ASCII")]
    InvalidContents,
}

impl SourceFile {
    /// Create a new [`SourceFile`] from a given file in the filesystem.
    ///
    /// # Errors
    ///
    /// If the file cannot be read, [`SourceFileError::IoError`] will be returned.
    ///
    /// If the file contains characters other than ASCII, [`SourceFileError::InvalidContents`] will be returned.
    pub fn new(file: impl Into<PathBuf> + AsRef<Path>) -> Result<Self, SourceFileError> {
        let contents = std::fs::read(&file)?;
        let contents = String::from_utf8(contents).map_err(|_| SourceFileError::InvalidContents)?;

        Self::from_string(SourceFileInput::Fs(&contents, file.into()))
    }

    /// Create a new [`SourceFile`] from an input string.
    ///
    /// If `f_name` is None, "<unnamed>" will be used.
    ///
    /// # Errors
    /// If the input contains characters other than ASCII, [`SourceFile::InvalidContents`] will be returned.
    ///
    /// [`SourceFile::InvalidContents`]: self::SourceFile#variant.InvalidContents
    #[allow(rustdoc::invalid_html_tags)]
    pub fn from_string(input: SourceFileInput) -> Result<Self, SourceFileError> {
        let mut line_starts = vec![0];
        for (idx, c) in input.contents().char_indices() {
            if !c.is_ascii() {
                return Err(SourceFileError::InvalidContents);
            }

            if c == '\n' {
                line_starts.push(idx);
            }
        }

        Ok(Self {
            file: input.into(),
            line_starts,
        })
    }

    /// Returns the line and column as `(line, column)`.
    #[must_use]
    pub fn line_col(&self, offset: usize) -> Option<(usize, usize)> {
        let line = match self.line_starts.binary_search(&offset) {
            Ok(l) => l,
            Err(l) => l.saturating_sub(1),
        };

        let line_start = self.line_starts.get(line)?;
        Some((line + 1, offset - *line_start + 1))
    }

    /// Return the slice at the range in the file.
    pub fn slice<'a>(&'a self, start: usize, stop: usize) -> std::io::Result<Cow<'a, str>> {
        let s = match &self.file {
            SourceFileLocation::Fs(path) => {
                let mut file = File::open(&path).unwrap();
                let len = stop - start;

                file.seek(SeekFrom::Start(start.try_into().unwrap()))?;

                let mut buffer = Vec::with_capacity(len);
                // SAFETY: we have set the capacity to `len`.
                unsafe { buffer.set_len(len) };
                file.read_exact(&mut buffer)?;

                // SAFETY: when we created this struct, the contents were checked for valid ASCII
                Cow::Owned(unsafe { String::from_utf8_unchecked(buffer) })
            }
            SourceFileLocation::Direct(direct) => Cow::Borrowed(&direct[start..=stop]),
        };

        Ok(s)
    }
}
