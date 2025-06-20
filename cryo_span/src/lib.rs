//! Line-column references to code files.
//!
//! This crate defines utilities for referencing code segments in files via [`Span`].
#![feature(rustdoc_missing_doc_code_examples)]
#![warn(clippy::all)]
#![warn(clippy::pedantic)]
#![deny(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]

use std::borrow::Cow;

use thiserror::Error;

mod source;

pub use source::*;

use crate::sealed::Sealed;

/// Represents the main file.
///
/// Since multiple files are not supported, this constant should always be used when constructing a [`Span`].
pub const INITIAL_FILE: usize = 0;

/// Represents a file segment.
///
/// A [`Span`] is composed of:
/// - a reference to [`SourceFile`] in the [`SourceMap`]
/// - the beginning of the segment
/// - the end of the segment
///
/// Furthermore, the start and stop indices are evaluated with [`RangeInclusive`](std::range::RangeInclusive). (`..=`)
///
/// [`Span`] implements [`Display`](std::fmt::Display), which allows for displaying the line, column, and actual code.
///
/// ## Examples
/// ```rust
/// use cryo_span::{Span, INITIAL_FILE};
/// let string = "Hello, world"; // assuming this is already loaded as a `SourceFile`.
/// let span = Span::new(INITIAL_FILE, 0, 4);
///
/// println!("{span}");
/// ```
/// Prints:
/// ```text
/// <unnamed>:0:10
/// Hello
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Error)]
pub struct Span {
    file: usize,
    start: usize,
    stop: usize,
}

#[allow(rustdoc::missing_doc_code_examples)]
impl Span {
    /// An empty [`Span`]. Useful when your input is empty or you've reached EOF.
    ///
    /// This constant should always be used instead of constructing a new [`Span`] by hand.
    #[allow(rustdoc::missing_doc_code_examples)]
    pub const EMPTY: Self = Self::new_unchecked(INITIAL_FILE, 0, 0);
    /// Convenience [`Span`] representing exactly **one** character at the start of the input file.
    #[allow(rustdoc::missing_doc_code_examples)]
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

    /// Returns the slice that the current span points to.
    ///
    /// A [`String`] is returned rather than an [`&str`] due to the storage method of the [`SourceMap`] and it's associated lifetimes.
    ///
    /// # Examples
    /// ```no_run
    /// use cryo_span::{INITIAL_FILE, Span};
    ///
    /// let span = Span::new(INITIAL_FILE, 0, 1);
    ///
    /// assert_eq!(span.slice(), "")
    ///
    #[must_use]
    pub fn slice<'a>(&'a self) -> String {
        let map = SourceMap::instance();
        let Some(file) = map.files.get(self.file) else {
            return String::new();
        };

        file.slice(self.start, self.stop).unwrap().into_owned()
    }

    fn file_name(&self) -> Cow<'_, str> {
        let map = SourceMap::instance();
        let Some(file) = map.files.get(self.file) else {
            return Cow::Borrowed("<unknown file>");
        };

        match &file.file {
            SourceFileLocation::Fs(path) => Cow::Owned(path.to_string_lossy().into_owned()),
            SourceFileLocation::Direct(_) => Cow::Borrowed("<unnamed file>"),
        }
    }

    /// Construct a new [`Span`].
    ///
    /// For the non-panicking version, view [`new_safe`](Self::new_safe).
    /// # Examples
    /// ```rust
    /// use cryo_span::{Span, SourceFile, SourceFileInput, SourceMap, INITIAL_FILE};
    ///
    /// let input = "Hello, world";
    /// SourceMap::push(SourceFile::from_string(SourceFileInput::Direct(input.to_owned())).unwrap());
    /// let span = Span::new(INITIAL_FILE, 0, 4); // points to: "Hello"
    ///
    /// assert_eq!(span.slice(), "Hello".to_string());
    /// ```
    ///
    /// # Panics
    ///
    /// This function will panic if `start >= stop`.
    #[must_use]
    #[track_caller]
    pub const fn new(file: usize, start: usize, stop: usize) -> Self {
        assert!(start <= stop, "invalid indices");

        Self::new_unchecked(file, start, stop)
    }

    /// Construct a new, unchecked [`Span`]. This provides very little advantage over calling [`Span::new`] or [`Span::new_safe`].
    ///
    /// This function will not check for index validity, and it is up to the caller to validate the indices.
    ///
    /// # Examples
    /// ```
    /// use cryo_span::{INITIAL_FILE, Span};
    ///
    /// Span::new_unchecked(INITIAL_FILE, 1, 0); // this works
    /// Span::new_unchecked(INITIAL_FILE, 0, 10);
    /// ```
    #[must_use]
    pub const fn new_unchecked(file: usize, start: usize, stop: usize) -> Self {
        Self { file, start, stop }
    }

    /// Construct a new [`Span`].
    ///
    /// This function is essentially identical to [`Span::new`], except that it does not panic.
    ///
    /// This function returns `None` if `start >= stop`.
    ///
    /// # Examples
    /// ```
    /// use cryo_span::{INITIAL_FILE, Span};
    ///
    /// assert_eq!(None, Span::new_safe(INITIAL_FILE, 1, 0));
    /// assert!(matches!(Span::new_safe(INITIAL_FILE, 0, 1), Some(_)))
    /// ```
    #[must_use]
    pub const fn new_safe(file: usize, start: usize, stop: usize) -> Option<Self> {
        if start >= stop {
            return None;
        }

        Some(Self::new_unchecked(file, start, stop))
    }

    /// Extend this span with another one.
    ///
    /// Extension is done by setting the current span's end to the other span's end.
    ///
    /// # Panics
    /// This method will panic if the other's span `stop` field is greater than the current one.
    ///
    /// See also:
    /// [`Span::extend_unchecked`]
    pub const fn extend(self, other: Span) -> Self {
        assert!(self.stop <= other.stop);
        self.extend_unchecked(other)
    }

    /// Does the same thing as [`Self::extend`], except it will not check for validity.
    pub const fn extend_unchecked(mut self, other: Span) -> Self {
        self.stop = other.stop;
        self
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

mod sealed {
    use crate::{GetSpan, Span};

    pub(crate) trait Sealed {}

    impl<T0, T1> Sealed for Result<(T0, T1), Span> where T0: GetSpan {}
}

/// Commodity trait for adding an offset to a lexer result.
#[allow(private_bounds, rustdoc::missing_doc_code_examples)]
pub trait LexResultExt: Sealed {
    /// Map the result, with the offset.
    ///
    /// # Examples
    /// ```compile_fail
    /// use cryo_span::{INITIAL_FILE, LexResultExt, Span};
    ///
    /// // add 10 to each of the indices, regardless if `Err` or `Ok`.
    /// let lexer_result = Ok(((), Span::new(INITIAL_FILE, 0, 10))).map_span(10);
    /// let lexer_result_err = Err(Span::new(INITIAL_FILE, 0, 10)).map_span(10);
    #[must_use]
    fn map_span(self, offset: usize) -> Self;
}

impl<T0, T1> LexResultExt for Result<(T0, T1), Span>
where
    T0: GetSpan,
{
    fn map_span(self, offset: usize) -> Self {
        self.map(|(mut v, s)| {
            v.get_span().start += offset;
            v.get_span().stop += offset;
            (v, s)
        })
        .map_err(|mut v| {
            v.start += offset;
            v.stop += offset;
            v
        })
    }
}

/// Accessor trait for [`LexResultExt`]. This trait should not be implemented manually, rather, it should be implemented with the [`impl_get_span`] macro.
#[allow(rustdoc::missing_doc_code_examples)]
pub trait GetSpan {
    #[doc(hidden)]
    fn get_span(&mut self) -> &mut Span;
}

/// Macro for automatically implementing [`GetSpan`]. \
/// The macro has the signature:
/// ```compile_fail
/// impl_get_span!(struct, field)
/// ```
/// Where "struct" is the name of the struct [`GetSpan`] should be implemented for and "field" is the field that the [`Span`] is stored in.
/// ## Examples
/// ```rust
/// use cryo_span::{impl_get_span, Span};
/// struct SpanContainer(Span);
///
/// impl_get_span!(SpanContainer, 0);
/// ```
#[macro_export]
macro_rules! impl_get_span {
    ($ty:ty, $field:tt) => {
        #[automatically_derived]
        impl $crate::GetSpan for $ty {
            fn get_span(&mut self) -> &mut Span {
                &mut self.$field
            }
        }
    };

    ($ty:ty) => {
        #[automatically_derived]
        impl $crate::GetSpan for $ty {
            fn get_span(&mut self) -> &mut Span {
                self
            }
        }
    };
}

impl_get_span!(Span);
