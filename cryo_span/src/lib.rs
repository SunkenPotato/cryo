//! Spans for the `cryo` language.
//!
//! A span declares a range inside a file.

use std::{
    fmt::{Display, Formatter},
    ops::{Add, AddAssign, Deref, DerefMut},
    sync::OnceLock,
};

use crate::source_map::SourceMap;

pub mod source_map;

#[cfg(test)]
mod tests;

#[cfg(test)]
pub use tests::TempFile;

// TODO: get rid of this
static SOURCE_MAP: OnceLock<SourceMap> = OnceLock::new();

/// Initialise the library `SourceMap`. It cannot be changed once initialised.
///
/// Required for operations such as [`Span`] formatting to work.
///
/// ## Examples
/// ```rs
/// use cryo_span::source_map::SourceMap;
///
/// let source_map = SourceMap::from_paths(&[][..]).unwrap();
///
/// assert_matches!(cryo_span::initialise(source_map).clone(), Ok(_));
/// assert_matches!(cryo_span::initialise(source_map), Err(_));
/// ```
// TODO: remove this function and instead force [`Span`] to use a specialised display struct with a reference to a sourcemap.
pub fn initialise(map: SourceMap) -> Result<&'static SourceMap, SourceMap> {
    SOURCE_MAP.set(map)?;

    Ok(SOURCE_MAP.get().unwrap())
}

/// A span.
///
/// A span contains the start, the stop, and the file of the section it refers to. It acts similar to a range.
///
/// ## Examples
/// ```rs
/// Span::new(0, 20);
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Span {
    /// The lower bound of this [`Span].
    pub start: usize,
    /// The upper bound of this [`Span`].
    pub stop: usize,
    /// The index of the file in the global [`SourceMap`].
    // TODO: replace this with an IndexVec index.
    pub file: u16,
}

impl Span {
    /// A zero span. It has the file `0`, the start `0` and the stop `0`.
    pub const ZERO: Self = Span::new(0, 0);

    /// Create a new [`Span`] from a start and a stop.
    ///
    /// If `start < stop`, this function will panic.
    #[inline]
    #[track_caller]
    pub const fn new(start: usize, stop: usize) -> Self {
        Self::new_file(start, stop, 0)
    }

    #[inline]
    #[track_caller]
    /// Create a new span from a start, a stop, and a file index to the [`SourceMap`].
    pub const fn new_file(start: usize, stop: usize, file: u16) -> Self {
        assert!(stop >= start);
        Self { start, stop, file }
    }

    /// Offset the `start` and `stop` of the [`Span`] by `offset`.
    pub const fn offset(mut self, offset: usize) -> Self {
        self.start += offset;
        self.stop += offset;

        self
    }
}

impl Add for Span {
    type Output = Self;
    fn add(mut self, rhs: Self) -> Self::Output {
        debug_assert!(self.stop <= rhs.stop);
        self.stop = rhs.stop;
        self
    }
}

impl AddAssign for Span {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl Display for Span {
    // TODO: replace unwraps.
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Some(instance) = SOURCE_MAP.get() else {
            write!(f, "failed to get source map")?;
            return Ok(());
        };

        let Some(file) = instance.get(self.file.into()) else {
            write!(f, "unable to find source file with index {}", self.file)?;
            return Ok(());
        };

        let path = &file.file;
        let ((start_line, start_col), (end_line, end_col)) = (
            file.line_col(self.start).unwrap(),
            file.line_col(self.stop).unwrap(),
        );

        let source = file.resolve_source(self.start, self.stop).unwrap();

        write!(
            f,
            "{}:{start_line}:{start_col} - {end_line}:{end_col}:",
            path.display()
        )?;
        for (idx, line) in source.lines().enumerate() {
            write!(f, "\n\t{} | {line}", idx + start_line)?;
        }

        Ok(())
    }
}

/// A spanned generic.
///
/// This struct is simply the generic and a [`Span`].
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Spanned<T> {
    /// The given type.
    pub t: T,
    /// The span.
    pub span: Span,
}

impl<T> Spanned<T> {
    /// Create a new [`Spanned`] from `T` and a [`Span`].
    pub const fn new(t: T, span: Span) -> Self {
        Self { t, span }
    }

    /// Map a `Spanned<T>` to a `Spanned<U>` via a given function.
    pub fn map<F, U>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned {
            t: f(self.t),
            span: self.span,
        }
    }

    /// Extend the inner span by the given span.
    ///
    /// This method is equivalent to adding `span` to the inner ]`Span`].
    pub fn extend(mut self, span: Span) -> Self {
        self.span += span;
        self
    }

    /// Offset the inner [`Span`] by `offset`.
    ///
    /// This method is equivalent to calling [`Span::offset`] on the inner span.
    pub const fn offset(mut self, offset: usize) -> Self {
        self.span = self.span.offset(offset);
        self
    }

    /// Reinterpret this `Spanned<T>` as a `(T, Span)`.
    pub fn tuple(self) -> (T, Span) {
        (self.t, self.span)
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.t
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.t
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} @ {}", self.t, self.span)
    }
}
