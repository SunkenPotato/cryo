//! Spans for the `cryo` language.
//!
//! A span declares a range inside a file.

use std::{
    fmt::{Display, Formatter},
    ops::{Add, AddAssign, Deref, DerefMut},
};

use crate::source_map::{SourceIndex, SourceMap};

pub mod source_map;

#[cfg(test)]
mod tests;

#[cfg(test)]
pub use tests::TempFile;

/// A span.
///
/// A span contains the start, the stop, and the file of the section it refers to. It acts similar to a range.
///
/// ## Examples
/// ```rust
/// use cryo_span::Span;
///
/// Span::new(0, 20);
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Span {
    /// The lower bound of this [`Span].
    pub start: usize,
    /// The upper bound of this [`Span`].
    pub stop: usize,
    /// The index of the file in the global [`SourceMap`].
    pub file: SourceIndex,
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
        Self::new_file(start, stop, SourceIndex::from_raw_unchecked(0))
    }

    #[inline]
    #[track_caller]
    /// Create a new span from a start, a stop, and a file index to the [`SourceMap`].
    pub const fn new_file(start: usize, stop: usize, file: SourceIndex) -> Self {
        assert!(stop >= start);
        Self { start, stop, file }
    }

    /// Offset the `start` and `stop` of the [`Span`] by `offset`.
    pub const fn offset(mut self, offset: usize) -> Self {
        self.start += offset;
        self.stop += offset;

        self
    }

    /// Obtain an interface which can display [`Span`]s.
    #[allow(private_interfaces)]
    pub const fn display<'span, 'map>(
        &'span self,
        map: &'map SourceMap,
    ) -> SpanDisplay<'span, 'map> {
        SpanDisplay(self, map)
    }
}

impl<T, const N: usize> From<[Spanned<T>; N]> for Spanned<[T; N]> {
    fn from(value: [Spanned<T>; N]) -> Self {
        let span = value.iter().fold(Span::ZERO, |a, b| a + b.span);
        let items = value.map(|s| s.t);

        Spanned { t: items, span }
    }
}

impl Add for Span {
    type Output = Self;
    fn add(mut self, rhs: Self) -> Self::Output {
        if rhs.stop >= self.stop {
            self.stop = rhs.stop;
        }
        self
    }
}

impl AddAssign for Span {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

/// Provides an interface which implements [`Display`].
pub struct SpanDisplay<'span, 'map>(&'span Span, &'map SourceMap);

impl Display for SpanDisplay<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Some(file) = self.1.get(self.0.file) else {
            return write!(
                f,
                "unable to find source file with index {}",
                self.0.file.index()
            );
        };

        let path = &file.file;
        let (Some((start_line, start_col)), Some((end_line, end_col))) =
            (file.line_col(self.0.start), file.line_col(self.0.stop))
        else {
            return write!(f, "could not format span {:?}", self.0);
        };

        let Ok(source) = file.resolve_source(self.0.start, self.0.stop) else {
            return write!(f, "could not resolve source of span {:?}", self.0);
        };

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

    /// Obtain an interface which implements [`Display`].
    #[allow(private_interfaces)]
    pub const fn display<'span, 'map>(
        &'span self,
        map: &'map SourceMap,
    ) -> SpannedDisplay<'span, 'map, T> {
        SpannedDisplay(self, map)
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

/// Provides access to a display implementation for `Spanned<T>`.
pub struct SpannedDisplay<'span, 'map, T>(&'span Spanned<T>, &'map SourceMap);

impl<T: Display> Display for SpannedDisplay<'_, '_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} @ {}", self.0.t, self.0.span.display(self.1))
    }
}
