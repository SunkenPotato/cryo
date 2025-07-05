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

static SOURCE_MAP: OnceLock<SourceMap> = OnceLock::new();

pub fn initialize(map: SourceMap) -> Result<&'static SourceMap, SourceMap> {
    SOURCE_MAP.set(map)?;

    Ok(SOURCE_MAP.get().unwrap())
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Span {
    pub start: usize,
    pub stop: usize,
    pub file: u16,
}

impl Span {
    pub const ZERO: Self = Span::new(0, 0);

    #[inline]
    #[track_caller]
    pub const fn new(start: usize, stop: usize) -> Self {
        Self::new_file(start, stop, 0)
    }

    #[inline]
    #[track_caller]
    pub const fn new_file(start: usize, stop: usize, file: u16) -> Self {
        assert!(stop >= start);
        Self { start, stop, file }
    }

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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Spanned<T> {
    pub t: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub const fn new(t: T, span: Span) -> Self {
        Self { t, span }
    }

    pub fn map<F, U>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned {
            t: f(self.t),
            span: self.span,
        }
    }

    pub fn extend(mut self, span: Span) -> Self {
        self.span += span;
        self
    }

    pub const fn offset(mut self, offset: usize) -> Self {
        self.span = self.span.offset(offset);
        self
    }

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
