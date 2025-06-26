use std::{
    fmt::{Display, Formatter},
    ops::{Add, AddAssign},
    sync::OnceLock,
};

use crate::source_map::SourceMap;

pub mod source_map;
#[cfg(test)]
mod tests;

static SOURCE_MAP: OnceLock<SourceMap> = OnceLock::new();

#[inline]
pub(crate) fn get_source_map() -> &'static SourceMap {
    SOURCE_MAP.get().expect("SourceMap should be initialized")
}

pub fn initialize(map: SourceMap) -> Result<&'static SourceMap, SourceMap> {
    SOURCE_MAP.set(map)?;

    Ok(get_source_map())
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Span {
    pub start: u32,
    pub stop: u32,
    pub file: u16,
}

impl Span {
    #[inline]
    pub const fn new(start: u32, stop: u32, file: u16) -> Self {
        assert!(stop > start);
        Self { start, stop, file }
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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let instance = get_source_map();
        let file = instance.get(self.file.into()).unwrap();

        let start = self.start.try_into().unwrap();
        let stop = self.stop.try_into().unwrap();
        let path = &file.file;
        let ((start_line, start_col), (end_line, end_col)) =
            (file.line_col(start).unwrap(), file.line_col(stop).unwrap());

        let source = file.resolve_source(start, stop).unwrap();

        write!(
            f,
            "{}:{start_line}:{start_col} - {end_line}:{end_col}:",
            path.display()
        )?;
        for line in source.lines() {
            write!(f, "\n\t5 | {line}")?;
        }

        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Spanned<T> {
    t: T,
    span: Span,
}

impl<T> Spanned<T> {
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
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} @ {}", self.t, self.span)
    }
}
