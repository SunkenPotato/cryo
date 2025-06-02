use thiserror::Error;

mod source;

pub use source::*;

pub const INITIAL_FILE: usize = 0;

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

pub trait LexResultExt {
    #[must_use]
    fn map_span(self, orig_len: usize) -> Self;
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

pub trait GetSpan {
    fn get_span(&mut self) -> &mut Span;
}

#[macro_export]
macro_rules! impl_get_span {
    ($type:ty, $field:tt) => {
        #[automatically_derived]
        impl $crate::GetSpan for $type {
            fn get_span(&mut self) -> &mut Span {
                &mut self.$field
            }
        }
    };
}
