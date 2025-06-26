use std::{
    fmt::{Display, Formatter},
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
        Self { start, stop, file }
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
