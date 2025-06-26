use std::{
    array,
    fs::File,
    io::{BufReader, Cursor, Write},
    path::PathBuf,
};

use crate::source_map::{SourceFile, SourceMap};

static INPUT: &str = "Hello\nworld\n!\n;";

struct TempFile(File, PathBuf);

impl TempFile {
    fn create() -> Self {
        let id = rand::random::<u32>();
        let path = PathBuf::from(id.to_string());

        Self(File::create(&path).unwrap(), path)
    }

    fn new(s: &str) -> Self {
        let mut f = Self::create();

        f.0.write_all(s.as_bytes()).unwrap();

        f
    }
}

impl Drop for TempFile {
    fn drop(&mut self) {
        std::fs::remove_file(&self.1).unwrap();
    }
}

#[test]
fn source_map_line_endings() {
    let reader = BufReader::new(Cursor::new(INPUT));
    let endings = SourceFile::compute_line_endings(reader).unwrap();

    assert_eq!(endings, [5, 11, 13]);
}

#[test]
fn line_col_source_file() {
    let reader = BufReader::new(Cursor::new(INPUT));
    let source_file = SourceFile {
        line_endings: SourceFile::compute_line_endings(reader).unwrap(),
        file: PathBuf::new(),
    };

    assert_eq!(source_file.line_col(6), Some((2, 1)))
}

#[test]
fn resolve_source() {
    let file = TempFile::new(INPUT);
    let source_file = SourceFile::new(&file.1).unwrap();

    assert_eq!(source_file.resolve_source(6, 11).unwrap(), "world");
}

#[test]
fn source_map() {
    let files = array::from_fn::<_, 8, _>(|_| TempFile::new(INPUT));
    let paths = array::from_fn::<_, 8, _>(|i| files[i].1.as_path());
    let map = &SourceMap::from_paths(&paths).unwrap().files;
    let line_endings = array::from_fn::<_, 8, _>(|i| &map[i].line_endings[..]);

    let expected = array::from_fn::<_, 8, _>(|_| [5, 11, 13]);

    assert_eq!(line_endings, expected)
}
