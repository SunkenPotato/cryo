use std::{
    fs::File,
    io::{self, BufReader, Read, Seek},
    path::{Path, PathBuf},
    sync::{Arc, Mutex, mpsc},
    thread,
};

use memchr::Memchr;

use crate::source_map::monotonic_vec::MonotonicVec;

const NEWLINE: u8 = b'\n';

mod monotonic_vec {
    use std::ops::Deref;

    #[derive(Clone, PartialEq, Eq, Debug)]
    pub struct MonotonicVec<T>(Vec<T>);

    impl<T> MonotonicVec<T> {
        pub const fn new() -> Self {
            Self(Vec::new())
        }

        pub fn push(&mut self, v: T) {
            self.0.push(v)
        }
    }

    impl<T> Deref for MonotonicVec<T> {
        type Target = Vec<T>;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct SourceFile {
    pub(crate) line_endings: Vec<usize>,
    pub(crate) file: PathBuf,
}

impl SourceFile {
    pub(crate) fn new(path: impl Into<PathBuf> + AsRef<Path>) -> std::io::Result<Self> {
        let file = File::open(&path)?;

        Ok(Self {
            file: path.into(),
            line_endings: Self::compute_line_endings(BufReader::new(file))?,
        })
    }

    pub(crate) fn compute_line_endings<R>(mut reader: BufReader<R>) -> std::io::Result<Vec<usize>>
    where
        R: Read,
    {
        let mut line_endings = vec![];
        let mut buffer = [0; 1024];

        let mut bytes_read = 0;

        loop {
            let n = reader.read(&mut buffer)?;

            if n == 0 {
                break;
            }

            let positions = Memchr::new(NEWLINE, &buffer).map(|v| v + bytes_read);

            line_endings.extend(positions);
            bytes_read += n;
        }

        Ok(line_endings)
    }

    pub fn line_col(&self, offset: usize) -> Option<(usize, usize)> {
        let line_end = match match self.line_endings.binary_search(&offset) {
            Ok(v) => v,
            Err(v) => v,
        } {
            0 => return Some((0, offset)),
            v => v,
        };

        let col = offset - self.line_endings.get(line_end - 1).unwrap();

        Some((line_end + 1, col))
    }

    pub fn resolve_source(&self, start: usize, stop: usize) -> Result<String, io::Error> {
        let mut file = File::open(&self.file)?;
        file.seek(io::SeekFrom::Start(start as _))?;

        let len = stop - start;
        let mut buffer = vec![0u8; len];

        file.read_exact(&mut buffer)?;

        Ok(String::from_utf8(buffer).expect("source should be valid UTF-8"))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SourceMap {
    pub(crate) files: MonotonicVec<SourceFile>,
}

impl SourceMap {
    pub fn from_paths(paths: &[&Path]) -> std::io::Result<Self> {
        let files = thread::scope(|s| {
            let (sender, receiver) =
                mpsc::channel::<Box<dyn FnOnce() -> Result<(), io::Error> + Send + Sync + '_>>();
            let receiver = Arc::new(Mutex::new(receiver));
            let files = Arc::new(Mutex::new(MonotonicVec::new()));
            let error = Arc::new(Mutex::new(None));

            let mut handles = vec![];

            for _ in 0..4 {
                let receiver = Arc::clone(&receiver);
                let error = Arc::clone(&error);
                let handle = s.spawn(move || {
                    while let Ok(job) = receiver.lock().unwrap().recv() {
                        if let Err(e) = job() {
                            *error.lock().unwrap() = Some(e); // ignore if already set
                        }
                    }
                });
                handles.push(handle);
            }

            for path in paths {
                let files = Arc::clone(&files);
                let job = Box::new(move || -> Result<(), io::Error> {
                    let result = SourceFile::new(path)?;
                    files.lock().unwrap().push(result);
                    Ok(())
                });
                sender.send(job).unwrap();
            }

            drop(sender); // close the channel

            for handle in handles {
                handle.join().unwrap();
                if let Some(e) = error.lock().unwrap().take() {
                    return Err(e);
                }
            }

            Ok(Arc::try_unwrap(files)
                .expect("Arc should be unique")
                .into_inner()
                .unwrap())
        })?;

        Ok(Self { files })
    }

    pub(crate) fn get(&self, idx: usize) -> Option<&SourceFile> {
        self.files.get(idx)
    }
}
