//! Diagnostics for the `cryo` compiler.
#![warn(clippy::pedantic)]
#![expect(clippy::cast_possible_truncation)]
#![warn(clippy::unwrap_used)]

use std::{
    fmt::Display,
    io::{self, BufWriter, Write},
    path::PathBuf,
};

use cryo_span::{Span, SpanIndexable};
use similar::ChangeTag;
use yansi::{Color, Paint, Painted};

/// A type of diagnostic.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum DiagnosticKind {
    /// An error diagnostic, with an optional error code.
    Error(Option<u16>),
    /// A warning diagnostic.
    Warning,
    /// A suggestion diagnostic.
    Suggestion,
}

impl DiagnosticKind {
    /// Get the color associated for writing to a terminal.
    #[must_use]
    pub const fn color(&self) -> Color {
        match self {
            Self::Error(_) => Color::BrightRed,
            Self::Warning => Color::Yellow,
            Self::Suggestion => Color::BrightBlue,
        }
    }
}

impl Display for DiagnosticKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Error(Some(code)) => {
                write!(f, "{}", format_args!("error[{code:0>5}]").bright_red())
            }
            Self::Error(None) => write!(f, "error"),
            Self::Warning => write!(f, "warning"),
            Self::Suggestion => write!(f, "info"),
        }
    }
}

/// A suggestion emitted by the compiler on how to fix code.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct DiagnosticSuggestion {
    /// The suggestion note.
    pub note: String,
    /// The code the compiler is suggesting to correct.
    pub original: Span,
    /// The code the compiler suggests.
    pub suggestion: Option<String>,
}

/// A compiler diagnostic.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Diagnostic {
    /// The full code to show in the diagnostic.
    pub outer: Span,
    /// The code to highlight in the diagnostic.
    pub highlighted: Span,
    /// The message of this diagnostic.
    pub message: String,
    /// Any notes belonging to this diagnostic.
    pub notes: Vec<String>,
    /// Suggestions this diagnostic may provide.
    pub suggestions: Vec<DiagnosticSuggestion>,
    /// The type of diagnostic this is.
    pub kind: DiagnosticKind,
}

impl Diagnostic {
    /// Emit this diagnostic to the terminal standard error output.
    ///
    /// `ctx` specifies the source this function should pull file index/sources from.
    ///
    /// One should avoid calling this function and instead emit diagnostics through [`Diagnostics::emit`].
    ///
    /// # Errors
    /// Returns an error if this could not write to `stderr`.
    pub fn emit<C>(&self, ctx: &C) -> io::Result<()>
    where
        C: SpanIndexable,
    {
        let mut stderr = std::io::stderr();

        self.emit_to(ctx, &mut stderr)
    }

    /// Emit the diagnostic to a sink.
    ///
    /// # Errors
    /// Returns an I/O error if it could not write to the sink.
    pub fn emit_to<C, S>(&self, ctx: &C, sink: &mut S) -> io::Result<()>
    where
        C: SpanIndexable,
        S: Write,
    {
        show_diagnostic(self, ctx, sink)
    }

    /// Create a new diagnostic.
    ///
    /// # Panics
    /// Panics if `outer` does not contain `highlighted`.
    #[must_use]
    pub const fn new(
        kind: DiagnosticKind,
        message: String,
        notes: Vec<String>,
        suggestions: Vec<DiagnosticSuggestion>,
        outer: Span,
        highlighted: Span,
    ) -> Self {
        assert!(outer.contains(highlighted));

        Self {
            outer,
            highlighted,
            message,
            notes,
            suggestions,
            kind,
        }
    }
}

/// A collection of diagnostics.
#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

/// A source file handle, pointing to a file.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum SourceFile {
    /// A source file stored on the filesystem.
    Fs(PathBuf),
    /// A source file stored in memory. This will have the name specified in [`Self::MEMORY_FILE`].
    Memory(Box<str>),
}

impl SourceFile {
    /// The name for source files stored in memory.
    pub const MEMORY_FILE: &str = "memory.cr";
}

/// A source file, storing line endings and the contents. Used when reporting diagnostics.
pub struct ContentSourceFile {
    pub(crate) name: String,
    pub(crate) source: Box<str>,
    pub(crate) endings: Vec<u32>,
}

impl SourceFile {
    /// Get the contents of this source file.
    ///
    /// # Errors
    /// If this was a [`SourceFile::Fs`], and we were unable to read the underlying file, an I/O error will be returned.
    pub fn contents(self) -> io::Result<Box<str>> {
        match self {
            Self::Memory(c) => Ok(c),
            Self::Fs(f) => std::fs::read_to_string(f).map(String::into_boxed_str),
        }
    }

    /// Get the name of the current file.
    #[expect(clippy::missing_panics_doc)]
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Fs(path) => path
                .as_os_str()
                .to_str()
                .expect("path should be valid UTF-8"),
            Self::Memory(_) => Self::MEMORY_FILE,
        }
    }

    /// Converts this handle into a [`ContentSourceFile`].
    ///
    /// # Errors
    /// This will return an error if it could not read the contents of the stored file.
    pub fn into_with_content(self) -> std::io::Result<ContentSourceFile> {
        let name = self.name().to_owned();
        let source = self.contents()?;
        let mut endings = Vec::with_capacity(128);

        for (index, ch) in source.char_indices() {
            if ch == '\n' {
                endings.push(index as u32 + 1);
            }
        }

        endings.shrink_to_fit();

        Ok(ContentSourceFile {
            name,
            source,
            endings,
        })
    }
}

impl SpanIndexable for ContentSourceFile {
    fn file(&self) -> &str {
        &self.name
    }

    fn source(&self, span: Span) -> Option<&str> {
        let start = span.start as usize;
        let stop = span.stop as usize;
        self.source.get(start..stop)
    }

    fn line_col(&self, offset: u32) -> (u32, u32) {
        match self.endings.binary_search(&offset) {
            Ok(i) => {
                let line = i as u32 + 1;
                let col = 1;
                (line, col)
            }
            Err(i) => {
                let line = i as u32;
                let line_start = self.endings[i - 1];
                let col = offset - line_start + 1;
                (line, col)
            }
        }
    }
}

impl Diagnostics {
    /// Create a new, empty collection of diagnostics.
    #[must_use]
    pub const fn new() -> Self {
        Self {
            diagnostics: vec![],
        }
    }

    /// Add a diagnostic to this collection.
    pub fn push(&mut self, d: Diagnostic) {
        self.diagnostics.push(d);
    }

    /// Check whether this diagnostic collection has any diagnostics of the provided kind.
    #[must_use]
    pub fn has(&self, kind: std::mem::Discriminant<DiagnosticKind>) -> bool {
        self.diagnostics
            .iter()
            .any(|v| std::mem::discriminant(&v.kind) == kind)
    }

    /// Emit all diagnostics this contains.
    ///
    /// # Errors
    /// This errors if this could not write write to `stderr` or read the source file.
    pub fn emit(self, source: &ContentSourceFile) -> io::Result<()> {
        let mut writer = BufWriter::new(std::io::stderr());
        self.diagnostics
            .iter()
            .try_for_each(|d| d.emit_to(source, &mut writer))?;

        writer.flush()
    }
}

#[expect(clippy::too_many_lines)]
fn show_diagnostic<C, S>(diagnostic: &Diagnostic, context: &C, sink: &mut S) -> io::Result<()>
where
    C: SpanIndexable,
    S: Write,
{
    let kind = diagnostic.kind;
    let message = &diagnostic.message;
    let highlight_span = diagnostic.highlighted;
    let outer_span = diagnostic.outer;

    let source = context.source(outer_span).expect("span should be valid");
    let file = context.file();
    let (outer_start_row, _) = context.line_col(outer_span.start);
    let (hl_start_row, hl_start_col) = context.line_col(highlight_span.start);
    let (hl_end_row, hl_end_col) = context.line_col(highlight_span.stop);

    let start_col_idx = hl_start_col as usize - 1;
    let end_col_idx = hl_end_col as usize - 1;

    writeln!(sink, "{}: {}", kind.bold(), message.bold())?;
    writeln!(
        sink,
        "{}\n",
        format_args!("-> {file}:{hl_start_row}:{hl_start_col}")
            .bold()
            .underline()
    )?;

    for (index, line) in source.lines().enumerate() {
        let row = index as u32 + outer_start_row;
        write!(sink, "{:0>5} | ", row.cyan().bold())?;
        if hl_start_row == hl_end_row && row == hl_start_row {
            // highlight inside one line
            write!(sink, "{}", &line[..start_col_idx])?;
            write!(
                sink,
                "{}",
                &line[start_col_idx..end_col_idx].paint(kind.color().underline())
            )?;
            writeln!(sink, "{}", &line[end_col_idx..])?;
        } else if row == hl_start_row {
            write!(sink, "{}", &line[..start_col_idx])?;
            writeln!(
                sink,
                "{}",
                &line[start_col_idx..].paint(kind.color().underline())
            )?;
        } else if row == hl_end_row {
            // ending line
            writeln!(
                sink,
                "{}{}",
                &line[..end_col_idx].paint(kind.color().underline()),
                &line[end_col_idx..],
            )?;
        } else if hl_start_row < row && row < hl_end_row {
            // middle lines
            writeln!(sink, "{}", line.paint(kind.color().underline()))?;
        } else {
            writeln!(sink, "{line}")?;
        }
    }

    writeln!(sink)?;

    for note in &diagnostic.notes {
        writeln!(sink, "{} {}", "note:".bright_blue().bold(), note.bold())?;
    }

    if !diagnostic.notes.is_empty() {
        writeln!(sink)?;
    }

    for suggestion in &diagnostic.suggestions {
        writeln!(
            sink,
            "{} {}\n",
            "suggestion:".magenta().bold(),
            suggestion.note.bold()
        )?;
        if let Some(ref s) = suggestion.suggestion {
            let source = context
                .source(suggestion.original)
                .expect("span should be valid");

            let diff = similar::TextDiff::from_chars(source, s);

            let (row, _) = context.line_col(suggestion.original.start);

            // show improved code
            let mut curr_line: Vec<Painted<_>> = Vec::new();

            for (i, change) in diff.iter_all_changes().enumerate() {
                match change.tag() {
                    ChangeTag::Delete => curr_line.push(change.value().red().underline()),
                    ChangeTag::Insert => curr_line.push(change.value().green().underline()),
                    ChangeTag::Equal => curr_line.push(change.value().primary()),
                }

                if change.value().ends_with('\n') {
                    write!(sink, "{:0>5} | ", (row + i as u32).bright_blue().bold())?;
                    curr_line.iter().try_for_each(|v| write!(sink, "{v}"))?;
                    writeln!(sink)?;
                    curr_line.drain(..);
                }
            }

            if !curr_line.is_empty() {
                write!(sink, "{:0>5} | ", row.cyan().bold())?;
                curr_line
                    .into_iter()
                    .try_for_each(|v| write!(sink, "{v}"))?;
                writeln!(sink)?;
            }
        }
    }

    if let DiagnosticKind::Error(Some(code)) = diagnostic.kind {
        writeln!(
            sink,
            "{}",
            format_args!("for more information, run `cryo --explain E{code:0>4}`.").bold()
        )?;
    }

    Ok(())
}

#[cfg(test)]
#[expect(clippy::unwrap_used)]
mod tests {
    use super::*;

    struct Source(String, Vec<u32>);

    impl Source {
        pub fn new(s: String) -> Self {
            let mut line_starts = vec![0]; // first line always starts at 0
            for (idx, c) in s.char_indices() {
                if c == '\n' {
                    line_starts.push(idx as u32 + 1); // start of next line
                }
            }
            Self(s, line_starts)
        }
    }

    impl SpanIndexable for Source {
        fn file(&self) -> &'static str {
            "source.cr"
        }

        fn source(&self, span: Span) -> Option<&str> {
            self.0.get(span.start as usize..span.stop as usize)
        }

        fn line_col(&self, offset: u32) -> (u32, u32) {
            match self.1.binary_search(&offset) {
                Ok(i) => {
                    let line = i as u32 + 1;
                    let col = 1; // exactly at line start
                    (line, col)
                }
                Err(i) => {
                    let line = i as u32;
                    let line_start = self.1[i - 1];
                    let col = offset - line_start + 1; // 1-based col
                    (line, col)
                }
            }
        }
    }

    #[test]

    fn show_diagnostic_ex() {
        let source = Source::new(String::from("for i in 0..x {\n\tlet x = 5\n}"));
        let diagnostic = Diagnostic {
            outer: Span::new(0, 28),
            highlighted: Span::new(27, 28),
            message: String::from("expected `;`"),
            notes: vec![String::from("bindings must be terminated by a semicolon")],
            suggestions: vec![DiagnosticSuggestion {
                note: String::from("add a semicolon"),
                original: Span::new(17, 26),
                suggestion: Some(String::from("let x = 5;")),
            }],
            kind: DiagnosticKind::Error(Some(1)),
        };

        let mut stderr = std::io::stderr();
        show_diagnostic(&diagnostic, &source, &mut stderr).unwrap();
        stderr.flush().unwrap();
    }
}
