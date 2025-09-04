//! Parser for the `cryo` language.

#[cfg(test)]
mod test_util;

use cryo_diagnostic::{Diagnostic, DiagnosticKind, Diagnostics, SourceFile};
use cryo_lexer::stream::{Guard, TokenStream};
use cryo_span::Span;

/// The result of a parser.
pub type ParseResult<T> = Result<T, ParseError>;

/// The parse trait. This should be implemented for all types which can be parsed from a tokenstream.
pub trait Parse: Sized + IsFail {
    /// Parse `Self` from a sequence of tokens.
    fn parse(tokens: &mut Guard) -> ParseResult<Self>;
}

/// Trait for checking whether parser productions are partial or complete fails, so that they may be reinterpreted as diagnostics.
pub trait IsFail {
    /// Check whether `self` is an error variant.
    fn is_fail(&self) -> bool;
}

/// A very basic, uninformative parse error.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ParseError {
    /// The span of where the error occurred.
    pub span: Span,
    /// The additional context to show.
    pub context: Span,
    /// The kind of error this is.
    pub kind: ParseErrorKind,
}

/// Parse error types. Used in conjunction with [`ParseError`].
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ParseErrorKind {
    /// A semicolon was missing.
    MissingSemi,
}

impl ParseErrorKind {
    /// The error code.
    pub const fn code(self) -> Option<u16> {
        match self {
            Self::MissingSemi => None,
        }
    }

    /// The error message.
    pub const fn message(self) -> &'static str {
        match self {
            Self::MissingSemi => "expected `;`",
        }
    }
}

impl ParseError {
    /// Convert this error into a diagnostic.
    pub fn into_diagnostic(self) -> Diagnostic {
        Diagnostic::new(
            DiagnosticKind::Error(self.kind.code()),
            self.kind.message().to_owned(),
            vec![],
            vec![],
            self.context,
            self.span,
        )
    }
}

/// A parser.
#[derive(Debug)]
pub struct Parser<'d> {
    /// The tokenstream this parser operates on.
    pub stream: TokenStream,
    /// The source of the inner tokenstream.
    pub source: SourceFile,
    /// The diagnostics reported by subparsers.
    pub diagnostics: &'d mut Diagnostics,
}

impl<'d> Parser<'d> {
    /// Create a new parser.
    pub const fn new(
        stream: TokenStream,
        source: SourceFile,
        diagnostics: &'d mut Diagnostics,
    ) -> Self {
        Self {
            stream,
            source,
            diagnostics,
        }
    }

    /// Parse the given inputs.
    pub fn parse(self) -> Result<(SourceFile, AbstractSyntaxTree), ParseError> {
        todo!()
    }
}

/// An abstract syntax tree, produced by a parser.
pub struct AbstractSyntaxTree {}
