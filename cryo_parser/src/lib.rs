//! Parser for the `cryo` language.
#![feature(sync_unsafe_cell)]

#[cfg(test)]
mod test_util;

pub mod expr;

use cryo_diagnostic::{Diagnostics, SourceFile};
use cryo_lexer::{
    Token, TokenKind,
    stream::{Guard, TokenStream, TokenStreamError},
};
use cryo_span::{HasSpan, Span};

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

impl IsFail for () {
    fn is_fail(&self) -> bool {
        false
    }
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
    /// An incorrect token was received.
    IncorrectToken {
        /// The token which was received.
        got: Token,
        /// The expected token kind.
        expected: ExpectedToken,
    },
    /// Unexpected EOF.
    EndOfInput(Span),
}

/// Utility for storing either one expected token kind or multiple in the form of a static slice.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ExpectedToken {
    /// An owned [`TokenKind`].
    Owned(TokenKind),
    /// A static slice of [`TokenKind`]s
    Multiple(&'static [TokenKind]),
}

impl ParseError {
    /// Create a new [`ParseError`].
    pub const fn new(span: Span, context: Span, kind: ParseErrorKind) -> Self {
        Self {
            span,
            context,
            kind,
        }
    }
}

impl From<TokenStreamError> for ParseError {
    fn from(value: TokenStreamError) -> Self {
        match value {
            TokenStreamError::EndOfInput(s) => Self {
                span: s,
                context: s,
                kind: ParseErrorKind::EndOfInput(s),
            },
            TokenStreamError::IncorrectToken { got, expected } => Self {
                span: got.span,
                context: got.span,
                kind: ParseErrorKind::IncorrectToken {
                    got,
                    expected: ExpectedToken::Owned(expected),
                },
            },
        }
    }
}

impl HasSpan for ParseErrorKind {
    fn span(&mut self) -> &mut Span {
        match self {
            Self::IncorrectToken { got, .. } => &mut got.span,
            Self::EndOfInput(s) => s,
        }
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

impl AbstractSyntaxTree {}
