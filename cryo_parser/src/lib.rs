//! Parser for the `cryo` language.
#![feature(sync_unsafe_cell)]

#[cfg(test)]
mod test_util;

pub mod expr;
pub mod ident;
pub mod item;
pub mod stmt;

use std::sync::LazyLock;

use cryo_diagnostic::{Diagnostics, SourceFile};
use cryo_lexer::{
    Token, TokenKind,
    atoms::Comma,
    stream::{Guard, StreamLike, TokenStream, TokenStreamError},
};
use cryo_span::{Span, Spanned};
use internment::Intern;

use crate::ident::Ident;

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
#[derive(Clone, Copy, Debug)]
pub enum ParseErrorKind {
    /// An incorrect token was received.
    IncorrectToken {
        /// The token which was received.
        got: Token,
        /// The expected token kind.
        expected: OneOrMany<TokenKind>,
    },
    /// Unexpected EOF.
    EndOfInput(Span),
    /// An unclosed delimiter was parsed.
    UnclosedDelimiter(TokenKind),
    /// The parser expected a keyword.
    ExpectedKeyword(OneOrMany<&'static LazyLock<Intern<str>>>),
}

impl PartialEq for ParseErrorKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::EndOfInput(v1), Self::EndOfInput(v2)) => v1 == v2,
            (Self::UnclosedDelimiter(v1), Self::UnclosedDelimiter(v2)) => v1 == v2,
            (
                Self::IncorrectToken {
                    got: got1,
                    expected: expected1,
                },
                Self::IncorrectToken {
                    got: got2,
                    expected: expected2,
                },
            ) => got1 == got2 && expected1 == expected2,
            _ => false,
        }
    }
}
impl Eq for ParseErrorKind {}

/// Utility for storing either one or more of `T`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum OneOrMany<T: 'static> {
    /// An owned [`TokenKind`].
    Owned(T),
    /// A static slice of [`TokenKind`]s
    Multiple(&'static [T]),
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
                    expected: OneOrMany::Owned(expected),
                },
            },
        }
    }
}

/// A list of `T` separated by a comma, with the trailing comma being optional.
#[derive(Clone, PartialEq, Eq, Debug, cryo_parser_proc_macro::IsFail)]
#[fail = false]
pub struct CommaSeparated<T: Parse> {
    /// The `T` followed by a comma.
    pub inner: Vec<(Spanned<T>, Spanned<Comma>)>,
    /// The last `T`, if there is no comma.
    pub last: Option<Box<Spanned<T>>>,
}

impl<T: Parse> Parse for CommaSeparated<T> {
    fn parse(tokens: &mut Guard) -> ParseResult<Self> {
        let mut inner = Vec::new();
        let mut last = None;

        while let Ok(t) = tokens.spanning(T::parse) {
            match tokens.advance_require(TokenKind::Comma) {
                Ok(c) => inner.push((t, c.map(|_| Comma))),
                Err(_) => {
                    last.replace(Box::new(t));
                    break;
                }
            }
        }

        Ok(Self { inner, last })
    }
}

/// A typed identifier, like: `x: int`.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypedIdent {
    /// The identifier.
    pub ident: Ident,
    /// The type.
    pub ty: Path,
}

impl IsFail for TypedIdent {
    fn is_fail(&self) -> bool {
        self.ident.is_fail() && self.ty.is_fail()
    }
}

impl Parse for TypedIdent {
    fn parse(tokens: &mut Guard) -> ParseResult<Self> {
        let ident = tokens.with(Ident::parse)?;
        tokens.advance_require(TokenKind::Colon)?;
        let ty = tokens.with(Path::parse)?;
        Ok(Self { ident, ty })
    }
}

/// A path, like `std::vec::Vec`.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Path(Vec<Spanned<Ident>>);

impl IsFail for Path {
    fn is_fail(&self) -> bool {
        self.0.is_empty()
    }
}

impl Parse for Path {
    fn parse(tokens: &mut Guard) -> ParseResult<Self> {
        let mut buf = Vec::new();

        while let Ok(t) = tokens.spanning(Ident::parse) {
            buf.push(t);
            if tokens
                .with(|tokens| {
                    tokens
                        .advance_require(TokenKind::Colon)
                        .and_then(|_| tokens.advance_require(TokenKind::Colon))
                })
                .is_err()
            {
                break;
            }
        }

        Ok(Self(buf))
    }
}

#[cfg(test)]
impl From<Spanned<&str>> for Path {
    fn from(value: Spanned<&str>) -> Self {
        Self(vec![value.map(Intern::<str>::from).map(Ident)])
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
