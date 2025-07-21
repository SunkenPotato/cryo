//! Parse errors.
//!
//! To accomodate different error types being returned from parsers, rather than returning a concrete type, parsers return a `Result<T, Box<dyn ParseError>>`.
//!
//! A parse error consists of a `code` indicating the code of the error type, a `subcode` which is unique to each variant within the enum, a [`Span`] supplied by `Spanned<ErrorType>` and a name used for display operations.
//!
//! Furthermore, each parse error type implements [`std::fmt::Display`].
//!
//! To declare a parse error type, view [`parse_error`](cryo_parser::parse_error).
// TODO refactor this to be a concrete struct instead of a trait

#![allow(private_bounds)]

use std::fmt::Debug;

use cryo_lexer::stream::TokenStreamError;
use derive_more::From;

use crate::S;

pub type ParseError = S<ParseErrorKind>;

#[derive(Debug, PartialEq, Eq, From)]
pub enum ParseErrorKind {
    TokenStreamError(TokenStreamError),
    LiteralParseError(LiteralParseError),
}

impl From<TokenStreamError> for ParseError {
    fn from(value: TokenStreamError) -> Self {}
}
