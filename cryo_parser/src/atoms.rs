//! Atoms.
//!
//! Atoms are components of code that are constant and only can be parsed from one specific atom token.

use crate::parser::Parse;
use cryo_lexer::{
    atoms::{Assign as AToken, Keyword, Semi as SToken},
    stream::TokenStreamError,
};
use cryo_span::Spanned;

/// A semicolon.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Semi;

impl Parse for Semi {
    type Output = Self;

    fn parse(
        tokens: &mut cryo_lexer::stream::TokenStreamGuard,
    ) -> crate::parser::ParseResult<Self::Output> {
        tokens
            .advance_require::<SToken>()
            .map(|v| v.map(|_| Self))
            .map_err(Into::into)
    }
}

/// `=`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Assign;

impl Parse for Assign {
    type Output = Self;

    fn parse(
        tokens: &mut cryo_lexer::stream::TokenStreamGuard,
    ) -> crate::parser::ParseResult<Self::Output> {
        tokens
            .advance_require::<AToken>()
            .map(|v| v.map(|_| Self))
            .map_err(Into::into)
    }
}

/// The `let` keyword.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Let;

impl Parse for Let {
    type Output = Self;

    fn parse(
        tokens: &mut cryo_lexer::stream::TokenStreamGuard,
    ) -> crate::parser::ParseResult<Self::Output> {
        match tokens.advance_require::<Keyword>() {
            Ok(Spanned {
                t: Keyword::Let,
                span,
            }) => Ok(Spanned::new(Self, span)),
            Ok(Spanned { span, .. }) => {
                Err(Box::new(TokenStreamError::IncorrectToken("let", span)))
            }
            Err(e) => Err(Box::new(e)),
        }
    }
}

/// The `mut` keyword.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Mut;
impl Parse for Mut {
    type Output = Self;

    fn parse(
        tokens: &mut cryo_lexer::stream::TokenStreamGuard,
    ) -> crate::parser::ParseResult<Self::Output> {
        match tokens.advance_require::<Keyword>() {
            Ok(Spanned {
                t: Keyword::Mut,
                span,
            }) => Ok(Spanned::new(Self, span)),
            Ok(Spanned { span, .. }) => {
                Err(Box::new(TokenStreamError::IncorrectToken("mut", span)))
            }
            Err(e) => Err(Box::new(e)),
        }
    }
}
