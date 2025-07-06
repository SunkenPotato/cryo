//! Atoms.
//!
//! Atoms are components of code that are constant and only can be parsed from one specific atom token.

use crate::parser::Parse;
use cryo_lexer::atoms::Semi as SToken;

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
