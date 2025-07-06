//! A binding reference.
//!
//! This is simply an identifier.

use cryo_lexer::identifier::Identifier;
use internment::Intern;

use crate::parser::Parse;

/// A binding reference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BindingRef(pub Intern<str>);

impl Parse for BindingRef {
    type Output = Self;

    fn parse(
        tokens: &mut cryo_lexer::stream::TokenStreamGuard,
    ) -> crate::parser::ParseResult<Self::Output> {
        tokens
            .advance_require::<Identifier>()
            .map(|v| v.map(|v| Self(Intern::from(v.0))))
            .map_err(Into::into)
    }
}
