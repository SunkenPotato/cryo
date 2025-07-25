//! Identifiers.
use cryo_lexer::identifier::Identifier;
use cryo_span::Spanned;
use internment::Intern;

use crate::parser::Parse;

/// An identifier.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Ident(pub Intern<str>);

impl Parse for Ident {
    type Output = Self;

    fn parse(
        tokens: &mut cryo_lexer::stream::TokenStreamGuard,
    ) -> crate::parser::ParseResult<Self::Output> {
        tokens
            .advance_require::<Identifier>()
            .map_err(Spanned::cvt)
            .map(|v| v.map(|v| Ident(Intern::from(v.0))))
    }
}
