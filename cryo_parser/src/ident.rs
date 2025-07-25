//! Identifiers.
use cryo_lexer::identifier::Identifier;
use internment::Intern;

use crate::Parse;

/// An identifier.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Ident(pub Intern<str>);

impl Parse for Ident {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        Ok(Self(Intern::from(
            tokens.advance_require::<Identifier>()?.0,
        )))
    }
}
