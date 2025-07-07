//! Visibility.
//!
//! Visibility defines from where an item is accessible.

use crate::parser::Parse;
use cryo_lexer::atoms::Visibility as VisToken;

/// Visibility.
///
/// Visibility defines where an item is accessible from.
pub enum Visibility {
    /// Private visibility (only accessible within the current cube).
    Private,
    /// Public visibility (accessible from all cubes).
    Public,
}

impl Parse for Visibility {
    type Output = Visibility;

    fn parse(
        tokens: &mut cryo_lexer::stream::TokenStreamGuard,
    ) -> crate::parser::ParseResult<Self::Output> {
        tokens
            .advance_require::<VisToken>()
            .map(|v| {
                v.map(|v| match v {
                    VisToken::Public => Self::Public,
                    VisToken::Private => Self::Private,
                })
            })
            .map_err(Into::into)
    }
}
