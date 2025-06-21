//! Binding references.
//!
//! Binding references represent the value they point to when evaluated. \
//! As such, they are an expression.
use cryo_lexer::identifier::Identifier;

use crate::{Parse, Spanned, SpecToken};

/// A reference to a binding.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct BindingRef(pub Identifier);

impl Parse for BindingRef {
    fn parse<'t>(stream: &mut crate::stream::TokenStreamGuard<'t>) -> crate::ParseResult<Self> {
        let SpecToken { token, span } = stream.advance_require::<Identifier>()?;
        let token = *token;

        Ok(Spanned::new(Self(token), span))
    }
}

#[cfg(test)]
mod tests {
    use cryo_lexer::{identifier::Identifier, t};

    use crate::{TokenStream, expr::binding_ref::BindingRef, parse_assert};

    #[test]
    fn parse_binding_ref() {
        let mut ts = TokenStream::new([t![id "bind"]]);

        parse_assert(&mut ts, BindingRef(Identifier::new("bind")));
    }
}
