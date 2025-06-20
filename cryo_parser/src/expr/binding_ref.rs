//! Binding references.
//!
//! Binding references represent the value they point to when evaluated. \
//! As such, they are an expression.
use cryo_lexer::identifier::Identifier;

use crate::{Parse, Spanned, SpecToken};
use cryo_lexer::identifier::Identifier as IToken;

/// A reference to a binding.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct BindingRef(pub Identifier);

impl Parse for BindingRef {
    fn parse(
        stream: &mut crate::TokenStream,
    ) -> Result<Spanned<Self>, Box<dyn crate::error::ParseError>> {
        let SpecToken { token, span } = stream.advance_require::<IToken>()?;
        let token = *token;
        stream.sync();
        Ok(Spanned(Self(token), span))
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
