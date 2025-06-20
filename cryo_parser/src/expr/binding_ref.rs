//! Binding references.
//!
//! Binding references represent the value they point to when evaluated.
//! As such, they are an expression.
use cryo_lexer::identifier::Identifier;

use crate::{Parse, SpecToken};
use cryo_lexer::identifier::Identifier as IToken;

/// A reference to a binding.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct BindingRef(pub Identifier);

impl Parse for BindingRef {
    fn parse(stream: &mut crate::TokenStream) -> Result<Self, Box<dyn crate::error::ParseError>> {
        let SpecToken { token, span } = stream.advance_require::<IToken>()?;
        Ok(Self(*token))
    }
}

#[cfg(test)]
mod tests {
    use cryo_lexer::{identifier::Identifier, t};

    use crate::{Parse, TokenStream, expr::binding_ref::BindingRef};

    #[test]
    fn parse_binding_ref() {
        let mut ts = TokenStream::new([t![id "bind"]]);

        assert_eq!(
            BindingRef::parse(&mut ts),
            Ok(BindingRef(Identifier::new("bind")))
        )
    }
}
