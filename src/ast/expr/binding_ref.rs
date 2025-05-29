use crate::ast::{Identifier, IdentifierParseError, Parse};

#[derive(Debug, PartialEq)]
pub struct BindingRef(pub Identifier); // Binding references are not equivalent to identifiers. identifiers cannot be evaluated (do not have a value)

impl Parse for BindingRef {
    type Error = IdentifierParseError;

    fn parse(input: &str) -> Result<(Self, &str), Self::Error> {
        Identifier::parse(input).map(|(v, s)| (Self(v), s))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Identifier, Parse, expr::binding_ref::BindingRef};

    #[test]
    fn parse_binding_reference() {
        let s = "binding";

        assert_eq!(
            Ok((BindingRef(Identifier::new("binding")), "")),
            BindingRef::parse(s)
        )
    }
}
