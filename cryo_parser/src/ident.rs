//! Identifiers.

use std::thread::LocalKey;

use cryo_lexer::{Symbol, identifier::Identifier};

use crate::Parse;

macro_rules! keywords {
    ($($name:ident = $value:tt),*) => {
        thread_local! {
            $(
                #[doc = concat!("The ", stringify!($value), " keyword.")]
                pub static $name: Symbol = Symbol::new(stringify!($value));
            )*

            /// A list of all keywords.
            pub static KEYWORDS: Box<[Symbol]> = Box::new([$(Symbol::new(stringify!($value))),*]);
        }
    };
}

keywords! {
    LET = let,
    MUT = mut,
    IF = if,
    ELSE = else,
    FUN = fun
}

/// A validated identifier, that is, one proven not to be a keyword.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Ident {
    /// The actual identifier.
    pub sym: Symbol,
    /// Represents whether this identifier is valid or not.
    pub valid: bool,
}

trait IdentEq<'a> {
    fn ieq(&'a self, sym: &Symbol) -> bool;
}

impl IdentEq<'_> for Symbol {
    fn ieq(&'_ self, sym: &Symbol) -> bool {
        self.eq(sym)
    }
}

impl IdentEq<'_> for str {
    fn ieq(&'_ self, sym: &Symbol) -> bool {
        sym.eq(self)
    }
}

impl IdentEq<'static> for LocalKey<Symbol> {
    fn ieq(&'static self, sym: &Symbol) -> bool {
        self.with(|v| v.eq(sym))
    }
}

impl Ident {
    /// Require this identifier to be equal to the value specified. This is mainly a convenience method for comparisons to `LocalKey<Symbol>`.
    pub fn require<'a>(self, v: &'a impl IdentEq<'a>) -> Result<Self, Self> {
        v.ieq(&self.sym).then_some(self).ok_or(self)
    }
}

impl From<Identifier> for Ident {
    fn from(ident: Identifier) -> Self {
        KEYWORDS.with(|keywords| match keywords.iter().find(|v| **v == ident.0) {
            Some(v) => Self {
                sym: *v,
                valid: false,
            },
            None => Self {
                sym: ident.0,
                valid: true,
            },
        })
    }
}

impl Parse for Ident {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let ident = tokens.advance_require::<Identifier>()?;

        Ok(Self::from(*ident.t))
    }
}

#[cfg(test)]
mod tests {
    use cryo_lexer::Symbol;
    use cryo_span::{Span, Spanned};

    use crate::{ident::Ident, test_util::assert_parse};

    #[test]
    fn do_not_parse_kw_as_ident() {
        assert_parse(
            "let",
            Spanned::new(
                Ident {
                    sym: Symbol::new("let"),
                    valid: false,
                },
                Span::new(0, 3),
            ),
        );
    }
}
