//! Identifiers.

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
    MUT = mut
}

/// A validated identifier, that is, one proven not to be a keyword.
pub struct Ident {
    /// The actual identifier.
    pub sym: Symbol,
    /// Represents whether this identifier is valid or not.
    pub valid: bool,
}

impl Parse for Ident {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        KEYWORDS.with(|keywords| {
            let ident = tokens.advance_require::<Identifier>()?;

            match keywords.iter().find(|v| **v == ident.t.0) {
                Some(v) => Ok(Self {
                    sym: *v,
                    valid: false,
                }),
                None => Ok(Self {
                    sym: ident.t.0,
                    valid: true,
                }),
            }
        })
    }
}
