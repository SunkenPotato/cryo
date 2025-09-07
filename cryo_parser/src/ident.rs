//! Parsed identifiers.

use std::ops::Deref;

use cryo_lexer::{Symbol, TokenKind};
use cryo_parser_proc_macro::IsFail;

use crate::{IsFail, Parse};

macro_rules! keywords {
    ($($key:ident = $val:expr)*) => {
        $(
            #[doc = concat!("The ", $val, " keyword.")]
            pub static $key: ::std::sync::LazyLock<Symbol> = ::std::sync::LazyLock::new(|| Symbol::from($val));
        )*

        #[doc = "A list of all keywords"]
        pub static KEYWORDS: &[&::std::sync::LazyLock<Symbol>] = &[$(&$key),*];
    }
}

keywords! {
    LET = "let"
    MUT = "mut"
    RECORD = "record"
    ENUM = "enum"
    IF = "if"
    ELSE = "else"
    FUNC = "func"
    CONST = "const"
}

/// An identifier.
#[derive(Clone, Copy, PartialEq, Eq, Debug, IsFail)]
#[fail = false]
#[repr(transparent)]
pub struct Ident(pub Symbol);

impl Ident {
    /// Check if this identifier is a keyword.
    pub fn is_kw(&self) -> bool {
        KEYWORDS.iter().any(|v| ***v == self.0)
    }
}

impl Parse for Ident {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        tokens
            .advance_require(TokenKind::Identifier)
            .map(|v| Self(v.t))
            .map_err(Into::into)
    }
}

impl Deref for Ident {
    type Target = Symbol;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
