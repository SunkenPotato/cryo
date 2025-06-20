//! Keywords in the cryo language.
//!
//! Keywords are reserved tokens that have no meaning but to hint at certain token compositions, such as `let` for bindings and `fn` for functions.

use cryo_span::Span;

/// The `let` keyword.
pub const KW_LET: &str = "let";
/// The `mut` keyword.
pub const KW_MUT: &str = "mut";

use super::{
    INITIAL_FILE, Lex, tag,
    tokens::{Token, TokenType},
};

macro_rules! keyword {
    ($($variant:ident = $val:tt),*) => {
        /// A keyword. View the module-level docs for more info.
        #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
        pub enum Keyword {
            $(
                #[allow(missing_docs)]
                $variant,
            )*
        }

        impl Keyword {
            /// All the enum variants / keywords.
            pub const VARIANTS: &[Self] = &[
                $(Self::$variant,)*
            ];

            /// Get the string slice that this keyword is.
            #[must_use]
            pub const fn get(&self) -> &str {
                match self {
                    $(
                        Self::$variant => $val,
                    )*
                }
            }
        }

        impl std::str::FromStr for Keyword {
            type Err = ();

            /// Attempt to create a variant from a string slice.
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $(
                        $val => Ok(Self::$variant),
                    )*
                    _ => Err(())
                }
            }
        }
    };
}

keyword! {
    Let = KW_LET,
    Mut = KW_MUT
}

impl Lex for Keyword {
    fn lex(input: &str) -> Result<(super::tokens::Token, &str), Span> {
        for variant in Self::VARIANTS {
            let s = variant.get();
            let span = Span::new(INITIAL_FILE, 0, s.len());

            if let Some(s) = tag(s, input) {
                return Ok((
                    Token {
                        token: TokenType::Keyword(*variant),
                        span,
                    },
                    s,
                ));
            }
        }

        Err(Span::new(INITIAL_FILE, 0, input.find(' ').unwrap_or(1)))
    }
}
