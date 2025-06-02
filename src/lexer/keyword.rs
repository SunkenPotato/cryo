use crate::span::Span;

use super::{
    INITIAL_FILE, Lex, tag,
    tokens::{Token, TokenType},
};

macro_rules! keyword {
    ($($variant:ident = $val:expr),*) => {
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        pub enum Keyword {
            $(
                $variant,
            )*
        }

        impl Keyword {
            pub const VARIANTS: &[Self] = &[
                $(Self::$variant,)*
            ];

            #[must_use]
            pub const fn get(&self) -> &str {
                match self {
                    $(
                        Self::$variant => $val,
                    )*
                }
            }
        }
    };
}

keyword! {
    Let = "let",
    Mut = "mut"
}

impl Lex for Keyword {
    fn lex(input: &str) -> Result<(super::tokens::Token, &str), crate::span::Span> {
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
