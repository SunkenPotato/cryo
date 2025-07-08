//! Atoms.
//!
//! Atoms are components of code that are constant and only can be parsed from one specific atom token.

use crate::parser::Parse;
use cryo_lexer::{
    atoms::{
        Assign as AToken, Colon as ColToken, Comma as CToken, Keyword, LCurly as LCToken,
        LParen as LPToken, RCurly as RCToken, RParen as RPToken, Semi as SToken,
    },
    stream::TokenStreamError,
};
use cryo_span::Spanned;

macro_rules! atom {
    ($(#[doc = $doc:literal])* $out:ident, $token:ident) => {
        $(#[doc = $doc])*
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        pub struct $out;

        impl Parse for $out {
            type Output = Self;

            fn parse(
                tokens: &mut cryo_lexer::stream::TokenStreamGuard,
            ) -> crate::parser::ParseResult<Self::Output> {
                tokens
                    .advance_require::<$token>()
                    .map(|v| v.map(|_| Self))
                    .map_err(Into::into)
            }
        }
    };

    ($(#[doc = $doc:literal])* $out:ident, $token:ident::$variant:ident) => {
        $(#[doc = $doc])*
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        pub struct $out;

        impl Parse for $out {
            type Output = Self;

            fn parse(
                tokens: &mut cryo_lexer::stream::TokenStreamGuard,
            ) -> crate::parser::ParseResult<Self::Output> {
                match tokens.advance_require::<$token>() {
                    Ok(
                        s @ Spanned {
                            t: $token::$variant,
                            ..
                        },
                    ) => Ok(s.map(|_| Self)),
                    Ok(Spanned { span, .. }) => Err(Box::new(TokenStreamError::IncorrectToken(
                        stringify!($token::$variant),
                        span,
                    ))),
                    Err(e) => Err(Box::new(e)),
                }
            }
        }
    };
}

atom!(
    /// A semicolon (`;`).
    Semi, SToken
);

atom!(
    /// `=`.
    Assign, AToken
);

atom!(
    /// A left curly brace (`{`).
    LCurly, LCToken
);

atom!(
    /// A right curly brace (`}`).
    RCurly, RCToken
);

atom!(
    /// The `let` keyword.
    Let, Keyword::Let
);

atom!(
    /// The `mut` keyword.
    Mut, Keyword::Mut
);

atom!(
    /// The `if` keyword.
    If, Keyword::If
);

atom!(
    /// The `else` keyword.
    Else, Keyword::Else
);

atom!(
    /// A comma.
    Comma, CToken
);

atom!(
    /// A colon.
    Colon, ColToken
);

atom!(
    /// A left parenthesis.
    LParen, LPToken
);

atom!(
    /// A right parenthesis.
    RParen, RPToken
);

atom!(
    /// The struct keyword.
    Struct, Keyword::Struct
);
