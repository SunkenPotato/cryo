//! Atoms.
//!
//! Atoms are components of code that are constant and only can be parsed from one specific atom token.

use cryo_lexer::{
    TokenLike,
    atoms::{
        Colon as ColToken, Comma as CToken, Dot as DToken, Equal as AToken, LCurly as LCToken,
        LParen as LPToken, RCurly as RCToken, RParen as RPToken, Semi as SToken,
    },
    stream::Guard,
};

use crate::ParseError;

struct Assert<const COND: bool>;

#[diagnostic::on_unimplemented(
    message = "it is undefined behaviour to pass a type to this function which is not a zero-sized type."
)]
trait True {}

impl True for Assert<true> {}

fn atom<S, T>(tokens: &mut Guard) -> Result<S, ParseError>
where
    T: TokenLike,
    Assert<{ size_of::<T>() == 0 }>: True,
    Assert<{ size_of::<S>() == 0 }>: True,
{
    tokens.advance_require::<T>()?;

    // SAFETY: we have checked the size of S and T with the constant generic expressions guaranteeing that they are ZSTs and therefore, zero patterns are valid for them.
    unsafe { Ok(::core::mem::zeroed()) }
}

macro_rules! atom {
    ($name:ident, $from:ident) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        #[doc = concat!("Parse helper for parsing the ", stringify!($name), " atom.")]
        pub struct $name;

        impl crate::Parse for $name {
            fn parse(tokens: &mut Guard) -> crate::ParseResult<Self> {
                atom::<Self, $from>(tokens)
            }
        }
    };
}

atom!(Assign, AToken);
atom!(Colon, ColToken);
atom!(Comma, CToken);
atom!(Dot, DToken);
atom!(LCurly, LCToken);
atom!(RCurly, RCToken);
atom!(LParen, LPToken);
atom!(RParen, RPToken);
atom!(Semi, SToken);
