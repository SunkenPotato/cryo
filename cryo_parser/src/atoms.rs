//! Atoms.
//!
//! Atoms are components of code that are constant and only can be parsed from one specific atom token.

use cryo_lexer::{
    FromToken,
    atoms::{
        Assign as AToken, Colon as ColToken, Comma as CToken, Dot as DToken, Keyword,
        LCurly as LCToken, LParen as LPToken, RCurly as RCToken, RParen as RPToken, Semi as SToken,
    },
    stream::Guard,
};

struct Assert<const COND: bool>;

#[diagnostic::on_unimplemented(
    message = "it is undefined behaviour to pass a type to this function which is not a zero-sized type."
)]
trait True {}

impl True for Assert<true> {}

pub fn atom<S, T>(tokens: &mut Guard) -> Option<S>
where
    for<'s> T: FromToken<'s>,
    Assert<{ size_of::<T>() == 0 }>: True,
    Assert<{ size_of::<S>() == 0 }>: True,
{
    tokens.advance_require::<T>().ok()?;

    // SAFETY: we have checked the size of S and T with the constant generic expressions guaranteeing that they are ZSTs and therefore, zero patterns are valid for them.
    unsafe { Some(::core::mem::zeroed()) }
}

macro_rules! atom {
    ($name:ident, $from:ident) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub struct $name;

        impl $name {
            pub fn parse(tokens: &mut Guard) -> Option<Self> {
                atom::<Self, $from>(tokens)
            }
        }
    };

    ($name:ident, $e:ident::$var:ident) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub struct $name;

        impl $name {
            pub fn parse(tokens: &mut Guard) -> Option<Self> {
                if let $e::$var = **tokens.advance_require::<$e>().ok()? {
                    return Some(Self);
                } else {
                    return None;
                }
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

atom!(Let, Keyword::Let);
atom!(Mut, Keyword::Mut);
atom!(If, Keyword::If);
atom!(Else, Keyword::Else);
atom!(Fun, Keyword::Fun);
atom!(Struct, Keyword::Struct);
