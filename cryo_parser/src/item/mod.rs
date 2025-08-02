//! Items.
//! Items are definitions for structures like functions.

use cryo_lexer::stream::StreamLike;
use fn_def::FnDef;
use struct_def::{EnumDef, StructDef};

use crate::{Parse, ident::Ident};

pub mod fn_def;
pub mod struct_def;

/// Placeholder for types.
pub type Ty = Ident;

/// An item.
#[derive(Debug, PartialEq, Eq)]
pub enum Item {
    /// A function definition.
    FnDef(FnDef),
    /// A structure definition.
    StructDef(StructDef),
    /// An enum definition.
    EnumDef(EnumDef),
}

impl Parse for Item {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        tokens
            .with(FnDef::parse)
            .map(Self::FnDef)
            .or_else(|_| tokens.with(StructDef::parse).map(Self::StructDef))
            .or_else(|_| tokens.with(EnumDef::parse).map(Self::EnumDef))
    }
}
