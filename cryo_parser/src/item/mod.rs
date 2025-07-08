//! Items.
//!
//! Items are top-level constructs such as functions which declare components for the entire program (or scope) to access.

#[cfg(test)]
mod tests;

pub mod struct_item;

// #[derive(Parse)]
// pub struct VisItem {
//      vis: Vis,
//      item: Item
// }
//
// TODO when modules are added

use cryo_parser_proc_macro::Parse;

use crate::{atoms::Colon, ident::Ident, item::struct_item::StructItem};

/// A typed binding.
///
/// Consists of a binding identifier, a colon, and a type identifier.
#[derive(Parse, PartialEq, Debug, Clone, Copy)]
pub struct TypedBinding {
    /// The identifier.
    pub ident: Ident,
    /// The separating colon.
    pub colon: Colon,
    /// The type.
    pub ty: Ident,
}

/// An item.
#[derive(Parse, PartialEq, Debug)]
pub enum Item {
    /// A struct item.
    Struct(StructItem),
}
