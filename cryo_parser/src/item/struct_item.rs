//! Struct items.

use cryo_parser_proc_macro::Parse;

use crate::{
    atoms::{Comma, LCurly, RCurly, Struct},
    ident::Ident,
    item::TypedBinding,
    parser::Punct,
};

/// A struct item.
#[derive(Parse, PartialEq, Debug)]
pub struct StructDef {
    /// The struct keyword.
    pub kw_struct: Struct,
    /// The identifier.
    pub ident: Ident,
    /// The body of the struct.
    pub body: StructBody,
}

/// A struct body.
#[derive(Parse, PartialEq, Debug)]
pub struct StructBody {
    /// The opening delimiter.
    pub l_paren: LCurly,
    /// The fields of this struct.
    pub fields: Punct<TypedBinding, Comma>,
    /// The closing delimiter.
    pub r_paren: RCurly,
}
