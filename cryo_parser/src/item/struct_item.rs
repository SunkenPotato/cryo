//! Struct items.

use cryo_parser_proc_macro::Parse;

use crate::{
    atoms::{Comma, LCurly, LParen, RCurly, RParen, Semi, Struct},
    ident::Ident,
    item::TypedBinding,
    parser::Punct,
};

/// A struct item.
#[derive(Parse, PartialEq, Debug)]
pub struct StructItem {
    /// The struct keyword.
    pub kw_struct: Struct,
    /// The identifier.
    pub ident: Ident,
    /// The body of the struct.
    pub body: StructBody,
}

/// A struct body.
#[derive(Parse, PartialEq, Debug)]
pub enum StructBody {
    /// An empty body.
    Empty(Semi),
    /// A tuple body (nameless fields).
    Tuple(StructTupleBody),
    /// A named body.
    Named(NamedStructBody),
}

/// A struct tuple body.
#[derive(Parse, PartialEq, Debug)]
pub struct StructTupleBody {
    /// The starting delimiter.
    pub l_paren: LParen,
    /// The fields of this struct.
    pub fields: Punct<Ident, Comma>,
    /// The ending delimiter.
    pub r_paren: RParen,
    /// The semicolon.
    pub semi: Semi,
}

/// A named tuple body.
#[derive(Parse, PartialEq, Debug)]
pub struct NamedStructBody {
    /// The starting delimiter.
    pub l_paren: LCurly,
    /// The fields of this struct.
    pub fields: Punct<TypedBinding, Comma>,
    /// The ending delimiter.
    pub r_paren: RCurly,
}
