//! Function definitions.

use cryo_parser_proc_macro::Parse;

use crate::{
    atoms::{Colon, Comma, Fun, LParen, RParen},
    expr::block::Block,
    ident::Ident,
    item::TypedBinding,
    parser::Punct,
};

/// A function definition.
#[derive(Parse, PartialEq, Debug)]
pub struct FnDef {
    /// The `fun` keyword.
    pub fn_kw: Fun,
    /// The identifier of this function/
    pub ident: Ident,
    /// The opening parenthesis for the arguments.
    pub l_arg_paren: LParen,
    /// The arguments.
    pub args: Punct<TypedBinding, Comma>,
    /// The right argument parenthesis.
    pub r_arg_paren: RParen,
    /// The return type.
    pub ret_ty: Option<FnRetTy>,
    /// The function body.
    pub body: Block,
}

/// A function return specification.
#[derive(Parse, PartialEq, Debug)]
pub struct FnRetTy {
    /// The colon before the return type.
    pub colon: Colon,
    /// The actual return type.
    pub ret_ty: Ident,
}
