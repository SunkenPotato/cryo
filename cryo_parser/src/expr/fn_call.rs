//! Function calls.

use cryo_parser_proc_macro::Parse;

use crate::{
    atoms::{Comma, LParen, RParen},
    expr::Expr,
    ident::Ident,
    parser::Punct,
};

/// A function call.
#[derive(Parse, PartialEq, Debug)]
pub struct FnCall {
    /// The identifier of the function to be called.
    pub fn_ident: Ident,
    /// The opening parenthesis for the arguments.
    pub l_arg_paren: LParen,
    /// The arguments.
    pub args: Punct<Expr, Comma>,
    /// The closing parenthesis for the arguments.
    pub r_arg_paren: RParen,
}
