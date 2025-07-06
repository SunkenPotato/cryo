//! Bindings.
//!
//! Bindings are used to store data with an identifier and access it later.

use cryo_parser_proc_macro::Parse;

use crate::{
    atoms::{Assign, Let, Mut, Semi},
    expr::{Expr, binding_ref::BindingRef},
};

/// A binding.
#[derive(Parse, PartialEq, Debug)]
pub struct Binding {
    /// The let keyword.
    pub let_kw: Let,
    /// Optional mutability keyword.
    pub mut_kw: Option<Mut>,
    /// The name of the binding.
    pub binding_name: BindingRef,
    /// The assign token (`=`).
    pub assign: Assign,
    /// The expression of this binding.
    pub rhs: Expr,
    /// The semicolon terminating this binding.
    pub semi: Semi,
}
