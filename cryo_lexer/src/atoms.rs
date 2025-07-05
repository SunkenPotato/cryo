//! Atoms.
//!
//! Atoms are tokens which are constant and only may be parsed from one specific input.

use crate::atom;

/// A semicolon token (`;`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Semi;

/// An assign token (`=`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Assign;

atom!(Semi, ";");
atom!(Assign, "=");
atom! {
    /// A keyword.
    ///
    /// Keywords are reserved tokens used to indicate behaviour or declare constructs.
    pub enum Keyword {
        /// The `let` keyword. Used to declare bindings.
        #("let")
        Let,
        /// The `mut` keyword. Used to declare mutability on bindings.
        #("mut")
        Mut
    }
}

atom! {
    /// Arithmetic operators.
    pub enum Operator {
        /// The addition operator (`+`).
        #("+")
        Add,
        /// The subtraction operator (`-`).
        #("-")
        Sub,
        /// The multiplication operator (`*`).
        #("*")
        Mul,
        /// The division operator (`/`).
        #("/")
        Div,
        /// The remainder operator (`%`).
        #("%")
        Rem
    }
}
