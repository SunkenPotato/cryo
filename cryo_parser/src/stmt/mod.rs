//! Statements.
//!
//! Statements are components of code that do not return a value - in contrast to expressions.

pub mod binding;

#[cfg(test)]
mod tests;

use cryo_parser_proc_macro::Parse;

use crate::{atoms::Semi, expr::Expr, stmt::binding::Binding};

/// A statement as an expression. This is simply an expression followed by a semicolon.
#[derive(Parse, PartialEq, Debug)]
pub struct ExprStmt {
    /// The expression.
    pub expr: Expr,
    /// The semicolon.
    pub semi: Semi,
}

/// Statements.
///
/// View the module-level docs for more information.
#[derive(Parse, PartialEq, Debug)]
pub enum Stmt {
    /// An expression statement.
    ExprStmt(ExprStmt),
    /// A binding.
    Binding(Binding),
}
