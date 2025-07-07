//! Block expressions.

use cryo_parser_proc_macro::Parse;

use crate::{
    atoms::{LCurly, RCurly},
    expr::Expr,
    stmt::Stmt,
};

/// A block expression.
#[derive(Parse, PartialEq, Debug)]
pub struct Block {
    /// The starting delimiter.
    pub l_curly: LCurly,
    /// The inner statements.
    pub stmts: Vec<Stmt>,
    /// The tail/return expression.
    pub tail: Option<Box<Expr>>,
    /// The ending delimiter.
    pub r_curly: RCurly,
}
