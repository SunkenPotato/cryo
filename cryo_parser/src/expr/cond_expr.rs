//! Conditional expressions.
//!
//! This module defines conditional expressions, also known as if-expressions, which are only executed if a given condition is true.

use cryo_parser_proc_macro::Parse;

use crate::{
    atoms::{Else, If},
    expr::{Expr, block::Block},
};

/// A conditional expression.
#[derive(Parse, PartialEq, Debug)]
pub struct IfExpr {
    /// The conditional block.
    pub if_block: IfBlock,
    /// Blocks to try if the initial condition does not hold.
    pub else_if_blocks: Vec<ElseIfBlock>,
    /// The block to execute if none of the blocks could be executed.
    pub else_block: Option<ElseBlock>,
}

/// A conditional block.
#[derive(Parse, PartialEq, Debug)]
pub struct IfBlock {
    /// The `if` keyword.
    pub if_kw: If,
    /// The condition that needs to be fulfilled.
    pub condition: Box<Expr>,
    /// The code to be executed if the condition holds.
    pub if_block: Block,
}

/// A fallback `if` block.
#[derive(Parse, PartialEq, Debug)]
pub struct ElseIfBlock {
    /// The `else` keyword.
    pub else_kw: Else,
    /// The `if` block.
    pub if_block: IfBlock,
}

/// The fallback block.
#[derive(Parse, PartialEq, Debug)]
pub struct ElseBlock {
    /// The `else` keyword.
    pub else_kw: Else,
    /// The code to be executed.
    pub block: Block,
}
