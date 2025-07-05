pub mod literal;
pub mod math_expr;
#[cfg(test)]
mod tests;

use cryo_parser_proc_macro::Parse;

use crate::expr::{literal::Literal, math_expr::MathExpr};

#[derive(Parse, Debug, PartialEq)]
pub enum Expr {
    MathExpr(Box<MathExpr>),
    ReducedExpr(ReducedExpr),
}

#[derive(Parse, Debug, PartialEq)]
pub enum ReducedExpr {
    Literal(Literal),
}
