pub mod literal;
#[cfg(test)]
mod tests;

use cryo_parser_proc_macro::Parse;

use crate::expr::literal::Literal;

// #[derive(Parse)]
pub enum Expr {
    // MathExpr(Box<MathExpr>),
    ReducedExpr(ReducedExpr),
}

// #[derive(Parse)]
pub enum ReducedExpr {
    Literal(Literal),
}
