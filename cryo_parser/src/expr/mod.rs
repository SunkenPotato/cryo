pub mod literal;

use cryo_parser_proc_macro::Parse;

// #[derive(Parse)]
pub enum Expr {
    // MathExpr(Box<MathExpr>),
    ReducedExpr(ReducedExpr),
}

// #[derive(Parse)]
pub enum ReducedExpr {}
