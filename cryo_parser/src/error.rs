use ParseResult::*;

pub trait AstNode {
    fn is_err(&self) -> bool;
}
