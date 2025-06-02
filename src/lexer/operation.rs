use std::str::FromStr;

use crate::span::Span;

use super::{
    Lex,
    tokens::{Token, TokenType},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

impl FromStr for Operation {
    type Err = Span;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "/" => Self::Div,
            "%" => Self::Rem,
            _ => return Err(Span::ONE),
        })
    }
}

impl Lex for Operation {
    fn lex(input: &str) -> Result<(super::tokens::Token, &str), crate::span::Span> {
        let op = Self::from_str(&input[..1])?;
        let token = Token::new(TokenType::Operation(op), Span::ONE);

        Ok((token, &input[1..]))
    }
}
