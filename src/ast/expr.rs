use super::{Parse, extract_whitespace, literal::Literal};

pub enum Expr {
    Literal(Literal),
    MathExpr(Box<MathExpr>),
}

impl Parse for Expr {
    fn parse(input: &str) -> Result<(Self, &str), Self::Error> {}
}

pub struct MathExpr {
    rhs: Expr,
    op: Op,
    lhs: Expr,
}

impl Parse for MathExpr {
    fn parse(input: &str) -> Result<(Self, &str), Self::Error> {
        let (rhs, rest) = Expr::parse(input)?;
        let (op, rest) = Op::parse(rest)?;
        let (lhs, rest) = Expr::parse(input)?;

        let s = Self { lhs, op, rhs };

        Ok((s, rest))
    }
}

#[derive(Debug)]
pub struct OpParseError;

pub enum Op {
    Add,
    Sub,
    Div,
    Mul,
    Mod,
}

impl Parse for Op {
    type Error = OpParseError;

    fn parse(input: &str) -> Result<(Self, &str), Self::Error> {
        let input = extract_whitespace(input);

        let op = match input.chars().next().ok_or(OpParseError)? {
            '+' => Op::Add,
            '-' => Op::Sub,
            '/' => Op::Div,
            '*' => Op::Mul,
            '%' => Op::Mod,
            _ => return Err(OpParseError),
        };

        Ok((op, &input[1..]))
    }
}
