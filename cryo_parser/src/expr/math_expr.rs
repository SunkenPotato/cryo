//! Arithmetic expressions.
//!
//! These are defined as consisting of an [`Expr`] evaluating to an integer (LHS), an [`Operator`], and another `Expr` evaluating to an integer (RHS).

use cryo_lexer::operation::Operation as OpToken;

use crate::{
    Parse, Spanned, SpecToken,
    error::ParseError,
    expr::{Expr, ReducedExpr},
};

/// An arithmetic expression.
///
/// An arithmetic expression must consist of a left hand expression, an operator, and a right hand expression.
///
/// These can be nested and act as a tree.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct MathExpr {
    /// The left hand side of the expression.
    pub lhs: Expr,
    /// The operator.
    pub op: Operator,
    /// The right hand side of the expression.
    pub rhs: Expr,
}

impl Parse for MathExpr {
    fn parse<'t>(
        stream: &mut crate::TokenStreamGuard<'t>,
    ) -> Result<Spanned<Self>, Box<dyn ParseError>> {
        let (lhs, lspan) = match ReducedExpr::parse(stream) {
            Ok(v) => v.map(Expr::ReducedExpr),
            Err(e) => return Err(e),
        }
        .as_tuple();
        println!("{stream:?}");

        let (op, op_span) = Operator::parse(stream).unwrap().as_tuple();
        let (rhs, rspan) = Expr::parse(stream).unwrap().as_tuple();

        let span = lspan.extend(op_span).extend(rspan);

        Ok(Spanned::new(Self { lhs, op, rhs }, span))
    }
}

/// Arithmetic operators.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Operator {
    /// The addition operator (`+`).
    Add,
    /// The subtraction operator (`-`).
    Sub,
    /// The multiplication operator (`*`).
    Mul,
    /// The division operator (`/`).
    Div,
    /// The remainder operator (`%`).
    Rem,
}

impl From<OpToken> for Operator {
    fn from(value: OpToken) -> Self {
        match value {
            OpToken::Add => Self::Add,
            OpToken::Sub => Self::Sub,
            OpToken::Mul => Self::Mul,
            OpToken::Div => Self::Div,
            OpToken::Rem => Self::Rem,
        }
    }
}

impl Parse for Operator {
    fn parse<'t>(
        stream: &mut crate::TokenStreamGuard<'t>,
    ) -> Result<Spanned<Self>, Box<dyn ParseError>> {
        let SpecToken { token, span } = stream.advance_require::<OpToken>()?;
        let token = *token;

        Ok(Spanned::new(token.into(), span))
    }
}

#[cfg(test)]
mod tests {
    use cryo_lexer::{identifier::Identifier, t};

    use crate::{
        TokenStream,
        expr::{
            Expr, ReducedExpr,
            binding_ref::BindingRef,
            literal::{Literal, NumberLiteral},
            math_expr::{MathExpr, Operator},
        },
        parse_assert,
    };

    #[test]
    fn parse_op() {
        let mut ts = TokenStream::new([t![op+]]);

        parse_assert(&mut ts, Operator::Add);
    }

    #[test]
    fn parse_math_expr() {
        let mut ts = TokenStream::new([t![nl 5], t![op+], t![nl 5]]);

        parse_assert(
            &mut ts,
            MathExpr {
                lhs: Expr::ReducedExpr(ReducedExpr::Literal(Literal::NumberLiteral(
                    NumberLiteral(5),
                ))),
                op: Operator::Add,
                rhs: Expr::ReducedExpr(ReducedExpr::Literal(Literal::NumberLiteral(
                    NumberLiteral(5),
                ))),
            },
        )
    }

    #[test]
    fn parse_math_expr_binding_ref() {
        let mut ts = TokenStream::new([t![nl 5], t![op+], t![id "y"]]);

        parse_assert(
            &mut ts,
            MathExpr {
                lhs: Expr::ReducedExpr(ReducedExpr::Literal(Literal::NumberLiteral(
                    NumberLiteral(5),
                ))),
                op: Operator::Add,
                rhs: Expr::ReducedExpr(ReducedExpr::BindingRef(BindingRef(Identifier::new("y")))),
            },
        )
    }
}
