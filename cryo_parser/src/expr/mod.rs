//! Expressions.
//!
//! Expressions are components, that, when evaluated, produce a value.

use std::fmt::Debug;

use cryo_lexer::{
    TokenType,
    atoms::{Equal, LCurly, RCurly},
    identifier::Identifier,
    stream::{Guard, StreamLike},
};

use crate::{
    Parse, ParseError,
    expr::literal::Literal,
    ident::{ELSE, IF, Ident},
    stmt::Stmt,
};

pub mod literal;

/// Binary operators.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Operator {
    /// The addition operator.
    Add,
    /// The subtraction operator.
    Sub,
    /// The multiplication operator.
    Mul,
    /// The division operator.
    Div,
    /// The remainder operator (`%`).
    Rem,
    /// The equality comparison operator.
    Eq,
    /// The inequality comparison operator.
    NotEq,
}

impl Operator {
    fn parse_1<const CONSUME: bool>(tokens: &mut Guard) -> crate::ParseResult<Self> {
        let op = match tokens.peek()?.t {
            TokenType::Plus(_) => Operator::Add,
            TokenType::Minus(_) => Operator::Sub,
            TokenType::Star(_) => Operator::Mul,
            TokenType::Slash(_) => Operator::Div,
            TokenType::Percent(_) => Operator::Rem,
            TokenType::Equal(_) => {
                tokens.peek_require::<Equal>()?;
                if CONSUME {
                    tokens
                        .advance()
                        .expect("stream should not be empty since it has been checked");
                }
                Operator::Eq
            }
            TokenType::Bang(_) => {
                tokens.peek_require::<Equal>()?;
                if CONSUME {
                    tokens
                        .advance()
                        .expect("stream should not be empty since it has been checked");
                }
                Operator::NotEq
            }
            t => {
                return Err(crate::ParseError::TokenStreamError(
                    cryo_lexer::stream::TokenStreamError::IncorrectToken(t),
                ));
            }
        };
        if CONSUME {
            tokens
                .advance()
                .expect("stream should not be empty since it has been checked");
        }
        Ok(op)
    }
}

impl Parse for Operator {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        Self::parse_1::<true>(tokens)
    }
}

impl Operator {
    /// Return the precedence of this operator.
    pub const fn precedence(&self) -> u8 {
        match self {
            Self::NotEq | Self::Eq => 1,
            Self::Add | Self::Sub => 2,
            Self::Mul | Self::Div | Self::Rem => 3,
        }
    }
}

/// A binary expression.
#[derive(Debug, PartialEq, Eq)]
pub struct BinaryExpr {
    /// The left-hand side of this expression.
    pub lhs: Box<Expr>,
    /// The operator.
    pub op: Operator,
    /// The right-hand side of this expression.
    pub rhs: Box<Expr>,
}

/// An expression.
#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    /// A simple expression.
    BaseExpr(BaseExpr),
    /// A binary expression.
    BinaryExpr(BinaryExpr),
}

impl Expr {
    fn parse_1(tokens: &mut Guard, min_prec: u8) -> crate::ParseResult<Self> {
        let mut lhs = tokens.with(BaseExpr::parse).map(Self::BaseExpr)?;

        while let Ok(op) = tokens.with(Operator::parse_1::<false>) {
            if op.precedence() <= min_prec {
                break;
            }

            tokens.with(Operator::parse).expect(
                "operator should parse correctly since operator token has already been consumed",
            );

            let rhs = tokens.with(|tokens| Expr::parse_1(tokens, op.precedence()))?;

            lhs = Self::BinaryExpr(BinaryExpr {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            })
        }

        Ok(lhs)
    }
}

impl Parse for Expr {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        Self::parse_1(tokens, 0)
    }
}

/// A base, or simple expression.
#[derive(Debug, PartialEq, Eq)]
pub enum BaseExpr {
    /// A literal value.
    Lit(Literal),
    /// A binding usage.
    BindingUsage(Ident),
    /// A block expression.
    BlockExpr(BlockExpr),
    /// A conditional expression.
    CondExpr(CondExpr),
}

/// A block expression, i.e., a series of statements and a final optional tail expression surrounded by `{` and `}`.
#[derive(Debug, PartialEq, Eq)]
pub struct BlockExpr {
    /// The statements this expression contains.
    pub stmts: Box<[Stmt]>,
    /// The final expression, which this evaluates to.
    pub tail: Option<Box<Expr>>,
}

impl Parse for BlockExpr {
    fn parse(tokens: &mut Guard) -> crate::ParseResult<Self> {
        tokens.advance_require::<LCurly>()?;
        let stmts = tokens.with(Vec::parse).unwrap().into_boxed_slice();
        let tail = tokens.with(Expr::parse).map(Box::new).ok();
        tokens.advance_require::<RCurly>()?;

        Ok(Self { stmts, tail })
    }
}

/// A conditional expression, where code is only executed if a certain condition is given.
#[derive(PartialEq, Eq, Debug)]
pub struct CondExpr {
    /// The main `if` block, this is required.
    pub if_block: IfBlock,
    /// The optional `else if` block(s).
    pub else_if_blocks: Box<[IfBlock]>,
    /// The else block, this will be evaluated if none of the conditions in the blocks return `true`.
    pub else_block: Option<BlockExpr>,
}

impl Parse for CondExpr {
    fn parse(tokens: &mut Guard) -> crate::ParseResult<Self> {
        let if_block = tokens.with(IfBlock::parse)?;
        let mut else_if_blocks = vec![];
        let mut else_block = None;

        while let Ok(ident) = tokens.with(Ident::parse)
            && let Ok(_) = ident.require(&ELSE)
        {
            if let Ok(possible_if_token) = tokens.peek_require::<Identifier>()
                && let Ok(_) = Ident::from(*possible_if_token.t).require(&IF)
            {
                let Ok(else_if_block) = tokens.with(IfBlock::parse) else {
                    break;
                };

                else_if_blocks.push(else_if_block);
            } else {
                let Ok(block_expr) = tokens.with(BlockExpr::parse) else {
                    break;
                };
                else_block.replace(block_expr);
                break;
            }
        }

        Ok(Self {
            if_block,
            else_if_blocks: else_if_blocks.into_boxed_slice(),
            else_block,
        })
    }
}

/// Represents a conditional block.
#[derive(PartialEq, Eq, Debug)]
pub struct IfBlock {
    /// The condition for this block.
    pub cond: Box<Expr>,
    /// The code to be executed.
    pub block: BlockExpr,
}

impl Parse for IfBlock {
    fn parse(tokens: &mut Guard) -> crate::ParseResult<Self> {
        tokens
            .with(Ident::parse)?
            .require(&IF)
            .map_err(|_| ParseError::MissingKw(IF.with(Clone::clone)))?;

        let cond = Box::new(tokens.with(Expr::parse)?);
        let block = tokens.with(BlockExpr::parse)?;

        Ok(Self { cond, block })
    }
}

impl Parse for BaseExpr {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        tokens
            .with(Literal::parse)
            .map(Self::Lit)
            .or_else(|_| tokens.with(CondExpr::parse).map(Self::CondExpr))
            .or_else(|_| tokens.with(BlockExpr::parse).map(Self::BlockExpr))
            .or_else(|_| tokens.with(Ident::parse).map(Self::BindingUsage))
    }
}

#[cfg(test)]
mod tests {
    #![allow(missing_docs)]
    use cryo_lexer::Symbol;
    use cryo_span::{Span, Spanned};

    use crate::{
        expr::{
            BaseExpr, BinaryExpr, CondExpr, Expr, IfBlock, Operator,
            literal::{IntegerLiteral, Literal},
        },
        ident::Ident,
        stmt::{Binding, Stmt, TypedIdent},
        test_util::assert_parse,
    };

    use super::BlockExpr;

    #[test]
    fn parse_add() {
        assert_parse(
            "5 + 6",
            Spanned::new(
                Expr::BinaryExpr(BinaryExpr {
                    lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                        IntegerLiteral::Value(5),
                    )))),
                    op: super::Operator::Add,
                    rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                        IntegerLiteral::Value(6),
                    )))),
                }),
                Span::new(0, 5),
            ),
        );
    }

    #[test]
    fn parse_add_mul() {
        assert_parse(
            // (5) + (6 * 2)
            "5 + 6 * 2",
            Spanned::new(
                Expr::BinaryExpr(BinaryExpr {
                    lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                        IntegerLiteral::Value(5),
                    )))),
                    op: Operator::Add,
                    rhs: Box::new(Expr::BinaryExpr(BinaryExpr {
                        lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(6),
                        )))),
                        op: Operator::Mul,
                        rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(2),
                        )))),
                    })),
                }),
                Span::new(0, 9),
            ),
        );
    }

    #[test]
    fn parse_mul_add() {
        assert_parse(
            //((6 * 2)+(5))
            //((6) * (2 + 5))
            "6 * 2 + 5",
            Spanned::new(
                Expr::BinaryExpr(BinaryExpr {
                    lhs: Box::new(Expr::BinaryExpr(BinaryExpr {
                        lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(6),
                        )))),
                        op: super::Operator::Mul,
                        rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(2),
                        )))),
                    })),
                    op: Operator::Add,
                    rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                        IntegerLiteral::Value(5),
                    )))),
                }),
                Span::new(0, 9),
            ),
        );
    }

    #[test]
    fn parse_complex_binary_expr() {
        assert_parse(
            "2 + 3 * 4 == 6 / 2 - 2",
            Spanned::new(
                Expr::BinaryExpr(BinaryExpr {
                    lhs: Box::new(Expr::BinaryExpr(BinaryExpr {
                        lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(2),
                        )))),
                        op: Operator::Add,
                        rhs: Box::new(Expr::BinaryExpr(BinaryExpr {
                            lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                IntegerLiteral::Value(3),
                            )))),
                            op: Operator::Mul,
                            rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                IntegerLiteral::Value(4),
                            )))),
                        })),
                    })),
                    op: Operator::Eq,
                    rhs: Box::new(Expr::BinaryExpr(BinaryExpr {
                        lhs: Box::new(Expr::BinaryExpr(BinaryExpr {
                            lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                IntegerLiteral::Value(6),
                            )))),
                            op: Operator::Div,
                            rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                IntegerLiteral::Value(2),
                            )))),
                        })),
                        op: Operator::Sub,
                        rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(2),
                        )))),
                    })),
                }),
                Span::new(0, 22),
            ),
        );
    }

    #[test]
    fn parse_stmt_only_block() {
        assert_parse(
            "{ 5; 7; }",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::BlockExpr(BlockExpr {
                    stmts: Box::new([
                        Stmt::ExprSemi(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(5),
                        )))),
                        Stmt::ExprSemi(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(7),
                        )))),
                    ]),
                    tail: None,
                })),
                Span::new(0, 9),
            ),
        );
    }

    #[test]
    fn parse_tail_only_block() {
        assert_parse(
            "{ 5 }",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::BlockExpr(BlockExpr {
                    stmts: Box::new([]),
                    tail: Some(Box::new(Expr::BaseExpr(BaseExpr::Lit(
                        Literal::IntegerLiteral(IntegerLiteral::Value(5)),
                    )))),
                })),
                Span::new(0, 5),
            ),
        )
    }

    #[test]
    fn parse_stmts_tail_block() {
        assert_parse(
            "{ let x: int = 5; x }",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::BlockExpr(BlockExpr {
                    stmts: Box::new([Stmt::Binding(Binding {
                        mutability: None,
                        ident: TypedIdent {
                            ident: Ident {
                                sym: Symbol::new("x"),
                                valid: true,
                            },
                            id_ty: Ident {
                                sym: Symbol::new("int"),
                                valid: true,
                            },
                        },
                        expr: Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            IntegerLiteral::Value(5),
                        ))),
                    })]),
                    tail: Some(Box::new(Expr::BaseExpr(BaseExpr::BindingUsage(Ident {
                        sym: Symbol::new("x"),
                        valid: true,
                    })))),
                })),
                Span::new(0, 21),
            ),
        )
    }

    #[test]
    fn parse_cond_expr_if() {
        assert_parse(
            "if f {}",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::CondExpr(CondExpr {
                    if_block: IfBlock {
                        cond: Box::new(Expr::BaseExpr(BaseExpr::BindingUsage(Ident {
                            sym: Symbol::new("f"),
                            valid: true,
                        }))),
                        block: BlockExpr {
                            stmts: Box::new([]),
                            tail: None,
                        },
                    },
                    else_if_blocks: Box::new([]),
                    else_block: None,
                })),
                Span::new(0, 7),
            ),
        );
    }

    #[test]
    fn parse_cond_expr_if_else_if() {
        assert_parse(
            "if f {} else if g {}",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::CondExpr(CondExpr {
                    if_block: IfBlock {
                        cond: Box::new(Expr::BaseExpr(BaseExpr::BindingUsage(Ident {
                            sym: Symbol::new("f"),
                            valid: true,
                        }))),
                        block: BlockExpr {
                            stmts: Box::new([]),
                            tail: None,
                        },
                    },
                    else_if_blocks: Box::new([IfBlock {
                        cond: Box::new(Expr::BaseExpr(BaseExpr::BindingUsage(Ident {
                            sym: Symbol::new("g"),
                            valid: true,
                        }))),
                        block: BlockExpr {
                            stmts: Box::new([]),
                            tail: None,
                        },
                    }]),
                    else_block: None,
                })),
                Span::new(0, 20),
            ),
        );
    }

    #[test]
    fn parse_cond_expr_if_else_if_else() {
        assert_parse(
            "if f {} else if g {} else {}",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::CondExpr(CondExpr {
                    if_block: IfBlock {
                        cond: Box::new(Expr::BaseExpr(BaseExpr::BindingUsage(Ident {
                            sym: Symbol::new("f"),
                            valid: true,
                        }))),
                        block: BlockExpr {
                            stmts: Box::new([]),
                            tail: None,
                        },
                    },
                    else_if_blocks: Box::new([IfBlock {
                        cond: Box::new(Expr::BaseExpr(BaseExpr::BindingUsage(Ident {
                            sym: Symbol::new("g"),
                            valid: true,
                        }))),
                        block: BlockExpr {
                            stmts: Box::new([]),
                            tail: None,
                        },
                    }]),
                    else_block: Some(BlockExpr {
                        stmts: Box::new([]),
                        tail: None,
                    }),
                })),
                Span::new(0, 28),
            ),
        )
    }
}
