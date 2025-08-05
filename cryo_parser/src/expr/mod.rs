//! Expressions.
//!
//! Expressions are components, that, when evaluated, produce a value.

use std::fmt::Debug;

use constructor::StructConstructor;
use cryo_lexer::{
    TokenType,
    atoms::{Equal, LCurly, LParen, RCurly, RParen},
    identifier::Identifier,
    stream::{Guard, StreamLike},
};
use cryo_span::Spanned;

use crate::{
    Parse, ParseError, Punctuated,
    atoms::Comma,
    expr::literal::Literal,
    ident::{ELSE, IF, Ident},
    path::Path,
    stmt::Stmt,
};

pub mod constructor;
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
    /// The accessor operator.
    Access,
}

impl Operator {
    fn parse_1<const CONSUME: bool>(tokens: &mut Guard) -> crate::ParseResult<Self> {
        let peek_token = tokens.peek()?;
        let op = match peek_token.t {
            TokenType::Plus(_) => Operator::Add,
            TokenType::Minus(_) => Operator::Sub,
            TokenType::Star(_) => Operator::Mul,
            TokenType::Slash(_) => Operator::Div,
            TokenType::Percent(_) => Operator::Rem,
            TokenType::Dot(_) => Operator::Access,
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
            _ => {
                return Err(crate::ParseError::TokenStreamError(
                    cryo_lexer::stream::TokenStreamError::IncorrectToken(*peek_token),
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
            Self::Access => 4,
        }
    }
}

/// A binary expression.
#[derive(Debug, PartialEq, Eq)]
pub struct BinaryExpr {
    /// The left-hand side of this expression.
    pub lhs: Box<Expr>,
    /// The operator.
    pub op: Spanned<Operator>,
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
    fn parse_1<const IF_COND: bool>(tokens: &mut Guard, min_prec: u8) -> crate::ParseResult<Self> {
        let mut lhs = tokens
            .with(BaseExpr::parse_1::<IF_COND>)
            .map(Self::BaseExpr)?;

        while let Ok(op) = tokens.with(Operator::parse_1::<false>) {
            if op.precedence() <= min_prec {
                break;
            }

            let op = tokens.spanning(Operator::parse).expect(
                "operator should parse correctly since operator token has already been consumed",
            );

            let rhs = tokens.with(|tokens| Expr::parse_1::<IF_COND>(tokens, op.precedence()))?;

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
        Self::parse_1::<true>(tokens, 0)
    }
}

/// A base, or simple expression.
#[derive(Debug, PartialEq, Eq)]
pub enum BaseExpr {
    /// A literal value.
    Lit(Literal),
    /// A binding usage.
    Path(Path),
    /// A block expression.
    BlockExpr(BlockExpr),
    /// A conditional expression.
    CondExpr(CondExpr),
    /// A function call.
    FnCall(FnCall),
    /// A struct constructor.
    StructConstructor(StructConstructor),
}

/// A block expression, i.e., a series of statements and a final optional tail expression surrounded by `{` and `}`.
#[derive(Debug, PartialEq, Eq)]
pub struct BlockExpr {
    /// The statements this expression contains.
    pub stmts: Box<[Spanned<Stmt>]>,
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
    pub if_block: Spanned<IfBlock>,
    /// The optional `else if` block(s).
    pub else_if_blocks: Spanned<Box<[Spanned<IfBlock>]>>,
    /// The else block, this will be evaluated if none of the conditions in the blocks return `true`.
    pub else_block: Option<Spanned<BlockExpr>>,
}

impl Parse for CondExpr {
    fn parse(tokens: &mut Guard) -> crate::ParseResult<Self> {
        let if_block = tokens.spanning(IfBlock::parse)?;
        let else_if_blocks = tokens
            .spanning(|tokens| {
                let mut vec = vec![];
                while let Ok(else_kw) = tokens.peek_require::<Identifier>()
                    && ELSE.with(|k| *k == else_kw.t.0)
                    && let Ok(if_kw) = tokens.peek_nth_require::<Identifier>(1)
                    && IF.with(|k| *k == if_kw.t.0)
                {
                    let else_start = tokens
                        .advance()
                        .expect("`else` identifier has already been confirmed")
                        .span
                        .start;

                    match tokens.spanning(IfBlock::parse) {
                        Ok(mut v) => {
                            v.span.start = else_start;
                            vec.push(v);
                        }
                        Err(_) => break,
                    }
                }

                Ok::<Vec<Spanned<IfBlock>>, ParseError>(vec)
            })?
            .map(|v| v.into_boxed_slice());

        let else_block = tokens.advance_require::<Identifier>().ok().and_then(|v| {
            ELSE.with(|k| *k == v.t.0)
                .then(|| tokens.spanning(BlockExpr::parse).ok())
                .flatten()
        });

        Ok(Self {
            if_block,
            else_if_blocks,
            else_block,
        })
    }
}

/// Represents a conditional block.
#[derive(PartialEq, Eq, Debug)]
pub struct IfBlock {
    /// The condition for this block.
    pub cond: Box<Spanned<Expr>>,
    /// The code to be executed.
    pub block: Spanned<BlockExpr>,
}

impl Parse for IfBlock {
    fn parse(tokens: &mut Guard) -> crate::ParseResult<Self> {
        tokens
            .with(Ident::parse)?
            .require(&IF)
            .map_err(|v| ParseError::MissingKw(v.sym.map(|_| IF.with(Clone::clone))))?;

        let cond = Box::new(tokens.spanning(|tokens| Expr::parse_1::<false>(tokens, 0))?);
        let block = tokens.spanning(BlockExpr::parse)?;

        Ok(Self { cond, block })
    }
}

/// A function call.
#[derive(Debug, PartialEq, Eq)]
pub struct FnCall {
    /// The function.
    pub func: Box<Spanned<Expr>>,
    /// The arguments passed.
    pub args: Spanned<Punctuated<Expr, Comma>>,
}

impl Parse for FnCall {
    fn parse(tokens: &mut Guard) -> crate::ParseResult<Self> {
        let func = Box::new(tokens.spanning(Expr::parse)?);
        tokens.advance_require::<LParen>()?;
        let args = tokens.spanning(Punctuated::parse)?;
        tokens.advance_require::<RParen>()?;

        Ok(Self { func, args })
    }
}

impl BaseExpr {
    fn parse_1<const IF_COND: bool>(
        tokens: &mut cryo_lexer::stream::Guard,
    ) -> crate::ParseResult<Self> {
        let mut result = tokens
            .with(Literal::parse)
            .map(Self::Lit)
            .or_else(|_| tokens.with(CondExpr::parse).map(Self::CondExpr))
            .or_else(|_| tokens.with(BlockExpr::parse).map(Self::BlockExpr));

        if IF_COND {
            result = result.or_else(|_| {
                tokens
                    .with(StructConstructor::parse)
                    .map(Self::StructConstructor)
            });
        }
        result = result.or_else(|_| {
            let ident = tokens.spanning(Path::parse);

            match ident {
                Ok(ident) => match tokens.peek_require::<LParen>() {
                    Ok(_) => {
                        tokens
                            .advance()
                            .expect("LParen token has already been confirmed");
                        let args = tokens.spanning(Punctuated::parse)?;
                        tokens.advance_require::<RParen>()?;
                        Ok(Self::FnCall(FnCall {
                            func: Box::new(ident.map(Self::Path).map(Expr::BaseExpr)),
                            args,
                        }))
                    }
                    _ => Ok(Self::Path(ident.t)),
                },
                Err(e) => Err(e),
            }
        });

        result
    }
}

impl Parse for BaseExpr {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        Self::parse_1::<false>(tokens)
    }
}

#[cfg(test)]
mod tests {
    use cryo_lexer::Symbol;
    use cryo_span::{Span, Spanned};

    use crate::{
        Punctuated,
        expr::{
            BaseExpr, BinaryExpr, CondExpr, Expr, IfBlock, Operator,
            literal::{IntegerLiteral, Literal},
        },
        ident::Ident,
        stmt::{Binding, Stmt, TypedIdent},
        test_util::assert_parse,
    };

    use super::{BlockExpr, FnCall, literal::StringLiteral};

    #[test]
    fn parse_add() {
        assert_parse(
            "5 + 6",
            Spanned::new(
                Expr::BinaryExpr(BinaryExpr {
                    lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                        Spanned::new(IntegerLiteral::Value(5), Span::new(0, 1)),
                    )))),
                    op: Spanned::new(super::Operator::Add, Span::new(2, 3)),
                    rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                        Spanned::new(IntegerLiteral::Value(6), Span::new(4, 5)),
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
                        Spanned::new(IntegerLiteral::Value(5), Span::new(0, 1)),
                    )))),
                    op: Spanned::new(Operator::Add, Span::new(2, 3)),
                    rhs: Box::new(Expr::BinaryExpr(BinaryExpr {
                        lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            Spanned::new(IntegerLiteral::Value(6), Span::new(4, 5)),
                        )))),
                        op: Spanned::new(Operator::Mul, Span::new(6, 7)),
                        rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            Spanned::new(IntegerLiteral::Value(2), Span::new(8, 9)),
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
                            Spanned::new(IntegerLiteral::Value(6), Span::new(0, 1)),
                        )))),
                        op: Spanned::new(super::Operator::Mul, Span::new(2, 3)),
                        rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            Spanned::new(IntegerLiteral::Value(2), Span::new(4, 5)),
                        )))),
                    })),
                    op: Spanned::new(Operator::Add, Span::new(6, 7)),
                    rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                        Spanned::new(IntegerLiteral::Value(5), Span::new(8, 9)),
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
                            Spanned::new(IntegerLiteral::Value(2), Span::new(0, 1)),
                        )))),
                        op: Spanned::new(Operator::Add, Span::new(2, 3)),
                        rhs: Box::new(Expr::BinaryExpr(BinaryExpr {
                            lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                Spanned::new(IntegerLiteral::Value(3), Span::new(4, 5)),
                            )))),
                            op: Spanned::new(Operator::Mul, Span::new(6, 7)),
                            rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                Spanned::new(IntegerLiteral::Value(4), Span::new(8, 9)),
                            )))),
                        })),
                    })),
                    op: Spanned::new(Operator::Eq, Span::new(10, 12)),
                    rhs: Box::new(Expr::BinaryExpr(BinaryExpr {
                        lhs: Box::new(Expr::BinaryExpr(BinaryExpr {
                            lhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                Spanned::new(IntegerLiteral::Value(6), Span::new(13, 14)),
                            )))),
                            op: Spanned::new(Operator::Div, Span::new(15, 16)),
                            rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                Spanned::new(IntegerLiteral::Value(2), Span::new(17, 18)),
                            )))),
                        })),
                        op: Spanned::new(Operator::Sub, Span::new(19, 20)),
                        rhs: Box::new(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                            Spanned::new(IntegerLiteral::Value(2), Span::new(21, 22)),
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
                        Spanned::new(
                            Stmt::ExprSemi(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                Spanned::new(IntegerLiteral::Value(5), Span::new(2, 3)),
                            )))),
                            Span::new(2, 4),
                        ),
                        Spanned::new(
                            Stmt::ExprSemi(Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                Spanned::new(IntegerLiteral::Value(7), Span::new(5, 6)),
                            )))),
                            Span::new(5, 7),
                        ),
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
                        Literal::IntegerLiteral(Spanned::new(
                            IntegerLiteral::Value(5),
                            Span::new(2, 3),
                        )),
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
                    stmts: Box::new([Spanned::new(
                        Stmt::Binding(Binding {
                            mutability: None,
                            ident: TypedIdent {
                                ident: Ident {
                                    sym: Spanned::new(Symbol::new("x"), Span::new(6, 7)),
                                    valid: true,
                                },
                                id_ty: Ident {
                                    sym: Spanned::new(Symbol::new("int"), Span::new(9, 12)),
                                    valid: true,
                                }
                                .into(),
                            },
                            expr: Expr::BaseExpr(BaseExpr::Lit(Literal::IntegerLiteral(
                                Spanned::new(IntegerLiteral::Value(5), Span::new(15, 16)),
                            ))),
                        }),
                        Span::new(2, 17),
                    )]),
                    tail: Some(Box::new(Expr::BaseExpr(BaseExpr::Path(
                        Ident {
                            sym: Spanned::new(Symbol::new("x"), Span::new(18, 19)),
                            valid: true,
                        }
                        .into(),
                    )))),
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
                    if_block: Spanned::new(
                        IfBlock {
                            cond: Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Path(
                                    Ident {
                                        sym: Spanned::new(Symbol::new("f"), Span::new(3, 4)),
                                        valid: true,
                                    }
                                    .into(),
                                )),
                                Span::new(3, 4),
                            )),
                            block: Spanned::new(
                                BlockExpr {
                                    stmts: Box::new([]),
                                    tail: None,
                                },
                                Span::new(5, 7),
                            ),
                        },
                        Span::new(0, 7),
                    ),
                    else_if_blocks: Spanned::new(Box::new([]), Span::new(7, 7)),
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
                    if_block: Spanned::new(
                        IfBlock {
                            cond: Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Path(
                                    Ident {
                                        sym: Spanned::new(Symbol::new("f"), Span::new(3, 4)),
                                        valid: true,
                                    }
                                    .into(),
                                )),
                                Span::new(3, 4),
                            )),
                            block: Spanned::new(
                                BlockExpr {
                                    stmts: Box::new([]),
                                    tail: None,
                                },
                                Span::new(5, 7),
                            ),
                        },
                        Span::new(0, 7),
                    ),
                    else_if_blocks: Spanned::new(
                        Box::new([Spanned::new(
                            IfBlock {
                                cond: Box::new(Spanned::new(
                                    Expr::BaseExpr(BaseExpr::Path(
                                        Ident {
                                            sym: Spanned::new(Symbol::new("g"), Span::new(16, 17)),
                                            valid: true,
                                        }
                                        .into(),
                                    )),
                                    Span::new(16, 17),
                                )),
                                block: Spanned::new(
                                    BlockExpr {
                                        stmts: Box::new([]),
                                        tail: None,
                                    },
                                    Span::new(18, 20),
                                ),
                            },
                            Span::new(8, 20),
                        )]),
                        Span::new(8, 20),
                    ),
                    else_block: None,
                })),
                Span::new(0, 20),
            ),
        );
    }

    #[test]
    fn parse_cond_expr_if_elseif_else() {
        assert_parse(
            "if f {} else if g {} else {}",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::CondExpr(CondExpr {
                    if_block: Spanned::new(
                        IfBlock {
                            cond: Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Path(
                                    Ident {
                                        sym: Spanned::new(Symbol::new("f"), Span::new(3, 4)),
                                        valid: true,
                                    }
                                    .into(),
                                )),
                                Span::new(3, 4),
                            )),
                            block: Spanned::new(
                                BlockExpr {
                                    stmts: Box::new([]),
                                    tail: None,
                                },
                                Span::new(5, 7),
                            ),
                        },
                        Span::new(0, 7),
                    ),
                    else_if_blocks: Spanned::new(
                        Box::new([Spanned::new(
                            IfBlock {
                                cond: Box::new(Spanned::new(
                                    Expr::BaseExpr(BaseExpr::Path(
                                        Ident {
                                            sym: Spanned::new(Symbol::new("g"), Span::new(16, 17)),
                                            valid: true,
                                        }
                                        .into(),
                                    )),
                                    Span::new(16, 17),
                                )),
                                block: Spanned::new(
                                    BlockExpr {
                                        stmts: Box::new([]),
                                        tail: None,
                                    },
                                    Span::new(18, 20),
                                ),
                            },
                            Span::new(8, 20),
                        )]),
                        Span::new(8, 20),
                    ),
                    else_block: Some(Spanned::new(
                        BlockExpr {
                            stmts: Box::new([]),
                            tail: None,
                        },
                        Span::new(26, 28),
                    )),
                })),
                Span::new(0, 28),
            ),
        )
    }

    #[test]
    fn parse_fn_call() {
        assert_parse(
            "printf(\"hello, world\")",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::FnCall(FnCall {
                    func: Box::new(Spanned::new(
                        Expr::BaseExpr(BaseExpr::Path(
                            Ident {
                                sym: Spanned::new(Symbol::new("printf"), Span::new(0, 6)),
                                valid: true,
                            }
                            .into(),
                        )),
                        Span::new(0, 6),
                    )),
                    args: Spanned::new(
                        crate::Punctuated {
                            inner: vec![],
                            last: Some(Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Lit(Literal::StringLiteral(
                                    Spanned::new(
                                        StringLiteral::Value(Box::from("hello, world")),
                                        Span::new(7, 21),
                                    ),
                                ))),
                                Span::new(7, 21),
                            ))),
                        },
                        Span::new(7, 21),
                    ),
                })),
                Span::new(0, 22),
            ),
        );
    }

    #[test]
    fn parse_field_access_call() {
        assert_parse(
            "a.iter()",
            Spanned::new(
                Expr::BinaryExpr(BinaryExpr {
                    lhs: Box::new(Expr::BaseExpr(BaseExpr::Path(
                        Ident {
                            sym: Spanned::new(Symbol::new("a"), Span::new(0, 1)),
                            valid: true,
                        }
                        .into(),
                    ))),
                    op: Spanned::new(Operator::Access, Span::new(1, 2)),
                    rhs: Box::new(Expr::BaseExpr(BaseExpr::FnCall(FnCall {
                        func: Box::new(Spanned::new(
                            Expr::BaseExpr(BaseExpr::Path(
                                Ident {
                                    sym: Spanned::new(Symbol::new("iter"), Span::new(2, 6)),
                                    valid: true,
                                }
                                .into(),
                            )),
                            Span::new(2, 6),
                        )),
                        args: Spanned::new(
                            Punctuated {
                                inner: vec![],
                                last: None,
                            },
                            Span::new(7, 7),
                        ),
                    }))),
                }),
                Span::new(0, 8),
            ),
        );
    }
}
