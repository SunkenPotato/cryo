//! Expressions.

use cryo_lexer::{TokenKind, stream::StreamLike};
use cryo_parser_proc_macro::IsFail;
use cryo_span::Spanned;

use crate::{
    CommaSeparated, IsFail, OneOrMany, Parse, ParseError, ParseErrorKind, Path,
    expr::literal::Literal,
    ident::{ELSE, IF, Ident},
    stmt::Stmt,
};

/// An operator for a binary operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq, IsFail)]
#[fail = false]
pub enum BinaryOp {
    /// The addition operator `+`.
    Add,
    /// The subtraction operator `-`.
    Sub,
    /// The division operator `/`.
    Div,
    /// The multiplication operator `*`.
    Mul,
    /// The remainder operator `%`.
    Rem,
    /// The dot operator `.`.s
    Dot,
    /// The equality operator `==`.
    Eq,
    /// The inequality operator `!=`.
    NotEq,
}

impl BinaryOp {
    const ACCEPTED_TOKENS: &[TokenKind] = &[
        TokenKind::Plus,
        TokenKind::Minus,
        TokenKind::Slash,
        TokenKind::Star,
        TokenKind::Percent,
        TokenKind::Dot,
        TokenKind::Equal,
        TokenKind::Bang,
    ];

    /// The precedence of this operator.
    pub const fn precedence(self) -> u8 {
        match self {
            Self::Eq | Self::NotEq => 1,
            Self::Add | Self::Sub => 2,
            Self::Mul | Self::Div | Self::Rem => 3,
            Self::Dot => 4,
        }
    }
}

/// A binary expression.
#[derive(Clone, Debug, PartialEq, Eq, IsFail)]
#[fail = false]
pub struct BinaryExpr {
    /// The left-hand side of this expression.
    pub lhs: Box<Spanned<Expr>>,
    /// The binary operator.
    pub op: Spanned<BinaryOp>,
    /// The right-hand side of this expression.
    pub rhs: Box<Spanned<Expr>>,
}

/// A base expression. This differs from a normal [`Expr`], in the way that it cannot be a binary expression.
#[derive(Clone, Debug, PartialEq, Eq, IsFail)]
#[fail(bubble)]
pub enum BaseExpr {
    /// A literal expression.
    Literal(Literal),
    /// A path. This could also be a binding.
    Path(Path),
    /// A unary expression.
    UnaryExpr(Unary),
    /// A parenthesized expression.
    Parenthesized(Box<Expr>),
    /// A block expression.
    Block(BlockExpr),
    /// A conditional expression.
    CondExpr(CondExpr),
    /// A function call.
    FnCall(FnCall),
    /// A field access.
    FieldAccess(FieldAccess),
}

/// An expression.
#[derive(Clone, Debug, PartialEq, Eq, IsFail)]
#[fail(bubble)]
pub enum Expr {
    /// A binary expression.
    BinaryExpr(BinaryExpr),
    /// A base expression.
    BaseExpr(BaseExpr),
}

/// A unary expression.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Unary {
    /// The operator.
    pub op: Spanned<UnaryOp>,
    /// The expression.
    pub expr: Box<Spanned<BaseExpr>>,
}

/// A unary operator, used in unary expressions.
#[derive(Clone, Copy, Debug, PartialEq, Eq, IsFail)]
#[fail = false]
pub enum UnaryOp {
    /// The `!` operator.
    Not,
    /// The `-` operator.
    Neg,
}

impl IsFail for Unary {
    fn is_fail(&self) -> bool {
        self.op.is_fail() && self.expr.is_fail()
    }
}

/// A block expression.
#[derive(Clone, PartialEq, Eq, Debug, IsFail)]
#[fail = false]
pub struct BlockExpr {
    /// The statements.
    pub stmts: Vec<Spanned<Stmt>>,
    /// The optional, trailing tail expression.
    pub tail: Option<Box<Spanned<Expr>>>,
}

/// A conditional expression, also known as `if`-expressions.
#[derive(Clone, PartialEq, Eq, Debug, IsFail)]
#[fail = false]
pub struct CondExpr {
    /// The initial `if`-block.
    pub if_block: Spanned<IfBlock>,
    /// `else if`-blocks.
    pub else_if_blocks: Vec<Spanned<IfBlock>>,
    /// A final, optional `else`-block.
    pub else_block: Option<Spanned<BlockExpr>>,
}

impl CondExpr {
    const TOKENS_AFTER_ELSE: &[TokenKind] = &[TokenKind::Identifier, TokenKind::LCurly];
}

/// An `if`-block. This may also be used an `else if` block, since they are semantically the same.
#[derive(Clone, PartialEq, Eq, Debug, IsFail)]
#[fail = false]
pub struct IfBlock {
    /// The condition.
    pub cond: Box<Spanned<Expr>>,
    /// The code to be executed.
    pub block: Spanned<BlockExpr>,
}

/// A function call.
#[derive(Clone, PartialEq, Eq, Debug, IsFail)]
#[fail = false]
pub struct FnCall {
    /// The target, like `print` in `print("Hello, world")`.
    pub target: Box<Spanned<BaseExpr>>,
    /// The arguments.
    pub args: Spanned<CommaSeparated<Expr>>,
}

/// A field access.
#[derive(Clone, PartialEq, Eq, Debug, IsFail)]
#[fail = false]
pub struct FieldAccess {
    /// The target, like `ptr` in `box.ptr`.
    pub target: Box<Spanned<BaseExpr>>,
    /// The field to access.
    pub field: Spanned<Ident>,
}

////////////////////////////////////
// Parsers                        //
////////////////////////////////////

impl Parse for BinaryOp {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let token = tokens.advance()?;
        let t = match token.kind {
            TokenKind::Plus => Self::Add,
            TokenKind::Minus => Self::Sub,
            TokenKind::Slash => Self::Div,
            TokenKind::Star => Self::Mul,
            TokenKind::Percent => Self::Rem,
            TokenKind::Dot => Self::Dot,
            TokenKind::Equal => {
                tokens.advance_require(TokenKind::Equal)?;
                Self::Eq
            }
            TokenKind::Bang => {
                tokens.advance_require(TokenKind::Equal)?;
                Self::NotEq
            }
            _ => {
                return Err(ParseError::new(
                    token.span,
                    token.span,
                    ParseErrorKind::IncorrectToken {
                        got: *token,
                        expected: OneOrMany::Multiple(Self::ACCEPTED_TOKENS),
                    },
                ));
            }
        };

        Ok(t)
    }
}

impl Parse for BaseExpr {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        if let Ok(l_paren) = tokens.advance_require(TokenKind::LParen) {
            let expr = tokens.with(Expr::parse)?;
            match tokens.advance_require(TokenKind::RParen) {
                Ok(_) => Ok(Self::Parenthesized(Box::new(expr))),
                Err(e) => Err(ParseError {
                    span: e.span(),
                    context: l_paren.span.extend(e.span()),
                    kind: ParseErrorKind::UnclosedDelimiter(TokenKind::LParen),
                }),
            }
        } else {
            Self::parse_1(tokens)
        }
    }
}

impl BaseExpr {
    fn parse_1(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        if let Ok(op) = tokens.spanning(UnaryOp::parse) {
            return Ok(Self::UnaryExpr(Unary {
                op,
                expr: Box::new(tokens.spanning(Self::parse)?),
            }));
        }

        let expr = tokens.spanning(|tokens| {
            tokens
                .with(Literal::parse)
                .map(Self::Literal)
                .or_else(|_| tokens.with(CondExpr::parse).map(Self::CondExpr))
                .or_else(|_| tokens.with(BlockExpr::parse).map(Self::Block))
                .or_else(|_| tokens.with(Path::parse).map(Self::Path))
        });

        tokens.with(|tokens| Self::parse_2(tokens, expr?))
    }

    fn parse_2(
        tokens: &mut cryo_lexer::stream::Guard,
        expr: Spanned<BaseExpr>,
    ) -> crate::ParseResult<Self> {
        if tokens.advance_require(TokenKind::LParen).is_ok() {
            let args = tokens.spanning(CommaSeparated::parse)?;
            let (_, span) = tokens.advance_require(TokenKind::RParen)?.tuple();
            let span = expr.span.extend(span);
            Self::parse_2(
                tokens,
                Spanned::new(
                    Self::FnCall(FnCall {
                        target: Box::new(expr),
                        args,
                    }),
                    span,
                ),
            )
        } else if tokens.advance_require(TokenKind::Dot).is_ok() {
            let field = tokens.spanning(Ident::parse)?;
            let span = expr.span.extend(field.span);

            Self::parse_2(
                tokens,
                Spanned::new(
                    Self::FieldAccess(FieldAccess {
                        target: Box::new(expr),
                        field,
                    }),
                    span,
                ),
            )
        } else {
            Ok(expr.t)
        }
    }
}

impl Expr {
    fn parse_1(tokens: &mut cryo_lexer::stream::Guard, min_prec: u8) -> crate::ParseResult<Self> {
        enum SpannedExpr {
            Base(Spanned<BaseExpr>),
            Binary(Spanned<BinaryExpr>),
        }

        #[expect(clippy::from_over_into)]
        impl Into<Spanned<Expr>> for SpannedExpr {
            fn into(self) -> Spanned<Expr> {
                match self {
                    Self::Base(v) => v.map(Expr::BaseExpr),
                    Self::Binary(v) => v.map(Expr::BinaryExpr),
                }
            }
        }

        let mut lhs = tokens.spanning(BaseExpr::parse).map(SpannedExpr::Base)?;

        while let Ok(op) = tokens.non_consuming(BinaryOp::parse) {
            if op.precedence() <= min_prec {
                break;
            }

            let op = tokens
                .spanning(BinaryOp::parse)
                .expect("operator should parse correctly as it has already been parsed in non-consuming mode");

            let rhs = tokens.spanning(|tokens| Expr::parse_1(tokens, op.precedence()))?;
            let rhs_span = rhs.span;

            let spanned_lhs = lhs.into();

            let bin_expr = BinaryExpr {
                lhs: Box::new(spanned_lhs),
                op,
                rhs: Box::new(rhs),
            };
            let lhs_span = bin_expr.lhs.span;

            lhs = SpannedExpr::Binary(Spanned::new(
                bin_expr,
                lhs_span.extend(op.span).extend(rhs_span),
            ));
        }

        Ok(Into::<Spanned<Expr>>::into(lhs).t)
    }
}

impl Parse for Expr {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        Self::parse_1(tokens, 0)
    }
}

impl UnaryOp {
    /// A list of tokens accepted by the [`UnaryOp`] parser.
    pub const ACCEPTED_TOKENS: &[TokenKind] = &[TokenKind::Bang];
}

impl Parse for UnaryOp {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let token = tokens.advance()?;
        match token.kind {
            TokenKind::Bang => Ok(Self::Not),
            TokenKind::Minus => Ok(Self::Neg),
            _ => Err(ParseError::new(
                token.span,
                token.span,
                ParseErrorKind::IncorrectToken {
                    got: *token,
                    expected: OneOrMany::Multiple(Self::ACCEPTED_TOKENS),
                },
            )),
        }
    }
}

impl Parse for BlockExpr {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        tokens.advance_require(TokenKind::LCurly)?;
        let mut stmts = Vec::new();
        let mut tail = None;

        while let Ok(stmt) = tokens.spanning(Stmt::parse) {
            stmts.push(stmt);
        }

        if let Ok(expr) = tokens.spanning(Expr::parse) {
            tail.replace(Box::new(expr));
        }
        tokens.advance_require(TokenKind::RCurly)?;

        Ok(Self { stmts, tail })
    }
}

impl Parse for IfBlock {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let if_kw = tokens.spanning(Ident::parse)?;

        if if_kw.0 != *IF {
            return Err(ParseError {
                span: if_kw.span,
                context: if_kw.span,
                kind: ParseErrorKind::ExpectedKeyword(OneOrMany::Owned(&IF)),
            });
        }

        let cond = Box::new(tokens.spanning(Expr::parse)?);
        let block = tokens.spanning(BlockExpr::parse)?;

        Ok(Self { cond, block })
    }
}

impl Parse for CondExpr {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let if_block = tokens.spanning(IfBlock::parse)?;
        let mut else_if_blocks = Vec::new();
        let mut else_block = None;

        loop {
            let Ok(else_kw) = tokens.spanning(Ident::parse) else {
                break;
            };

            if else_kw.0 != *ELSE {
                break;
            }

            let next_token = tokens.peek()?;

            match next_token.kind {
                TokenKind::LCurly => {
                    else_block.replace(tokens.spanning(BlockExpr::parse)?);
                    break;
                }
                TokenKind::Identifier => else_if_blocks.push(tokens.spanning(IfBlock::parse)?),
                _ => {
                    return Err(ParseError {
                        span: next_token.span,
                        context: next_token.span,
                        kind: ParseErrorKind::IncorrectToken {
                            got: *next_token,
                            expected: OneOrMany::Multiple(Self::TOKENS_AFTER_ELSE),
                        },
                    });
                }
            }
        }

        Ok(Self {
            if_block,
            else_if_blocks,
            else_block,
        })
    }
}

#[cfg(false)]
impl Parse for FnCall {
    fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
        let target = tokens.spanning(BaseExpr::parse).map(Box::new)?;
        tokens.advance_require(TokenKind::LParen)?;
        let args = tokens.spanning(CommaSeparated::parse)?;
        tokens.advance_require(TokenKind::RParen)?;

        Ok(Self { target, args })
    }
}

/// Literal parsers and structs.
pub mod literal {
    use crate::{IsFail, Parse};
    use cryo_lexer::{
        Symbol, TokenKind,
        stream::{StreamLike, TokenStreamError},
    };
    use cryo_parser_proc_macro::IsFail;

    /// A literal expression.
    #[derive(Debug, PartialEq, Eq, Clone, IsFail)]
    #[fail(bubble)]
    pub enum Literal {
        /// A string literal.
        StringLiteral(StringLiteral),
        /// An integer literal.
        IntegerLiteral(IntegerLiteral),
    }

    /// A string literal expression.
    #[derive(Debug, PartialEq, Eq, Clone, IsFail)]
    #[fail = false]
    pub enum StringLiteral {
        /// A fully parsed string literal.
        Value(Symbol),
        /// An invalid escape was encountered.
        InvalidEscape(Symbol, char),
    }

    /// An integer literal expression.
    #[derive(Debug, PartialEq, Eq, Clone, IsFail)]
    pub enum IntegerLiteral {
        /// A fully parsed integer literal.
        Value(i32),
        /// A series of `-` was parsed, but no integer literal.
        #[fail]
        Negations,
        /// An overflow would have occurred.
        #[fail]
        Overflow(i32),
    }

    ////////////////////////////////////
    // Parsers                        //
    ////////////////////////////////////

    impl Parse for Literal {
        fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
            tokens
                .with(StringLiteral::parse)
                .map(Self::StringLiteral)
                .or_else(|_| tokens.with(IntegerLiteral::parse).map(Self::IntegerLiteral))
        }
    }

    impl Parse for StringLiteral {
        fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
            let str_token = tokens.advance_require(TokenKind::StringLiteral)?;
            let mut buf = String::with_capacity(str_token.len());

            let mut iter = str_token.chars();

            while let Some(ch) = iter.next() {
                let new_ch = if ch == '\\' {
                    // lexer guarantees there will be a next character
                    let esc = iter.next().unwrap();

                    match esc {
                        '\\' => '\\',
                        'n' => '\n',
                        't' => '\t',
                        '0' => '\0',
                        other => return Ok(Self::InvalidEscape(buf.as_str().into(), other)),
                    }
                } else {
                    ch
                };

                buf.push(new_ch)
            }

            Ok(Self::Value(buf.as_str().into()))
        }
    }

    impl Parse for IntegerLiteral {
        fn parse(tokens: &mut cryo_lexer::stream::Guard) -> crate::ParseResult<Self> {
            let mut neg = 1;

            while tokens.advance_require(TokenKind::Minus).is_ok() {
                neg *= -1;
            }

            let int_token = match tokens.advance_require(TokenKind::IntegerLiteral) {
                Ok(v) => v,
                Err(TokenStreamError::EndOfInput { .. }) => return Ok(Self::Negations),
                Err(e) => return Err(e.into()),
            };

            let mut int = 0i32;

            for digit in int_token.chars() {
                if let '0'..='9' = digit {
                    let snapshot = int;
                    int = match int.checked_mul(10) {
                        Some(v) => v,
                        None => return Ok(Self::Overflow(snapshot)),
                    };

                    int = match int.checked_add(digit.to_digit(10).unwrap() as i32) {
                        Some(v) => v,
                        None => return Ok(Self::Overflow(snapshot)),
                    };
                }
            }

            int *= neg;

            Ok(Self::Value(int))
        }
    }

    ////////////////////////////////////
    // Tests                          //
    ////////////////////////////////////

    #[cfg(test)]
    mod tests {
        use super::*;
        use cryo_span::{Span, Spanned};

        use crate::test_util::assert_parse;

        #[test]
        fn parse_int() {
            assert_parse(
                "--123_456",
                Spanned::new(IntegerLiteral::Value(123_456), Span::new(0, 9)),
            );
        }

        #[test]
        fn parse_int_only_neg_sign() {
            assert_parse(
                "--",
                Spanned::new(IntegerLiteral::Negations, Span::new(0, 2)),
            )
        }

        #[test]
        fn parse_int_overflow() {
            assert_parse(
                "2147483648",
                Spanned::new(IntegerLiteral::Overflow(214748364), Span::new(0, 10)),
            );
        }

        #[test]
        fn parse_str() {
            assert_parse(
                "\"Hello, world!\\n\"",
                Spanned::new(
                    StringLiteral::Value("Hello, world!\n".into()),
                    Span::new(0, 17),
                ),
            );
        }

        #[test]
        fn parse_str_inv_escape() {
            assert_parse(
                "\"Hello, world!\\f\"",
                Spanned::new(
                    StringLiteral::InvalidEscape("Hello, world!".into(), 'f'),
                    Span::new(0, 17),
                ),
            )
        }
    }
}

////////////////////////////////////
// Tests                          //
////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;
    use cryo_lexer::Symbol;
    use cryo_span::{Span, Spanned};

    use crate::{
        expr::literal::{IntegerLiteral, StringLiteral},
        stmt::BindingDef,
        test_util::assert_parse,
    };

    #[test]
    fn parse_bin_expr() {
        assert_parse(
            "2 + 2",
            Spanned::new(
                Expr::BinaryExpr(BinaryExpr {
                    lhs: Box::new(Spanned::new(
                        Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                            IntegerLiteral::Value(2),
                        ))),
                        Span::new(0, 1),
                    )),
                    op: Spanned::new(BinaryOp::Add, Span::new(2, 3)),
                    rhs: Box::new(Spanned::new(
                        Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                            IntegerLiteral::Value(2),
                        ))),
                        Span::new(4, 5),
                    )),
                }),
                Span::new(0, 5),
            ),
        );
    }

    #[test]
    fn parse_bin_expr_nested() {
        // [(4 * 3) - 2] == (2 * 5)
        assert_parse(
            "4 * 3 - 2 == 2 * 5",
            Spanned::new(
                Expr::BinaryExpr(BinaryExpr {
                    // 4 * 3 - 2
                    lhs: Box::new(Spanned::new(
                        Expr::BinaryExpr(BinaryExpr {
                            // 4 * 3
                            lhs: Box::new(Spanned::new(
                                Expr::BinaryExpr(BinaryExpr {
                                    // 4
                                    lhs: Box::new(Spanned::new(
                                        Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                                            IntegerLiteral::Value(4),
                                        ))),
                                        Span::new(0, 1),
                                    )),
                                    // *
                                    op: Spanned::new(BinaryOp::Mul, Span::new(2, 3)),
                                    // 3
                                    rhs: Box::new(Spanned::new(
                                        Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                                            IntegerLiteral::Value(3),
                                        ))),
                                        Span::new(4, 5),
                                    )),
                                }),
                                Span::new(0, 5),
                            )),
                            // -
                            op: Spanned::new(BinaryOp::Sub, Span::new(6, 7)),
                            // 2
                            rhs: Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                                    IntegerLiteral::Value(2),
                                ))),
                                Span::new(8, 9),
                            )),
                        }),
                        Span::new(0, 9),
                    )),
                    // ==
                    op: Spanned::new(BinaryOp::Eq, Span::new(10, 12)),
                    // 2 * 5
                    rhs: Box::new(Spanned::new(
                        Expr::BinaryExpr(BinaryExpr {
                            // 2
                            lhs: Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                                    IntegerLiteral::Value(2),
                                ))),
                                Span::new(13, 14),
                            )),
                            // *
                            op: Spanned::new(BinaryOp::Mul, Span::new(15, 16)),
                            // 5
                            rhs: Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                                    IntegerLiteral::Value(5),
                                ))),
                                Span::new(17, 18),
                            )),
                        }),
                        Span::new(13, 18),
                    )),
                }),
                Span::new(0, 18),
            ),
        )
    }

    #[test]
    fn parse_unary_ops() {
        assert_parse(
            "!(0==0)",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::UnaryExpr(Unary {
                    op: Spanned::new(UnaryOp::Not, Span::new(0, 1)),
                    expr: Box::new(Spanned::new(
                        BaseExpr::Parenthesized(Box::new(Expr::BinaryExpr(BinaryExpr {
                            lhs: Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                                    IntegerLiteral::Value(0),
                                ))),
                                Span::new(2, 3),
                            )),
                            op: Spanned::new(BinaryOp::Eq, Span::new(3, 5)),
                            rhs: Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                                    IntegerLiteral::Value(0),
                                ))),
                                Span::new(5, 6),
                            )),
                        }))),
                        Span::new(1, 7),
                    )),
                })),
                Span::new(0, 7),
            ),
        )
    }

    #[test]
    fn parse_binary_with_unary() {
        assert_parse(
            "5 - -5",
            Spanned::new(
                Expr::BinaryExpr(BinaryExpr {
                    lhs: Box::new(Spanned::new(
                        Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                            IntegerLiteral::Value(5),
                        ))),
                        Span::new(0, 1),
                    )),
                    op: Spanned::new(BinaryOp::Sub, Span::new(2, 3)),
                    rhs: Box::new(Spanned::new(
                        Expr::BaseExpr(BaseExpr::UnaryExpr(Unary {
                            op: Spanned::new(UnaryOp::Neg, Span::new(4, 5)),
                            expr: Box::new(Spanned::new(
                                BaseExpr::Literal(Literal::IntegerLiteral(IntegerLiteral::Value(
                                    5,
                                ))),
                                Span::new(5, 6),
                            )),
                        })),
                        Span::new(4, 6),
                    )),
                }),
                Span::new(0, 6),
            ),
        );
    }

    #[test]
    fn parse_block_expr() {
        assert_parse(
            "{ let x = 5; x }",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::Block(BlockExpr {
                    stmts: vec![Spanned::new(
                        Stmt::BindingDef(BindingDef {
                            ident: Spanned::new(Ident(Symbol::from("x")), Span::new(6, 7)),
                            mutable: None,
                            value: Spanned::new(
                                Expr::BaseExpr(BaseExpr::Literal(Literal::IntegerLiteral(
                                    IntegerLiteral::Value(5),
                                ))),
                                Span::new(10, 12),
                            ),
                        }),
                        Span::new(2, 12),
                    )],
                    tail: Some(Box::new(Spanned::new(
                        Expr::BaseExpr(BaseExpr::Path(Spanned::new("x", Span::new(13, 14)).into())),
                        Span::new(13, 14),
                    ))),
                })),
                Span::new(0, 16),
            ),
        );
    }

    #[test]
    fn parse_cond_expr() {
        assert_parse(
            "if true {} else if false {} else {}",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::CondExpr(CondExpr {
                    if_block: Spanned::new(
                        IfBlock {
                            cond: Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Path(
                                    Spanned::new("true", Span::new(3, 7)).into(),
                                )),
                                Span::new(3, 7),
                            )),
                            block: Spanned::new(
                                BlockExpr {
                                    stmts: vec![],
                                    tail: None,
                                },
                                Span::new(8, 10),
                            ),
                        },
                        Span::new(0, 10),
                    ),
                    else_if_blocks: vec![Spanned::new(
                        IfBlock {
                            cond: Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Path(
                                    Spanned::new("false", Span::new(19, 24)).into(),
                                )),
                                Span::new(19, 24),
                            )),
                            block: Spanned::new(
                                BlockExpr {
                                    stmts: vec![],
                                    tail: None,
                                },
                                Span::new(25, 27),
                            ),
                        },
                        Span::new(16, 27),
                    )],
                    else_block: Some(Spanned::new(
                        BlockExpr {
                            stmts: vec![],
                            tail: None,
                        },
                        Span::new(33, 35),
                    )),
                })),
                Span::new(0, 35),
            ),
        );
    }

    #[test]
    fn parse_fn_call() {
        assert_parse(
            "print(\"Hello, world\")",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::FnCall(FnCall {
                    target: Box::new(Spanned::new(
                        BaseExpr::Path(Spanned::new("print", Span::new(0, 5)).into()),
                        Span::new(0, 5),
                    )),
                    args: Spanned::new(
                        CommaSeparated {
                            inner: vec![],
                            last: Some(Box::new(Spanned::new(
                                Expr::BaseExpr(BaseExpr::Literal(Literal::StringLiteral(
                                    StringLiteral::Value(Symbol::from("Hello, world")),
                                ))),
                                Span::new(6, 20),
                            ))),
                        },
                        Span::new(6, 20),
                    ),
                })),
                Span::new(0, 21),
            ),
        );
    }

    #[test]
    fn parse_field_access() {
        assert_parse(
            "box.leak()",
            Spanned::new(
                Expr::BaseExpr(BaseExpr::FnCall(FnCall {
                    target: Box::new(Spanned::new(
                        BaseExpr::FieldAccess(FieldAccess {
                            target: Box::new(Spanned::new(
                                BaseExpr::Path(Spanned::new("box", Span::new(0, 3)).into()),
                                Span::new(0, 3),
                            )),
                            field: Spanned::new(Ident(Symbol::from("leak")), Span::new(4, 8)),
                        }),
                        Span::new(0, 8),
                    )),
                    args: Spanned::new(
                        CommaSeparated {
                            inner: vec![],
                            last: None,
                        },
                        Span::new(9, 9),
                    ),
                })),
                Span::new(0, 10),
            ),
        )
    }
}
