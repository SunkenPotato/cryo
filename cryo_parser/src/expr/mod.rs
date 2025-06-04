//! Expressions.
//!
//! Expressions are defined as statements of code that return a value, be it `null`, `()`, or any other.
//!
//! For different types of expressions, view the [`Expr`] enum.

pub mod literal;
pub mod math_expr;

/// A single expression.
///
/// Expressions can have two different types, namely:
/// - [`MathExpr`]
/// - [`ReducedExpr`]
///
/// Due to [`MathExpr`] consisting of possibly many different chained `Expr`s, `Expr` is split up into `MathExpr` and `ReducedExpr`, which contains all [`Expr`] variants except for [`MathExpr`].
pub enum Expr {}
