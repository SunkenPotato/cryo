//! Literal expressions.
//!
//! Literal expressions are expressions whose value is known at parse-time, are direct and require no further computation.
//!
//! Literal expressions are *not* any of the following:
//! - binding references
//! - arithmetic expressions

/// A literal expression.
///
/// A [`Literal`] can be any of the following:
/// - [`NumberLiteral`]
/// - [`StringLiteral`]
///
/// View the module-level docs for a definition of literal expressions.
pub enum Literal {
    /// The number literal variant.
    NumberLiteral(NumberLiteral),
    /// The string literal variant.
    StringLiteral(StringLiteral),
}

/// An integer literal.
///
/// This literal has the same constraints as an [`i32`], since it is represented by an `i32`.
pub struct NumberLiteral(pub i32);

/// An escaped string literal.
///
/// This literal has the same constraints as a [`String`], since it is represented by a `String`.
pub struct StringLiteral(pub String);
