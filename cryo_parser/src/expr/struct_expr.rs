//! Struct expressions.

use cryo_parser_proc_macro::Parse;

use crate::{
    atoms::{Colon, Comma, LCurly, RCurly},
    expr::Expr,
    ident::Ident,
    parser::Punct,
};

/// A struct expression.
#[derive(Parse, PartialEq, Debug)]
pub struct StructExpr {
    /// The identifier of a struct.
    pub ident: Ident,
    /// The body of the given struct.
    pub body: StructExprBody,
}

/// A named expression, such as `a: 5`. Used in struct constructor expressions.
#[derive(Parse, PartialEq, Debug)]
pub struct NamedExpr {
    /// The field identifier.
    pub field: Ident,
    /// The separating colon.
    pub colon: Colon,
    /// The associated expression.
    pub expr: Expr,
}

/// A struct constructor body.
#[derive(Parse, PartialEq, Debug)]
pub struct StructExprBody {
    /// The opening delimiter.
    pub l_paren: LCurly,
    /// The fields.
    pub fields: Punct<NamedExpr, Comma>,
    /// The closing delimiter.
    pub r_paren: RCurly,
}
