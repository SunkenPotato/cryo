use crate::group;

use super::{
    Identifier, IdentifierParseError, Parse,
    expr::{Expr, ExprParseError},
    extract_whitespace, tag_err,
};

pub const MUTABLE_TOKEN: &str = "mut";
pub const BIND_TOKEN: &str = "let";
pub const ASSIGN_TOKEN: &str = "=";
pub const SEMICOLON: &str = ";";

#[derive(Debug, PartialEq)]
pub struct Binding {
    identifier: Identifier,
    expr: Expr,
    mutable: Option<Mutable>,
}

group! {
    #[derive(Debug, PartialEq)]
    pub enum BindingParseError {
        EndOfInput,
        BindTokenNotFound,
        AssignTokenNotFound,
        SemicolonNotFound,
        IdentifierParseError(IdentifierParseError),
        ExprParseError(ExprParseError)
    }
}

impl Parse for Binding {
    type Error = BindingParseError;

    fn parse(mut input: &str) -> Result<(Self, &str), Self::Error> {
        input = extract_whitespace(input);
        input = tag_err(BIND_TOKEN, input, BindingParseError::BindTokenNotFound)?;
        let (mutable, input) = match Mutable::parse(input) {
            Ok(v) => (Some(v.0), v.1),
            Err(e) => match e {
                MutableParseError::EndOfInput => return Err(BindingParseError::EndOfInput),
                MutableParseError::NotFound => (None, input),
            },
        };

        let (identifier, mut input) = Identifier::parse(input)?;
        input = tag_err(
            ASSIGN_TOKEN,
            extract_whitespace(input),
            BindingParseError::AssignTokenNotFound,
        )?;
        let (expr, mut input) = Expr::parse(input)?;

        input = tag_err(
            SEMICOLON,
            extract_whitespace(input),
            BindingParseError::SemicolonNotFound,
        )?;

        let s = Self {
            identifier,
            expr,
            mutable,
        };

        Ok((s, input))
    }
}

#[derive(Debug, PartialEq)]
pub struct Mutable;

#[derive(Debug, PartialEq)]
pub enum MutableParseError {
    NotFound,
    EndOfInput,
}

impl Parse for Mutable {
    type Error = MutableParseError;

    fn parse(mut input: &str) -> Result<(Self, &str), Self::Error> {
        input = extract_whitespace(input);
        let next3 = input.get(0..3).ok_or(MutableParseError::EndOfInput)?;
        match next3 {
            MUTABLE_TOKEN => Ok((Self, &input[3..])),
            _ => Err(MutableParseError::NotFound),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        Identifier, Parse,
        binding::{Binding, BindingParseError, Mutable},
        expr::Expr,
        literal::{Literal, integer::Integer, string::StringLiteral},
    };

    #[test]
    fn parse_int_binding() {
        let s = "let x = 5;";

        assert_eq!(
            Ok((
                Binding {
                    mutable: None,
                    expr: Expr::Literal(Literal::Integer(Integer(5))),
                    identifier: Identifier::new("x")
                },
                ""
            )),
            Binding::parse(s)
        );
    }

    #[test]
    fn parse_string_binding() {
        let s = "let x = \"Hello, world\";";

        assert_eq!(
            Ok((
                Binding {
                    mutable: None,
                    expr: Expr::Literal(Literal::String(StringLiteral("Hello, world".into()))),
                    identifier: Identifier::new("x")
                },
                ""
            ),),
            Binding::parse(s)
        );
    }

    #[test]
    fn parse_mutable_binding() {
        let s = "let mut x = 5;";

        assert_eq!(
            Ok((
                Binding {
                    mutable: Some(Mutable),
                    expr: Expr::Literal(Literal::Integer(Integer(5))),
                    identifier: Identifier::new("x")
                },
                ""
            )),
            Binding::parse(s)
        );
    }

    #[test]
    fn do_not_parse_missing_semicolon() {
        let s = "let x = 5";

        assert_eq!(Err(BindingParseError::SemicolonNotFound), Binding::parse(s))
    }

    #[test]
    fn do_not_parse_missing_assign() {
        let s = "let x 5;";

        assert_eq!(
            Err(BindingParseError::AssignTokenNotFound),
            Binding::parse(s)
        );
    }

    #[test]
    fn do_not_parse_missing_bind() {
        let s = "x = 5;";

        assert_eq!(Err(BindingParseError::BindTokenNotFound), Binding::parse(s))
    }

    #[test]
    fn do_not_parse_incorrect_mutable_token() {
        let s = "let mat x = 5";

        assert_eq!(
            Err(BindingParseError::AssignTokenNotFound),
            Binding::parse(s)
        )
    }
}
