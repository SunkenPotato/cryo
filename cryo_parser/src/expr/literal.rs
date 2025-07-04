use crate::parser::Parse;

pub struct IntegerLiteral(pub i32);

impl Parse for IntegerLiteral {
    type Output = Self;

    fn parse(
        tokens: &mut cryo_lexer::stream::TokenStreamGuard,
    ) -> crate::parser::ParseResult<Self::Output> {
    }
}
