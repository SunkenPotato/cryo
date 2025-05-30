use crate::lexer::tokens::Token;

pub trait Parse: Sized {
    fn parse(stream: &[Token]) -> Result<(Self, &[Token]), impl ParseError>;
}

pub trait ParseError {
    fn code(&self) -> u16;
    fn message(&self) -> String;
    fn tokens(&self) -> &[Token];
}
