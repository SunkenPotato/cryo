use cryo_lexer::tokens::Token;

/// Parses a list of tokens into an AST.
pub struct Parser {
    #[expect(unused)]
    tokens: Vec<Token>,
}

impl Parser {
    pub const fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    pub fn parse(self) -> () {
        todo!()
    }
}
