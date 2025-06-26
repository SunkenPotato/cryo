use cryo_span::Spanned;

pub type Token = Spanned<TokenType>;

macro_rules! atom {
    (
        $(#[$attr:meta])*
        #($atom:expr)
        $visibility:vis struct $identifier:ident;
    ) => {
        $visibility struct $identifier;

        impl $crate::Lex for $identifier {
            fn lex(s: &str) -> Result<(Self, &str), $crate::LexicalError> {
                let rest = s.strip_prefix($atom).ok_or($crate::LexicalError::SequenceNotFound($atom))?;
                Ok((Self, rest))
            }
        }
    };
}

trait Lex: Sized {
    fn lex(s: &str) -> Result<(Self, &str), LexicalError>;
}

#[derive(Debug, Clone)]
pub enum TokenType {}

pub enum LexicalError<'a> {
    SequenceNotFound(&'a str),
}

pub fn lexer(input: &str) -> Result<Vec<Token>, LexicalError> {
    todo!()
}
