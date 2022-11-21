use crate::lexer::Token;

#[derive(Clone, Copy)]
pub(super) enum UnOpr {
    NOT,
    MINUS,
    BNOT,
    LEN,
    NoOpr,
}

impl From<Token> for UnOpr {
    fn from(token: Token) -> Self {
        match &token {
            Token::Not => UnOpr::NOT,
            Token::Operator('-') => UnOpr::MINUS,
            Token::Operator('~') => UnOpr::BNOT,
            Token::Operator('#') => UnOpr::LEN,
            _ => UnOpr::NoOpr,
        }
    }
}
