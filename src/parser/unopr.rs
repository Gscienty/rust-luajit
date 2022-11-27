use std::fmt::Display;

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

impl Display for UnOpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NOT => write!(f, "not"),
            Self::MINUS => write!(f, "minus"),
            Self::BNOT => write!(f, "bnot"),
            Self::LEN => write!(f, "len"),
            Self::NoOpr => write!(f, "noopr"),
        }
    }
}
