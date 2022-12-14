use std::fmt::Display;

use crate::lexer::Token;

#[derive(Clone, Copy)]
pub(super) enum BinOpr {
    ADD,
    SUB,
    MUL,
    DIV,
    IDIV,
    MOD,
    POW,
    CONCAT,
    NE,
    EQ,
    LT,
    GE,
    LE,
    GT,
    BAND,
    BOR,
    BXOR,
    AND,
    OR,
    SHL,
    SHR,

    NoOpr,
}

impl BinOpr {
    pub(crate) const INIT_PRI: u8 = 0;
    pub(crate) const UNARY_PRI: u8 = 12;

    pub(crate) fn lpri(&self) -> u8 {
        match self {
            BinOpr::ADD | BinOpr::SUB => 10,
            BinOpr::MUL | BinOpr::MOD | BinOpr::DIV | BinOpr::IDIV => 11,
            BinOpr::POW => 14,
            BinOpr::BAND => 6,
            BinOpr::BOR => 4,
            BinOpr::BXOR => 5,
            BinOpr::SHL | BinOpr::SHR => 7,
            BinOpr::CONCAT => 9,
            BinOpr::EQ | BinOpr::LT | BinOpr::LE | BinOpr::NE | BinOpr::GT | BinOpr::GE => 3,
            BinOpr::AND => 2,
            BinOpr::OR => 1,
            _ => 0,
        }
    }

    pub(crate) fn rpri(&self) -> u8 {
        match self {
            BinOpr::ADD | BinOpr::SUB => 10,
            BinOpr::MUL | BinOpr::MOD | BinOpr::DIV | BinOpr::IDIV => 11,
            BinOpr::POW => 13,
            BinOpr::BAND => 6,
            BinOpr::BOR => 4,
            BinOpr::BXOR => 5,
            BinOpr::SHL | BinOpr::SHR => 7,
            BinOpr::CONCAT => 8,
            BinOpr::EQ | BinOpr::LT | BinOpr::LE | BinOpr::NE | BinOpr::GT | BinOpr::GE => 3,
            BinOpr::AND => 2,
            BinOpr::OR => 1,
            _ => 0,
        }
    }
}

impl From<Token> for BinOpr {
    fn from(token: Token) -> Self {
        match &token {
            Token::Operator('+') => BinOpr::ADD,
            Token::Operator('-') => BinOpr::SUB,
            Token::Operator('*') => BinOpr::MUL,
            Token::Operator('/') => BinOpr::DIV,
            Token::IDIV => BinOpr::IDIV,
            Token::Operator('%') => BinOpr::MOD,
            Token::Operator('^') => BinOpr::POW,
            Token::Concat => BinOpr::CONCAT,
            Token::NE => BinOpr::NE,
            Token::EQ => BinOpr::EQ,
            Token::LE => BinOpr::LE,
            Token::Operator('<') => BinOpr::LT,
            Token::GE => BinOpr::GE,
            Token::Operator('>') => BinOpr::GT,
            Token::Operator('&') => BinOpr::BAND,
            Token::Operator('|') => BinOpr::BOR,
            Token::And => BinOpr::AND,
            Token::Or => BinOpr::OR,
            Token::Operator('~') => BinOpr::BXOR,
            Token::SHL => BinOpr::SHL,
            Token::SHR => BinOpr::SHR,

            _ => BinOpr::NoOpr,
        }
    }
}

impl From<&Token> for BinOpr {
    fn from(token: &Token) -> Self {
        match token {
            Token::Operator('+') => BinOpr::ADD,
            Token::Operator('-') => BinOpr::SUB,
            Token::Operator('*') => BinOpr::MUL,
            Token::Operator('/') => BinOpr::DIV,
            Token::IDIV => BinOpr::IDIV,
            Token::Operator('%') => BinOpr::MOD,
            Token::Operator('^') => BinOpr::POW,
            Token::Concat => BinOpr::CONCAT,
            Token::NE => BinOpr::NE,
            Token::EQ => BinOpr::EQ,
            Token::LE => BinOpr::LE,
            Token::Operator('<') => BinOpr::LT,
            Token::GE => BinOpr::GE,
            Token::Operator('>') => BinOpr::GT,
            Token::Operator('&') => BinOpr::BAND,
            Token::Operator('|') => BinOpr::BOR,
            Token::And => BinOpr::AND,
            Token::Or => BinOpr::OR,
            Token::Operator('~') => BinOpr::BXOR,
            Token::SHL => BinOpr::SHL,
            Token::SHR => BinOpr::SHR,

            _ => BinOpr::NoOpr,
        }
    }
}

impl Display for BinOpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ADD => write!(f, "add"),
            Self::SUB => write!(f, "sub"),
            Self::MUL => write!(f, "mul"),
            Self::DIV => write!(f, "div"),
            Self::IDIV => write!(f, "idiv"),
            Self::MOD => write!(f, "mod"),
            Self::POW => write!(f, "pow"),
            Self::CONCAT => write!(f, "concat"),
            Self::NE => write!(f, "ne"),
            Self::EQ => write!(f, "eq"),
            Self::LT => write!(f, "lt"),
            Self::GE => write!(f, "ge"),
            Self::LE => write!(f, "le"),
            Self::GT => write!(f, "gt"),
            Self::BAND => write!(f, "band"),
            Self::BOR => write!(f, "bor"),
            Self::BXOR => write!(f, "bxor"),
            Self::AND => write!(f, "and"),
            Self::OR => write!(f, "or"),
            Self::SHL => write!(f, "shl"),
            Self::SHR => write!(f, "shr"),
            Self::NoOpr => write!(f, "noopr"),
        }
    }
}
