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
    AND,
    OR,
    XOR,
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
            BinOpr::XOR => 5,
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
            BinOpr::XOR => 5,
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
            Token::Operator('~') => BinOpr::XOR,
            Token::SHL => BinOpr::SHL,
            Token::SHR => BinOpr::SHR,

            _ => BinOpr::NoOpr,
        }
    }
}
