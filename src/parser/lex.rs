use crate::lexer::Token;

use super::{ParseErr, Parser};

#[macro_export]
macro_rules! consume_token {
    ($parser: expr, $token: pat) => {
        match $parser.lexer.token {
            $token => $parser.skip(),
            _ => Err(ParseErr::UnexpectedSymbol),
        }
    };
}

#[macro_export]
macro_rules! check_token {
    ($parser: expr, $token: pat) => {
        match $parser.lexer.token {
            $token => $parser.skip().is_ok(),
            _ => false,
        }
    };
}

#[macro_export]
macro_rules! matches_token {
    ($parser: expr, $token: pat) => {
        matches!($parser.lexer.token, $token)
    };
}

impl Parser {
    pub(super) fn skip(&mut self) -> Result<(), ParseErr> {
        self.lexer.token_next()
    }

    pub(super) fn name(&mut self) -> Result<String, ParseErr> {
        match &self.lexer.token {
            Token::Name(name) => {
                let name = name.clone();

                self.skip()?;
                Ok(name)
            }
            _ => Err(ParseErr::BadUsage),
        }
    }

    pub(super) fn lookahead(&mut self) -> Result<Token, ParseErr> {
        self.lexer.token_lookahead()
    }
}
