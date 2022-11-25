use crate::lexer::Token;

use super::{ParseErr, Parser};

pub(super) struct ParseLex<'s> {
    p: &'s mut Parser,
}

#[macro_export]
macro_rules! match_token {
    (consume: $parser: expr, $token: pat) => {
        $parser.lex_mut(|x| match &x.token {
            $token => {
                x.token_next();
                Ok(())
            }
            _ => Err(ParseErr::UnexpectedSymbol),
        })
    };
    (test_consume: $parser: expr, $token: pat) => {
        $parser.lex_mut(|x| match &x.token {
            $token => {
                x.token_next();
                true
            }
            _ => false,
        })
    };
    (test: $parser: expr, $token: pat) => {
        $parser.lex(|x| matches!(x.token, $token))
    };
}

impl<'s> ParseLex<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
    }

    pub(super) fn name(&mut self) -> Result<String, ParseErr> {
        self.p.lex_mut(|x| match &x.token {
            Token::Name(v) => {
                let v = String::from(v);
                x.token_next();

                Ok(v)
            }
            _ => Err(ParseErr::BadUsage),
        })
    }

    pub(super) fn skip(&mut self) {
        self.p.lex_mut(|x| x.token_next());
    }

    pub(super) fn current_token(&self) -> Token {
        self.p.lex(|x| x.token.clone())
    }
}
