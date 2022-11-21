use crate::lexer::Token;

use super::{ParseErr, Parser};

pub(super) struct ParseLex<'s> {
    p: &'s mut Parser,
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
