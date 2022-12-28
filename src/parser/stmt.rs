use crate::lexer::Token;

use super::{ParseErr, Parser};

impl Parser {
    // stmt ::= `;` | local_stmt | label_stmt | break_stmt | goto_stmt
    //          | do_stmt | while_stmt | repeat_stmt | if_stmt | func_stmt
    //          | return_stmt | for_stmt | exp_stmt
    pub(super) fn stmt(&mut self) -> Result<(), ParseErr> {
        match self.lexer.token {
            Token::Operator(';') => self.skip()?,
            Token::Local => self.local_stmt()?,
            Token::Label => self.label_stmt()?,
            Token::Break => self.break_stmt()?,
            Token::Goto => self.goto_stmt()?,
            Token::Do => self.do_stmt()?,
            Token::While => self.while_stmt()?,
            Token::Repeat => self.repeat_stmt()?,
            Token::If => self.if_stmt()?,
            Token::Function => self.func_stmt()?,
            Token::Return => self.return_stmt()?,
            Token::For => self.for_stmt()?,
            _ => self.exp_stmt()?,
        }

        self.fs.prop_mut().freereg = self.nvarstack();

        Ok(())
    }

    // stmtlist ::= stmt { stmt }
    pub(super) fn stmtlist(&mut self) -> Result<(), ParseErr> {
        while !self.lexer.token.block_follow(true) {
            if matches!(self.lexer.token, Token::Return) {
                self.stmt()?;
                break;
            }
            self.stmt()?;
        }

        Ok(())
    }
}
