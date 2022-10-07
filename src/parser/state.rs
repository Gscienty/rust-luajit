use std::{cell::RefCell, rc::Rc, borrow::BorrowMut};

use crate::lex::{LexState, LuaSource, Token};

use super::scope::Scope;

pub(crate) struct ParseState<'s> {
    lex_state: LexState<'s>,
    free_reg: u32,
    active_var_count: u32,
    vtop: u32,
    scopes: Option<Rc<RefCell<Scope>>>,
}

impl<'s> ParseState<'s> {
    pub(crate) fn new(source: LuaSource<'s>) -> Self {
        ParseState {
            lex_state: LexState::new(source),
            free_reg: 0,
            active_var_count: 0,
            vtop: 0,
            scopes: None,
        }
    }

    pub(crate) fn begin_scope(&mut self, flags: u8) -> Rc<RefCell<Scope>> {
        let scope = Scope::new();

        scope.as_ref().borrow_mut().active_var_count = self.active_var_count;
        scope.as_ref().borrow_mut().start = self.vtop;
        scope.as_ref().borrow_mut().flags = flags;

        if let Some(prev) = self.scopes.clone() {
            scope.as_ref().borrow_mut().prev = Some(prev);
        }
        self.scopes = Some(scope.clone());

        scope
    }

    pub(crate) fn end_scope(&mut self) {
        self.scopes = if let Some(prev) = self.scopes
            .as_ref()
            .and_then(|scope| scope.as_ref().borrow().prev.clone()) {
            Some(prev)
        } else {
            None
        }

        // var remove
    }

    fn parse_statement(&mut self) -> bool {
        let line = self.lex_state.line_number;

        match self.lex_state.token {
            Token::If => {

            },
            Token::While => {

            },
            Token::Do => {

            },
            Token::For => {

            },
            Token::Repeat => {

            },
            Token::Function => {

            },
            Token::Local => {

            },
            Token::Return => {

            },
            Token::Break => {

            },
            Token::Semicolon => {
                self.lex_state.next();
            },
            Token::Label => {

            },
            Token::Goto => {

            },
            _ => {

            },
        }

        false
    }

    fn parse_block(&mut self) {
        _ = self.begin_scope(0);
        self.parse_chunk();
    }

    pub(crate) fn parse_chunk(&mut self) {
        while !self.lex_state.is_end_block() {
            let last = self.parse_statement();

            _ = self.lex_state.check_and_consume(Token::Semicolon);
            self.free_reg = self.active_var_count;

            if !last {
                break;
            }
        }
    }
}