use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use crate::{consume_token, lexer::Token, matches_token};

use super::{ParseErr, Parser};

pub(super) struct BlockContent {
    pub(super) prev: Option<Block>,

    pub(super) nactvar: usize,
    pub(super) upval: bool,
    pub(super) first_label: usize,
    pub(super) first_goto: usize,
    pub(super) is_loop: bool,
    pub(super) inside_tobeclosed: bool,
    pub(super) needclose: bool,
}

#[derive(Clone)]
pub(super) struct Block(pub(crate) Rc<RefCell<BlockContent>>);

impl Block {
    pub(super) fn new() -> Self {
        Block(Rc::new(RefCell::new(BlockContent {
            prev: None,

            nactvar: 0,
            upval: false,
            first_label: 0,
            first_goto: 0,
            is_loop: false,
            inside_tobeclosed: false,
            needclose: false,
        })))
    }

    pub(super) fn prop(&self) -> Ref<BlockContent> {
        self.0.as_ref().borrow()
    }

    pub(super) fn prop_mut(&self) -> RefMut<BlockContent> {
        self.0.as_ref().borrow_mut()
    }
}

impl Parser {
    pub(super) fn enterblock(&mut self, isloop: bool) {
        let block = Block::new();

        block.prop_mut().is_loop = isloop;
        block.prop_mut().nactvar = self.fs.prop().nactvar;
        block.prop_mut().first_label = self.label.len();
        block.prop_mut().first_goto = self.goto.len();
        block.prop_mut().inside_tobeclosed = self.fs.prop().block.prop().inside_tobeclosed;
        block.prop_mut().prev = Some(self.fs.prop().block.clone());

        self.fs.prop_mut().block = block;
    }

    pub(super) fn leaveblock(&mut self) -> Result<(), ParseErr> {
        let block = self.fs.prop().block.clone();

        let level = self.reglevel(block.prop().nactvar);
        self.removevars(level);

        let hasclose = if block.prop().is_loop {
            self.createlabel("break", false)?
        } else {
            false
        };

        if !hasclose && block.prop().prev.is_some() && block.prop().upval {
            self.emit_close(level);
        }

        self.fs.prop_mut().freereg = level;
        while self.label.len() > block.prop().first_label {
            self.label.pop();
        }

        if let Some(prev_block) = &block.prop().prev {
            self.fs.prop_mut().block = prev_block.clone();
            self.movegotosout(&block)?;
        } else if block.prop().first_goto < self.goto.len() {
            return Err(ParseErr::BadUsage);
        }

        Ok(())
    }

    pub(super) fn block_follow(&self, withuntil: bool) -> bool {
        if matches_token!(self, Token::Else | Token::ElseIf | Token::End | Token::EOF) {
            true
        } else if matches_token!(self, Token::Until) {
            withuntil
        } else {
            false
        }
    }

    // block_stmt ::= stmtlist_stmt
    pub(super) fn block_stmt(&mut self) -> Result<(), ParseErr> {
        self.enterblock(false);
        self.stmtlist()?;
        self.leaveblock()?;

        Ok(())
    }

    // do_stmt ::= `do` block_stmt `end`
    pub(super) fn do_stmt(&mut self) -> Result<(), ParseErr> {
        self.skip()?;
        self.block_stmt()?;
        consume_token!(self, Token::End)?;
        Ok(())
    }
}
