use std::{cell::RefCell, rc::Rc};

use crate::{
    consume_token,
    lexer::{Lexer, Token},
    object::{ConstantPool, LabelDesc, Prototype, Upval, Var, VarKind},
};

use super::{FuncState, ParseErr};

pub(crate) struct Parser {
    pub(super) fs: FuncState,

    pub(super) lexer: Lexer,

    pub(super) actvar: Vec<Var>,
    pub(super) goto: Vec<LabelDesc>,
    pub(super) label: Vec<LabelDesc>,

    pub(super) last_target: usize,

    pub(crate) constant_pool: Rc<RefCell<ConstantPool>>,
}

impl Parser {
    pub(crate) fn new(
        source: &str,
        proto: Prototype,
        constant_pool: Rc<RefCell<ConstantPool>>,
    ) -> Self {
        Self {
            fs: FuncState::new(proto), // global func
            lexer: Lexer::new(source),

            actvar: Vec::new(),
            goto: Vec::new(),
            label: Vec::new(),

            last_target: 0,

            constant_pool,
        }
    }

    pub(super) const ENV: &str = "nenv";

    pub(crate) fn parse(&mut self) -> Result<(), ParseErr> {
        let mut envupv = Upval::new(Parser::ENV);
        envupv.instack = true;
        envupv.kind = VarKind::REG;
        envupv.idx = 0;

        self.new_upvar(envupv);

        self.lexer.token_next()?; // read first token

        self.stmtlist()?;

        consume_token!(self, Token::EOF)?;

        Ok(())
    }
}
