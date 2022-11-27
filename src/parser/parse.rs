use std::{cell::RefCell, rc::Rc};

use crate::{
    code::InterCode,
    lexer::Lexer,
    object::{ConstantPool, LabelDesc, RefValue, Table, VarDesc, VarKind},
};

use super::{
    Emiter, FuncState, ParseCode, ParseErr, ParseExpr, ParseGTab, ParseLex, ParseReg, ParseStmt,
    ParseVar,
};

pub(crate) struct Parser {
    gtab: RefValue,
    pub(super) nkgc: usize,
    pub(super) nkn: usize,

    gfs: FuncState,
    pub(super) fs: FuncState,

    lexer: Lexer,

    envn: String,

    actvar: Vec<VarDesc>,
    goto: Vec<LabelDesc>,
    label: Vec<LabelDesc>,

    pub(super) codes: Vec<InterCode>,
    pub(super) last_target: usize,

    pub(super) freereg: usize,
    pub(super) constant_pool: Rc<RefCell<ConstantPool>>,
}

impl Parser {
    pub(super) fn new(source: &str) -> Self {
        let gfs = FuncState::new();

        Self {
            gtab: Table::new(),
            nkgc: 0,
            nkn: 0,

            gfs: gfs.clone(),
            fs: gfs.clone(),
            lexer: Lexer::new(source),

            envn: String::from("ENV"),

            actvar: Vec::new(),
            goto: Vec::new(),
            label: Vec::new(),

            codes: Vec::new(),
            last_target: 0,

            freereg: 0,
            constant_pool: Rc::new(RefCell::new(ConstantPool::new())),
        }
    }

    pub(super) fn lex<U, F>(&self, f: F) -> U
    where
        F: FnOnce(&Lexer) -> U,
    {
        f(&self.lexer)
    }

    pub(super) fn lex_mut<U, F>(&mut self, f: F) -> U
    where
        F: FnOnce(&mut Lexer) -> U,
    {
        f(&mut self.lexer)
    }

    pub(super) fn emiter(&mut self) -> Emiter {
        Emiter::new(self)
    }

    pub(super) fn reserver_regs(&mut self, n: usize) {
        self.freereg += n
    }

    pub(super) fn free_reg(&mut self, reg: usize) -> Result<(), ParseErr> {
        self.freereg -= 1;

        if self.freereg != reg {
            return Err(ParseErr::BadUsage);
        }
        Ok(())
    }

    pub(super) fn global<U, F>(&self, f: F) -> U
    where
        F: FnOnce(&Table) -> U,
    {
        match self.gtab.table(|table| f(&table)) {
            Some(result) => result,
            _ => unreachable!(),
        }
    }

    pub(super) fn global_mut<U, F>(&mut self, f: F) -> U
    where
        F: FnOnce(&mut Table) -> U,
    {
        match self.gtab.table_mut(|table| f(table)) {
            Some(result) => result,
            _ => unreachable!(),
        }
    }

    pub(super) fn getloc_abs(&self, idx: usize) -> Option<&VarDesc> {
        self.actvar.get(idx)
    }

    pub(super) fn getloc(&self, idx: usize) -> Option<&VarDesc> {
        self.actvar.get(self.fs.prop().firstlocal + idx)
    }

    pub(super) fn getloc_mut(&mut self, idx: usize) -> Option<&mut VarDesc> {
        self.actvar.get_mut(self.fs.prop().firstlocal + idx)
    }

    pub(super) fn pushloc(&mut self, name: &str) -> usize {
        self.actvar.push(VarDesc::new(name, VarKind::REG));
        self.actvar.len() - 1 - self.fs.prop().firstlocal
    }

    pub(super) fn env_name(&self) -> &str {
        &self.envn
    }

    pub(super) fn labelcnt(&self) -> usize {
        self.label.len()
    }

    pub(super) fn gotocnt(&self) -> usize {
        self.goto.len()
    }

    pub(super) fn parse_lex(&mut self) -> ParseLex {
        ParseLex::new(self)
    }

    pub(super) fn parse_stmt<'s>(&'s mut self) -> ParseStmt {
        ParseStmt::new(self)
    }

    pub(super) fn parse_code<'s>(&'s mut self) -> ParseCode {
        ParseCode::new(self)
    }

    pub(super) fn parse_reg<'s>(&'s mut self) -> ParseReg {
        ParseReg::new(self)
    }

    pub(super) fn parse_var<'s>(&'s mut self) -> ParseVar {
        ParseVar::new(self)
    }

    pub(super) fn parse_expr<'s>(&'s mut self) -> ParseExpr {
        ParseExpr::new(self)
    }

    pub(super) fn parse_gtab<'s>(&'s mut self) -> ParseGTab {
        ParseGTab::new(self)
    }

    pub(crate) fn parse(&mut self) -> Result<(), ParseErr> {
        self.gfs.setvararg(0)?; // main function declared vararg
        self.lexer.token_next(); // read first token

        self.parse_stmt().stmtlist()
    }
}
