use crate::{
    lexer::Lexer,
    object::{ExprDesc, LabelDesc, RefValue, Table, VarDesc, VarKind},
};

use super::{
    FuncState, ParseCode, ParseErr, ParseExpr, ParseFunc, ParseGTab, ParseLex, ParseStmt, ParseVar,
};

pub(crate) struct Parser {
    gtab: RefValue,
    pub(super) nkgc: usize,
    pub(super) nkn: usize,

    gfs: FuncState,
    lexer: Lexer,

    envn: String,

    actvar: Vec<VarDesc>,
    goto: Vec<LabelDesc>,
    label: Vec<LabelDesc>,
}

impl Parser {
    pub(super) fn new(source: &str) -> Self {
        Self {
            gtab: Table::new(),
            nkgc: 0,
            nkn: 0,

            gfs: FuncState::new(),
            lexer: Lexer::new(source),

            envn: String::from("ENV"),

            actvar: Vec::new(),
            goto: Vec::new(),
            label: Vec::new(),
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

    pub(super) fn global_mut<U, F>(&mut self, f: F) -> U
    where
        F: FnOnce(&mut Table) -> U,
    {
        match self.gtab.table_mut(|table| f(table)) {
            Some(result) => result,
            _ => unreachable!(),
        }
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

    pub(super) fn getloc_abs(&self, idx: usize) -> Option<&VarDesc> {
        self.actvar.get(idx)
    }

    pub(super) fn getloc(&self, fs: &FuncState, idx: usize) -> Option<&VarDesc> {
        self.actvar.get(fs.prop().firstlocal + idx)
    }

    pub(super) fn getloc_mut(&mut self, fs: &FuncState, idx: usize) -> Option<&mut VarDesc> {
        self.actvar.get_mut(fs.prop().firstlocal + idx)
    }

    pub(super) fn pushloc(&mut self, fs: &FuncState, name: &str) -> usize {
        self.actvar.push(VarDesc::new(name, VarKind::REG));
        self.actvar.len() - 1 - fs.prop().firstlocal
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

    pub(super) fn parse_func<'s>(&'s mut self, fs: &'s mut FuncState) -> ParseFunc {
        ParseFunc::new(fs, self)
    }

    pub(super) fn parse_stmt<'s>(&'s mut self, fs: &'s mut FuncState) -> ParseStmt {
        ParseStmt::new(fs, self)
    }

    pub(super) fn parse_code<'s>(&'s mut self, fs: &'s mut FuncState) -> ParseCode {
        ParseCode::new(fs, self)
    }

    pub(super) fn parse_var<'s>(&'s mut self, fs: &'s mut FuncState) -> ParseVar {
        ParseVar::new(fs, self)
    }

    pub(super) fn parse_expr<'s>(
        &'s mut self,
        fs: &'s mut FuncState,
        expr: &'s mut ExprDesc,
    ) -> ParseExpr {
        ParseExpr::new(fs, self, expr)
    }

    pub(super) fn parse_gtab<'s>(&'s mut self) -> ParseGTab {
        ParseGTab::new(self)
    }

    pub(crate) fn parse(&mut self) -> Result<(), ParseErr> {
        self.gfs.setvararg(0)?; // main function declared vararg
        self.lexer.token_next(); // read first token

        self.parse_stmt(&mut self.gfs.clone()).stmtlist()
    }
}
