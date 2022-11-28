use std::{cell::RefCell, rc::Rc};

use crate::{
    code::InterCode,
    lexer::Lexer,
    object::{ConstantPool, LabelDesc, RefValue, Table, Var},
};

use super::{
    Emiter, FuncState, ParseCode, ParseErr, ParseExpr, ParseFunc, ParseGTab, ParseLex, ParseReg,
    ParseStmt, ParseVar,
};

pub(crate) struct Parser {
    gtab: RefValue,
    pub(super) nkgc: usize,
    pub(super) nkn: usize,

    gfs: FuncState,
    pub(super) fs: FuncState,

    lexer: Lexer,

    envn: String,

    pub(super) actvar: Vec<Var>,
    goto: Vec<LabelDesc>,
    label: Vec<LabelDesc>,

    pub(super) codes: Vec<InterCode>,
    pub(super) last_target: usize,

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

    pub(super) fn env_name(&self) -> &str {
        &self.envn
    }

    pub(super) fn labelcnt(&self) -> usize {
        self.label.len()
    }

    pub(super) fn gotocnt(&self) -> usize {
        self.goto.len()
    }

    pub(super) fn emiter(&mut self) -> Emiter {
        Emiter::new(self)
    }

    pub(super) fn plex(&mut self) -> ParseLex {
        ParseLex::new(self)
    }

    pub(super) fn pstmt<'s>(&'s mut self) -> ParseStmt {
        ParseStmt::new(self)
    }

    pub(super) fn pcode<'s>(&'s mut self) -> ParseCode {
        ParseCode::new(self)
    }

    pub(super) fn preg<'s>(&'s mut self) -> ParseReg {
        ParseReg::new(self)
    }

    pub(super) fn pvar<'s>(&'s mut self) -> ParseVar {
        ParseVar::new(self)
    }

    pub(super) fn pexp<'s>(&'s mut self) -> ParseExpr {
        ParseExpr::new(self)
    }

    pub(super) fn pfscope<'s>(&'s mut self) -> ParseFunc {
        ParseFunc::new(self)
    }

    pub(super) fn pgtab<'s>(&'s mut self) -> ParseGTab {
        ParseGTab::new(self)
    }

    pub(crate) fn parse(&mut self) -> Result<(), ParseErr> {
        self.lexer.token_next(); // read first token

        self.pstmt().stmtlist()
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::Parser, utils::Logger};

    #[test]
    fn test_code_concat() {
        log::set_logger(&Logger {}).unwrap();
        log::set_max_level(log::LevelFilter::Debug);

        let mut p = Parser::new(
            "
                local a = 1;
                local b = 1 + 2 .. '3' + 4 * 5 / '6';
                local c = 'a'..'b' + a * b;
            ",
        );

        assert!(p.parse().is_ok());

        for ci in 0..p.emiter().pc() {
            if let Some(c) = p.emiter().get_code(ci) {
                println!("{}", c);
            }
        }
    }
}
