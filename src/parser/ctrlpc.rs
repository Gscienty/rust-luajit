use crate::{
    check_token, consume_token,
    lexer::Token,
    matches_token,
    object::{LabelDesc, Var, VarKind},
};

use super::{block::Block, ParseErr, Parser};

impl Parser {
    fn slovegoto(&mut self, goto_pc: usize, label_pc: usize) -> Result<(), ParseErr> {
        self.jump_patchlistaux(Some(goto_pc), Some(label_pc), None, Some(label_pc))
    }

    fn slovegotos(&mut self, label: &LabelDesc) -> Result<bool, ParseErr> {
        let mut i = self.fs.prop().block.prop().first_goto;

        let mut needsclose = false;
        while i < self.goto.len() {
            match self.goto.get(i) {
                Some(goto) if goto.name.eq(&label.name) => {
                    needsclose |= goto.close;
                    self.slovegoto(goto.pc, label.pc)?;
                    self.goto.remove(i);
                }
                _ => {
                    i += 1;
                }
            }
        }

        Ok(needsclose)
    }

    pub(super) fn createlabel(&mut self, name: &str, last: bool) -> Result<bool, ParseErr> {
        let pc = self.mark_pc();
        let nactvar = if last {
            self.fs.prop().block.prop().nactvar
        } else {
            self.fs.prop().nactvar
        };

        let label = LabelDesc::new(name, pc, nactvar);
        let result = if self.slovegotos(&label)? {
            let nactvar = self.nvarstack();
            self.emit_close(nactvar);
            true
        } else {
            false
        };
        self.label.push(label);

        Ok(result)
    }

    pub(super) fn creategoto(&mut self, name: &str, pc: usize) -> Result<(), ParseErr> {
        let nactvar = self.fs.prop().nactvar;
        let goto = LabelDesc::new(name, pc, nactvar);

        self.goto.push(goto);

        Ok(())
    }

    pub(super) fn movegotosout(&mut self, block: &Block) -> Result<(), ParseErr> {
        let block_level = self.reglevel(block.prop().nactvar);

        for i in block.prop().first_goto..self.goto.len() {
            let goto = if let Some(goto) = self.goto.get(i) {
                goto.clone()
            } else {
                continue;
            };
            let goto_level = self.reglevel(goto.nactvar);

            if let Some(goto) = self.goto.get_mut(i) {
                if goto_level > block_level {
                    goto.close |= block.prop().upval;
                }
                goto.nactvar = block.prop().nactvar;
            }
        }

        Ok(())
    }

    fn findlabel(&self, name: &str) -> Option<LabelDesc> {
        let first_label = self.fs.prop().first_label;
        let len = self.label.len();

        for i in first_label..len {
            match self.label.get(i) {
                Some(l) if l.name.eq(name) => return Some(l.clone()),
                _ => {}
            }
        }
        None
    }

    fn repeated(&self, name: &str) -> Result<(), ParseErr> {
        if matches!(self.findlabel(name), Some(..)) {
            Err(ParseErr::BadUsage)
        } else {
            Ok(())
        }
    }

    // label_stmt ::= `::` name `::`
    pub(super) fn label_stmt(&mut self) -> Result<(), ParseErr> {
        self.skip()?;
        let name = self.name()?;
        consume_token!(self, Token::Label)?;

        while matches_token!(self, Token::Operator(';') | Token::Label) {
            self.stmt()?;
        }

        let last = self.block_follow(false);

        self.repeated(&name)?;
        self.createlabel(&name, last)?;

        Ok(())
    }

    // break_stmt ::= `break`
    pub(super) fn break_stmt(&mut self) -> Result<(), ParseErr> {
        self.skip()?;

        let pc = self.pc();
        self.creategoto("break", pc)?;

        Ok(())
    }

    // goto_stmt ::= `goto` name
    pub(super) fn goto_stmt(&mut self) -> Result<(), ParseErr> {
        self.skip()?;
        let name = self.name()?;

        if let Some(label) = self.findlabel(&name) {
            let nactvar = self.fs.prop().block.prop().nactvar;
            let level = self.reglevel(nactvar);

            if self.nvarstack() > level {
                self.emit_close(level);
            }

            let pc = self.emit_jmp();
            self.jump_patchlist(Some(pc), Some(label.pc))?;
        } else {
            let pc = self.pc();
            self.creategoto(&name, pc)?;
        }

        Ok(())
    }

    // while_stmt ::= `while` cond_exp `do` block_stmt `end`
    pub(super) fn while_stmt(&mut self) -> Result<(), ParseErr> {
        self.skip()?;

        let startpc = self.pc();
        let exitpc = self.cond_exp()?;

        self.enterblock(true);
        consume_token!(self, Token::Do)?;
        self.block_stmt()?;
        consume_token!(self, Token::End)?;

        let backpc = self.emit_jmp();
        self.jump_patchlist(Some(backpc), Some(startpc))?;
        self.leaveblock()?;
        self.jump_patchtohere(Some(exitpc))?;

        Ok(())
    }

    // repeat_stmt ::= `repeat` block_stmt `until` cond_exp
    pub(super) fn repeat_stmt(&mut self) -> Result<(), ParseErr> {
        self.skip()?;

        let startpc = self.pc();

        self.enterblock(true);
        self.enterblock(false);

        self.stmtlist()?;

        consume_token!(self, Token::Until)?;

        let mut condexitpc = self.cond_exp()?;

        let upvar = self.fs.prop().block.prop().upval;
        let nactvar = self.fs.prop().block.prop().nactvar;

        self.leaveblock()?;

        if upvar {
            let exitpc = self.emit_jmp();
            self.jump_patchtohere(Some(condexitpc))?;
            self.emit_close(nactvar);
            condexitpc = self.emit_jmp();
            self.jump_patchtohere(Some(exitpc))?;
        }

        self.jump_patchlist(Some(condexitpc), Some(startpc))?;
        self.leaveblock()?;

        Ok(())
    }

    // thenblock_stmt ::= [ `if` | `elseif` ] exp `then` block_stmt
    fn thenblock_stmt(&mut self, esc: Option<usize>) -> Result<Option<usize>, ParseErr> {
        self.skip()?;
        let cond = self.exp()?;
        consume_token!(self, Token::Then)?;

        let jf = if matches_token!(self, Token::Break) {
            let exp = self.goiftrue(cond)?;
            self.skip()?;
            self.enterblock(false);

            if let Some(pc) = exp.true_jumpto {
                self.creategoto("break", pc)?;
            } else {
                return Err(ParseErr::BadUsage);
            }

            while check_token!(self, Token::Operator(';')) {}

            if self.block_follow(false) {
                self.leaveblock()?;
                return Ok(esc);
            }

            Some(self.emit_jmp())
        } else {
            let exp = self.goiftrue(cond)?;
            self.enterblock(false);

            exp.false_jumpto
        };

        self.stmtlist()?;
        self.leaveblock()?;

        let mut esc = esc;
        if matches_token!(self, Token::Else | Token::ElseIf) {
            let pc = self.emit_jmp();
            self.jump_concatlist(&mut esc, Some(pc))?;
        }
        self.jump_patchtohere(jf)?;

        Ok(esc)
    }

    // if_stmt ::= `if` cond_exp `then` block_stmt
    //              { `elseif` cond_exp `then` block_stmt }
    //              [ `else` block_stmt ]
    //              `end`
    pub(super) fn if_stmt(&mut self) -> Result<(), ParseErr> {
        let mut esc = self.thenblock_stmt(None)?;

        while matches_token!(self, Token::ElseIf) {
            esc = self.thenblock_stmt(esc)?;
        }
        if check_token!(self, Token::Else) {
            self.block_stmt()?;
        }
        consume_token!(self, Token::End)?;

        self.jump_patchtohere(esc)?;

        Ok(())
    }

    // forbody_stmt ::= `do` block_stmt
    fn forbody_stmt(&mut self, base: usize, nvars: usize, isnum: bool) -> Result<(), ParseErr> {
        consume_token!(self, Token::Do)?;

        let startpc = if isnum {
            self.emit_forprep(base)
        } else {
            self.emit_tforprep(base)
        };

        self.enterblock(false);

        self.adjlocalvars(nvars);
        self.reserve(nvars);

        self.block_stmt()?;

        self.leaveblock()?;

        let lastpc = self.pc();
        self.patch_forjump(startpc, lastpc, false);
        if !isnum {
            self.emit_tforcall(base, nvars);
        }

        let exitpc = if isnum {
            self.emit_forloop(base)
        } else {
            self.emit_tforloop(base)
        };
        self.patch_forjump(exitpc, startpc + 1, true);

        Ok(())
    }

    // fornum_stmt ::= name `=` exp `,` exp [ `,` exp ] forbody_stmt
    fn fornum_stmt(&mut self, name: String) -> Result<(), ParseErr> {
        let base = self.fs.prop().freereg;

        self.new_localvar(Var::new(String::from("(init)"), VarKind::REG));
        self.new_localvar(Var::new(String::from("(limit)"), VarKind::REG));
        self.new_localvar(Var::new(String::from("(step)"), VarKind::REG));
        self.new_localvar(Var::new(name, VarKind::REG));

        consume_token!(self, Token::Operator('='))?;
        self.expsetreg()?;
        consume_token!(self, Token::Operator(','))?;
        self.expsetreg()?;
        if check_token!(self, Token::Operator(',')) {
            self.expsetreg()?;
        } else {
            let reg = self.fs.prop().freereg;
            self.emit_loadint(reg, 1);
            self.reserve(1);
        }

        self.adjlocalvars(3);
        self.forbody_stmt(base, 1, true)?;

        Ok(())
    }

    // forlist_stmt ::= name { `,` name } `in` explist_exp forbody_stmt
    fn forlist_stmt(&mut self, name: String) -> Result<(), ParseErr> {
        let base = self.fs.prop().freereg;

        self.new_localvar(Var::new(String::from("(gen)"), VarKind::REG));
        self.new_localvar(Var::new(String::from("(state)"), VarKind::REG));
        self.new_localvar(Var::new(String::from("(ctrl)"), VarKind::REG));
        self.new_localvar(Var::new(String::from("(toclose)"), VarKind::REG));
        self.new_localvar(Var::new(name, VarKind::REG));

        let mut nvars = 5;
        while check_token!(self, Token::Operator(',')) {
            let name = self.name()?;
            self.new_localvar(Var::new(name, VarKind::REG));

            nvars += 1;
        }
        consume_token!(self, Token::In)?;

        let (nexp, exp) = self.explist_exp()?;
        self.adjassign(4, nexp, exp)?;
        self.adjlocalvars(4);

        self.fs.prop().block.prop_mut().upval = true;
        self.fs.prop().block.prop_mut().inside_tobeclosed = true;
        self.fs.prop().block.prop_mut().needclose = true;

        self.forbody_stmt(base, nvars - 4, false)?;

        Ok(())
    }

    // for_stmt ::= `for` (fornum_stmt | forlist_stmt) `end`
    pub(super) fn for_stmt(&mut self) -> Result<(), ParseErr> {
        self.skip()?;

        self.enterblock(true);

        let name = self.name()?;

        match &self.lexer.token {
            Token::Operator('=') => self.fornum_stmt(name)?,
            Token::Operator(',') | Token::In => self.forlist_stmt(name)?,
            _ => return Err(ParseErr::BadUsage),
        }
        consume_token!(self, Token::End)?;

        self.leaveblock()?;

        Ok(())
    }
}
