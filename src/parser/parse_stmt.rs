use crate::{
    code::InterCode,
    lexer::Token,
    match_token,
    object::{Expr, ExprValue, Var, VarKind},
};

use super::{ParseErr, ParseLex, Parser};

pub(super) struct ParseStmt<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseStmt<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
    }

    pub(super) fn block_follow(&self, withuntil: bool) -> bool {
        if match_token!(test: self.p, Token::Else | Token::ElseIf | Token::End | Token::EOF) {
            true
        } else if match_token!(test: self.p, Token::Until) {
            withuntil
        } else {
            false
        }
    }

    // test_then_block ::= [ `if` | `elseif` ] cond_exp `then` block_stmt
    pub(super) fn test_then_block(
        &mut self,
        escape: Option<usize>,
    ) -> Result<Option<usize>, ParseErr> {
        log::debug!("parse test_then_block_stmt");

        // parse `if` | `elseif`
        self.p.plex().skip()?;
        // parse cond_exp
        let exp = self.p.pexp().expr_exp()?;
        // parse `then`
        match_token!(consume: self.p, Token::Then)?;
        let jf = if match_token!(test: self.p, Token::Break) {
            let exp = self.p.pexp().goiffalse(exp)?;
            // parse `break`
            self.p.plex().skip()?;

            self.p.pfscope().enterblock(false);

            match exp.true_jumpto {
                Some(pc) => self.p.pexp().creategoto("break", pc)?,
                _ => return Err(ParseErr::BadUsage),
            }
            // parse `;`
            while match_token!(test_consume: self.p, Token::Operator(';')) {}

            if self.block_follow(false) {
                self.p.pfscope().leaveblock()?;
                return Ok(escape);
            } else {
                Some(self.p.emiter().emit_jmp())
            }
        } else {
            let exp = self.p.pexp().goiftrue(exp)?;
            self.p.pfscope().enterblock(false);

            exp.false_jumpto
        };
        // parse block_stmt
        self.stmtlist()?;

        self.p.pfscope().leaveblock()?;

        let mut escape = escape;
        if match_token!(test: self.p, Token::Else | Token::ElseIf) {
            let pc = self.p.emiter().emit_jmp();
            self.p.pexp().jump_concatlist(&mut escape, Some(pc))?;
        }
        self.p.pexp().jump_patchtohere(jf)?;

        Ok(escape)
    }

    pub(super) fn block_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse block_stmt");

        self.p.pfscope().enterblock(false);

        self.stmtlist()?;

        self.p.pfscope().leaveblock()?;

        Ok(())
    }

    // if_stmt ::=  `if` cond_exp `then` block_stmt
    //              { `elseif` cond_exp `then` block_stmt }
    //              [ `else` block_stmt ]
    //              `end`
    pub(super) fn if_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse if_stmt");

        let mut escape = None;

        // parse `if` cond_exp `then` block_stmt
        escape = self.test_then_block(escape)?;
        // parse `elseif` cond_exp `then` block_stmt
        while match_token!(test: self.p, Token::ElseIf) {
            escape = self.test_then_block(escape)?;
        }
        // parse `else` block
        if match_token!(test_consume: self.p, Token::Else) {
            self.block_stmt()?;
        }
        // parse `end`
        match_token!(consume: self.p, Token::End)?;
        self.p.pexp().jump_patchtohere(escape)?;

        Ok(())
    }

    // while_stmt ::= `while` cond_exp `do` block_stmt `end`
    pub(super) fn while_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse where_stmt");

        let initpc = self.p.emiter().pc();

        // parse `while`
        self.p.plex().skip()?;
        // parse cond_exp
        let exitpc = self.p.pexp().cond_exp()?;
        // enter loop
        self.p.pfscope().enterblock(true);
        // parse `do`
        match_token!(consume: self.p, Token::Do)?;
        // parse block_stmt
        self.block_stmt()?;
        // jump back
        let backpc = self.p.emiter().emit_jmp();
        self.p.pexp().jump_patchlist(Some(backpc), Some(initpc))?;
        // parse `end`
        match_token!(consume: self.p, Token::End)?;
        self.p.pfscope().leaveblock()?;
        self.p.pexp().jump_patchtohere(Some(exitpc))?;

        Ok(())
    }

    // forbody_stmt ::= `do` block_stmt
    pub(super) fn forbody_stmt(
        &mut self,
        base: usize,
        nvars: usize,
        isgen: bool,
    ) -> Result<(), ParseErr> {
        log::debug!("parse forbody_stmt");

        // parse `do`
        match_token!(consume: self.p, Token::Do)?;

        let prep = if isgen {
            self.p.emiter().emit_tforprep(base)
        } else {
            self.p.emiter().emit_forprep(base)
        };

        self.p.pfscope().enterblock(false);
        self.p.pvar().adjust_localvars(nvars);
        self.p.pvar().reserver_regs(nvars);

        // parse block_stmt
        self.block_stmt()?;

        self.p.pfscope().leaveblock()?;

        let pc = self.p.emiter().pc();
        self.p.pexp().patch_forjump(prep, pc, false);
        if isgen {
            self.p.emiter().emit_tforcall(base, nvars);
        }
        let endfor = if isgen {
            self.p.emiter().emit_tforloop(base)
        } else {
            self.p.emiter().emit_forloop(base)
        };
        self.p.pexp().patch_forjump(endfor, prep + 1, true);

        Ok(())
    }

    // fornum_stmt ::=  name `=` exp `,` exp [`,` exp] forbody_stmt
    pub(super) fn fornum_stmt(&mut self, name: &str) -> Result<(), ParseErr> {
        log::debug!("parse fornum_stmt");

        let base = self.p.fs.prop().freereg;

        self.p.pvar().pushloc(Var::new("(for state)"));
        self.p.pvar().pushloc(Var::new("(for state)"));
        self.p.pvar().pushloc(Var::new("(for state)"));
        self.p.pvar().pushloc(Var::new(name));

        // parse `=`
        match_token!(consume: self.p, Token::Operator('='))?;
        // parse exp (initial value)
        self.p.pexp().exprtoreg_exp()?;
        // parse `,`
        match_token!(consume: self.p, Token::Operator(','))?;
        // parse exp (limit value)
        self.p.pexp().exprtoreg_exp()?;
        // parse [ `,` exp ] (step value)
        if match_token!(test_consume: self.p, Token::Operator(',')) {
            // parse exp (step value)
            self.p.pexp().exprtoreg_exp()?;
        } else {
            let freereg = self.p.fs.prop().freereg;

            // default step 1
            self.p.emiter().emit_loadint(freereg, 1);
            self.p.pvar().reserver_regs(1);
        }

        self.p.pvar().adjust_localvars(3);
        // parse forbody_stmt
        self.forbody_stmt(base, 1, false)?;

        Ok(())
    }

    // forlist_stmt ::= name { `,` name } `in` explist_exp forbody_stmt
    pub(super) fn forlist_stmt(&mut self, name: &str) -> Result<(), ParseErr> {
        log::debug!("parse forlist_stmt");

        let base = self.p.fs.prop().freereg;

        self.p.pvar().pushloc(Var::new("(for state)"));
        self.p.pvar().pushloc(Var::new("(for state)"));
        self.p.pvar().pushloc(Var::new("(for state)"));
        self.p.pvar().pushloc(Var::new("(for state)"));

        self.p.pvar().pushloc(Var::new(name));

        let mut nvars = 5;

        // parse { `,` name }
        while match_token!(test_consume: self.p, Token::Operator(',')) {
            let name = self.p.plex().name()?;
            self.p.pvar().pushloc(Var::new(&name));
            nvars += 1;
        }
        // parse `in`
        match_token!(consume: self.p, Token::In)?;
        // parse explist_exp
        let (nexps, exp) = self.p.pexp().exprlist_exp()?;

        self.p.pvar().adjust_assign(4, nexps, exp)?;
        self.p.pvar().adjust_localvars(4);

        self.p.fs.prop().block.prop_mut().upval = true;
        self.p.fs.prop().block.prop_mut().inside_tobeclosed = true;
        self.p.fs.prop().block.prop_mut().needclose = true;

        // parse forbody_stmt
        self.forbody_stmt(base, nvars - 4, true)?;

        Ok(())
    }

    // for_stmt ::= `for` (fornum_stmt | forlist_stmt) `end`
    pub(super) fn for_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse for_stmt");

        self.p.pfscope().enterblock(true);

        // parse `for`
        self.p.plex().skip()?;
        // parse name
        let var_name = ParseLex::new(self.p).name()?;
        // parse (fornum_stmt | forlist_stmt)
        match self.p.lex(|x| x.token.clone()) {
            Token::Operator('=') => self.fornum_stmt(&var_name)?,
            Token::Operator(',') | Token::In => self.forlist_stmt(&var_name)?,
            _ => return Err(ParseErr::UnexpectedSymbol),
        }
        // parse `end`
        match_token!(consume: self.p, Token::End)?;

        self.p.pfscope().leaveblock()?;

        Ok(())
    }

    // do_stmt ::= `do` block_stmt `end`
    pub(super) fn do_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse do_stmt");

        // parse `do`
        self.p.plex().skip()?;
        // parse block_stmt
        self.block_stmt()?;
        // parse `end`
        match_token!(consume: self.p, Token::End)?;

        Ok(())
    }

    // repeat_stmt ::= `repeat` block_stmt `until` cond_exp
    pub(super) fn repeat_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse repeat_stmt");

        let initpc = self.p.emiter().pc();
        self.p.pfscope().enterblock(true);
        self.p.pfscope().enterblock(false);

        // parse `repeat`
        self.p.plex().skip()?;
        // parse block_stmt
        self.stmtlist()?;
        // parse `until`
        match_token!(consume: self.p, Token::Until)?;
        // parse cond_exp
        let mut condexit = self.p.pexp().cond_exp()?;

        let upval = self.p.fs.prop().block.prop().upval;
        let nactvar = self.p.fs.prop().block.prop().nactvar;
        self.p.pfscope().leaveblock()?;

        if upval {
            let exit = self.p.emiter().emit_jmp();
            self.p.pexp().jump_patchtohere(Some(condexit))?;
            self.p.emiter().emit_close(nactvar);
            condexit = self.p.emiter().emit_jmp();
            self.p.pexp().jump_patchtohere(Some(exit))?;
        }

        self.p.pexp().jump_patchlist(Some(condexit), Some(initpc))?;
        self.p.pfscope().leaveblock()?;

        Ok(())
    }

    // parlist_stmt ::= [ {name `,`} (name | `...`) ]
    pub(super) fn parlist_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse parlist_stmt");

        let mut nparams = 0;
        let mut isvararg = false;
        if !match_token!(test: self.p, Token::Operator(')')) {
            loop {
                match self.p.lex(|x| x.token.clone()) {
                    // parse name
                    Token::Name(name) => {
                        self.p.pvar().pushloc(Var::new(name.as_str()));
                        self.p.plex().skip()?;
                        nparams += 1;
                    }
                    // parse `...`
                    Token::Dots => {
                        self.p.plex().skip()?;
                        isvararg = true;
                    }
                    _ => return Err(ParseErr::BadUsage),
                }

                // parse `,`
                if isvararg || !match_token!(test_consume: self.p, Token::Operator(',')) {
                    break;
                }
            }
        }

        self.p.pvar().adjust_localvars(nparams);
        let nactvar = self.p.fs.prop().nactvar;

        self.p.fs.prop().proto.prop_mut().nparams = nactvar;
        if isvararg {
            self.p.fs.prop().proto.prop_mut().vararg = true;
            self.p.emiter().emit_varargprep(nparams);
        }
        self.p.pvar().reserver_regs(nactvar);

        Ok(())
    }

    // body_stmt ::= `(` parlist `)` block_stmt `end`
    pub(super) fn body_stmt(&mut self, ismethod: bool) -> Result<Expr, ParseErr> {
        log::debug!("parse body_stmt");

        self.p.pfscope().enterfunc();

        // parse `(`
        match_token!(consume: self.p, Token::Operator('('))?;
        if ismethod {
            self.p.pvar().pushloc(Var::new("self"));
            self.p.pvar().adjust_localvars(1);
        }
        // parse parlist_stmt
        self.parlist_stmt()?;
        // parse `)`
        match_token!(consume: self.p, Token::Operator(')'))?;
        // parse block_stmt
        self.stmtlist()?;
        // parse `end`
        match_token!(consume: self.p, Token::End)?;

        let pfscope = self.p.fs.prop().prev.clone();
        let exp = if let Some(pfscope) = pfscope {
            let closureid = pfscope.prop().proto.prop().children_proto.len() - 1;
            let pc = self.p.emiter().emit_closure(closureid)?;

            Expr::reloc(pc)
        } else {
            return Err(ParseErr::BadUsage);
        };
        self.p.pfscope().leavefunc()?;

        self.p.pvar().exp_tonextreg(exp)
    }

    // fieldsel_stmt -> [`.` | `:`] name
    pub(super) fn fieldsel_stmt(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        log::debug!("parse fieldsel_stmt");

        let exp = self.p.pvar().exp_toanyregup(exp)?;

        // parse [`.` | `:`]
        self.p.plex().skip()?;
        // parse name
        let name = self.p.plex().name()?;

        self.p.pvar().indexed(exp, Expr::from(name.as_str()))
    }

    // funcname_stmt -> name { fieldsel_stmt } [ `:` name ]
    pub(super) fn funcname_stmt(&mut self) -> Result<(bool, Expr), ParseErr> {
        log::debug!("parse funcname_stmt");

        // parse name
        let name = self.p.plex().name()?;

        let mut exp = self.p.pvar().single_var(&name)?;
        log::debug!("funcname exp: {}", exp.value);

        // parse { fieldsel_stmt }
        while match_token!(test: self.p, Token::Operator('.')) {
            exp = self.fieldsel_stmt(exp)?;
        }
        let mut ismethod = false;
        // parse [ `:` name ]
        if match_token!(test: self.p, Token::Operator(':')) {
            exp = self.fieldsel_stmt(exp)?;
            ismethod = true;
        }

        Ok((ismethod, exp))
    }

    // func_stmt := `function` funcname_stmt body_stmt
    pub(super) fn func_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse func_stmt");

        // parse `function`
        self.p.plex().skip()?;
        // parse funcname_stmt
        let (ismethod, nexp) = self.funcname_stmt()?;
        // parse body_stmt
        let bexp = self.body_stmt(ismethod)?;

        // store var
        self.p.pvar().store_var(&nexp, bexp)?;

        match nexp.value {
            ExprValue::IndexStr(reg, _) => self.p.pvar().free_reg(reg)?,
            _ => {}
        }

        Ok(())
    }

    // localfunc_stmt ::= name body_stmt
    pub(super) fn localfunc_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse localfunc_stmt");

        let fvar = self.p.fs.prop().nactvar;
        // parse name
        let name = self.p.plex().name()?;
        self.p.pvar().pushloc(Var::new(&name));
        self.p.pvar().adjust_localvars(1);

        // parse body_stmt
        self.body_stmt(false)?;

        let pc = self.p.emiter().pc();
        if let Some(var) = self.p.fs.prop().proto.prop_mut().locvars.get_mut(fvar) {
            var.start_pc = pc;
        }

        Ok(())
    }

    // attrib_stmt ::= [`<` name `>`]
    pub(super) fn attrib_stmt(&mut self, name: &str) -> Result<Var, ParseErr> {
        log::debug!("parse attrib_stmt");

        let mut var = Var::new(name);

        // parse `<`
        if match_token!(test_consume: self.p, Token::Operator('<')) {
            // parse name
            let attr = self.p.plex().name()?;

            // parse `>`
            match_token!(consume: self.p, Token::Operator('>'))?;

            var.kind = match attr.as_str() {
                "const" => VarKind::CONST,
                "close" => VarKind::TOCLOSE,
                _ => return Err(ParseErr::BadUsage),
            }
        }

        Ok(var)
    }

    // localvar_stmt ::= name attrib_stmt { `,` name attrib_stmt }[ `=` exprlist_exp ]
    pub(super) fn localvar_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse localvar_stmt");

        let mut toclose = None;
        let mut nvars = 0;

        let vlatestidx = loop {
            // parse name
            let name = self.p.plex().name()?;
            // parse attrib_stmt
            let var = self.attrib_stmt(&name)?;

            if matches!(var.kind, VarKind::TOCLOSE) {
                toclose = match toclose {
                    None => Some(self.p.fs.prop().nactvar + nvars),
                    Some(_) => return Err(ParseErr::BadUsage),
                }
            }

            let vidx = self.p.pvar().pushloc(var);
            nvars += 1;

            // parse `,`
            if !match_token!(test_consume: self.p, Token::Operator(',')) {
                break vidx;
            }
        };

        // parse `=`
        let (nexps, exp) = if match_token!(test_consume: self.p, Token::Operator('=')) {
            // parse exprlist_exp
            self.p.pexp().exprlist_exp()?
        } else {
            (0, Expr::void())
        };

        let var = self.p.pvar().getloc(vlatestidx);
        let latestvalue =
            if nvars == nexps && matches!(var.and_then(|v| Some(v.kind)), Some(VarKind::CONST)) {
                self.p.pvar().exp_torefvalue(&exp)
            } else {
                Err(ParseErr::BadUsage)
            };

        if let Ok(val) = latestvalue {
            match self.p.pvar().getloc_mut(vlatestidx) {
                Some(v) => {
                    v.kind = VarKind::CTC;
                    v.val = val;
                }
                _ => return Err(ParseErr::BadUsage),
            }

            self.p.pvar().adjust_localvars(nvars - 1);
            self.p.fs.prop_mut().nactvar += 1;
        } else {
            self.p.pvar().adjust_assign(nvars, nexps, exp)?;
            self.p.pvar().adjust_localvars(nvars);
        }

        if let Some(level) = toclose {
            self.p.fs.prop().block.prop_mut().upval = true;
            self.p.fs.prop().block.prop_mut().inside_tobeclosed = true;
            self.p.fs.prop().block.prop_mut().needclose = true;

            let vidx = self.p.pvar().reglevel(level);
            self.p.emiter().emit_tbc(vidx);
        }

        Ok(())
    }

    // local_stmt ::= `local` [localfunc_stmt | localvar_stmt]
    pub(super) fn local_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse local_stmt");

        // parse `local`
        self.p.plex().skip()?;
        // parse `function`
        if match_token!(test_consume: self.p, Token::Function) {
            // parse localfunc_stmt
            self.localfunc_stmt()
        } else {
            // parse localvar_stmt
            self.localvar_stmt()
        }
    }

    // label_stmt ::= `::` name `::`
    pub(super) fn label_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse label_stmt");

        // parse `::`
        self.p.plex().skip()?;
        // parse name
        let name = self.p.plex().name()?;
        // parse `::`
        match_token!(consume: self.p, Token::Label)?;

        // parse next stmt
        while match_token!(test: self.p, Token::Operator(';') | Token::Label) {
            self.stmt()?;
        }
        let islast = self.block_follow(false);

        log::debug!("label_stmt islast == {}", islast);

        self.p.pexp().repeated(&name)?;
        self.p.pexp().createlabel(&name, islast)?;

        Ok(())
    }

    // return_stmt ::= `return` [explist_exp] [`;`]
    pub(super) fn return_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse return_stmt");

        // parse `return`
        self.p.plex().skip()?;

        let mut first = self.p.pvar().instack_nvars();
        let nret = if self.block_follow(true) || match_token!(test: self.p, Token::Operator(';')) {
            0
        } else {
            // parse exprlist_exp
            let (mut nret, exp) = self.p.pexp().exprlist_exp()?;

            if exp.hasmultret() {
                self.p.pexp().setreturns(&exp, 254)?;
                match exp.value {
                    ExprValue::Call(pc)
                        if nret == 1 && !self.p.fs.prop().block.prop().inside_tobeclosed =>
                    {
                        self.p.emiter().modify_code(pc, |c| {
                            *c = match *c {
                                InterCode::CALL(ra, rb, rc) => InterCode::TAILCALL(ra, rb, rc),
                                _ => *c,
                            }
                        });
                    }
                    _ => {}
                }

                nret = 255;
            } else {
                if nret == 1 {
                    let exp = self.p.pvar().exp_toanyreg(exp)?;
                    first = self.p.pvar().locreg(&exp)?;
                } else {
                    self.p.pvar().exp_toanyreg(exp)?;
                }
            }

            nret
        };
        self.p.pexp().ret(first, nret);

        // parse `;`
        match_token!(test_consume: self.p, Token::Operator(';'));

        Ok(())
    }

    // break_stmt ::= `break`
    pub(super) fn break_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse break_stmt");

        // parse `break`
        self.p.plex().skip()?;

        let pc = self.p.emiter().pc();
        self.p.pexp().creategoto("break", pc)
    }

    // goto_stmt ::= `goto` name
    pub(super) fn goto_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse goto_stmt");

        // parse `goto`
        self.p.plex().skip()?;

        // parse name
        let name = self.p.plex().name()?;

        if let Some(label) = self.p.pexp().findlabel(&name) {
            let blnactvar = self.p.fs.prop().block.prop().nactvar;
            let bllevel = self.p.pvar().reglevel(blnactvar);

            if self.p.pvar().instack_nvars() > bllevel {
                self.p.emiter().emit_close(bllevel);
            }

            let pc = self.p.emiter().emit_jmp();
            self.p.pexp().jump_patchlist(Some(pc), Some(label.pc))?;
        } else {
            let pc = self.p.emiter().pc();
            self.p.pexp().creategoto(&name, pc)?;
        }

        Ok(())
    }

    // assigment_stmt ::= suffixed_exp restassign_stmt
    // restassign_stmt ::= ',' suffixed_exp restassign_stmt | `=` explist_exp
    pub(super) fn restassign_stmt(&mut self, assignlist: &mut Vec<Expr>) -> Result<(), ParseErr> {
        log::debug!("parse restassign_stmt");

        // parse `,`
        if match_token!(test_consume: self.p, Token::Operator(',')) {
            // parse suffixed_exp
            let exp = self.p.pexp().suffixed_exp()?;
            assignlist.push(exp);

            // parse restassign_stmt
            self.restassign_stmt(assignlist)?;
        } else {
            // parse `=`
            match_token!(consume: self.p, Token::Operator('='))?;

            // parse exprlist_exp
            let (nexps, exp) = self.p.pexp().exprlist_exp()?;
            if nexps != assignlist.len() {
                self.p.pvar().adjust_assign(assignlist.len(), nexps, exp)?;
            } else {
                let exp = self.p.pexp().setonereturn(exp)?;
                match assignlist.last() {
                    Some(var) => self.p.pvar().store_var(var, exp)?,
                    _ => return Err(ParseErr::BadUsage),
                }

                return Ok(());
            }
        }

        let exp = Expr::nonreloc(self.p.fs.prop().freereg - 1);
        match assignlist.last() {
            Some(var) => self.p.pvar().store_var(var, exp)?,
            _ => return Err(ParseErr::BadUsage),
        }

        Ok(())
    }

    // expr_stmt -> func_stmt | assigment_stmt
    pub(super) fn expr_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse expr_stmt");

        let mut assignlist = Vec::<Expr>::new();

        // parse suffixed_exp
        let exp = self.p.pexp().suffixed_exp()?;
        assignlist.push(exp);

        if match_token!(test: self.p, Token::Operator('=' | ',')) {
            // assignment_stmt

            self.restassign_stmt(&mut assignlist)?;
        } else {
            // func_stmt

            match assignlist.last().and_then(|e| Some(&e.value)) {
                Some(ExprValue::Call(pc)) => self.p.emiter().set_rc(*pc, 1),
                _ => {}
            }
        }

        Ok(())
    }

    pub(super) fn stmt(&mut self) -> Result<(), ParseErr> {
        match self.p.lex(|x| x.token.clone()) {
            Token::Operator(';') => self.p.plex().skip(),
            Token::Local => self.local_stmt(),
            Token::Label => self.label_stmt(),
            Token::Break => self.break_stmt(),
            Token::Goto => self.goto_stmt(),
            Token::Do => self.do_stmt(),
            Token::While => self.while_stmt(),
            Token::Repeat => self.repeat_stmt(),
            Token::For => self.for_stmt(),
            Token::If => self.if_stmt(),
            Token::Function => self.func_stmt(),
            Token::Return => self.return_stmt(),
            _ => self.expr_stmt(),
        }
    }

    pub(super) fn stmtlist(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse stmtlist");

        while self.p.lex(|x| !x.token.block_follow(true)) {
            if self.p.lex(|x| matches!(x.token, Token::Return)) {
                self.stmt()?;
                break;
            }
            self.stmt()?;
        }
        Ok(())
    }
}
