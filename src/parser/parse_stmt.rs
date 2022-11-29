use crate::{
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

    fn adjust_assign(&mut self, nvars: usize, nexps: usize, exp: Expr) -> Result<(), ParseErr> {
        log::debug!("adjust_assign, nvars: {}, nexps: {}", nvars, nexps);

        let needed = nvars as i32 - nexps as i32;

        if exp.hasmultret() {
            let extra = if needed + 1 >= 0 { needed + 1 } else { 0 } as usize;
            self.p.pcode().setreturns(exp, extra)?;
        } else {
            if !matches!(exp.value, ExprValue::Void) {
                self.p.preg().exp_toanyreg(exp)?;
            }

            if needed > 0 {
                let freereg = self.p.fs.prop().freereg;
                self.p.emiter().emit_loadnil(freereg, needed as usize);
            }
        }

        if needed > 0 {
            self.p.pfscope().reserver_regs(needed as usize);
        } else {
            self.p.fs.prop_mut().freereg -= needed.abs() as usize;
        }

        Ok(())
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
            let exp = self.p.pcode().goiffalse(exp)?;
            // parse `break`
            self.p.plex().skip()?;

            self.p.pfscope().enterblock(false);

            match exp.true_jumpto {
                Some(pc) => self.p.plabel().creategoto("break", pc)?,
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
            let exp = self.p.pcode().goiftrue(exp)?;
            self.p.pfscope().enterblock(false);

            exp.false_jumpto
        };
        // parse block_stmt
        self.stmtlist()?;

        self.p.pfscope().leaveblock()?;

        let mut escape = escape;
        if match_token!(test: self.p, Token::Else | Token::ElseIf) {
            let pc = self.p.emiter().emit_jmp();
            self.p.pcode().jump_concatlist(&mut escape, Some(pc))?;
        }
        self.p.pcode().jump_patchtohere(jf)?;

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
        self.p.pcode().jump_patchtohere(escape)?;

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
        self.p.pcode().jump_patchlist(Some(backpc), Some(initpc))?;
        // parse `end`
        match_token!(consume: self.p, Token::End)?;
        self.p.pfscope().leaveblock()?;
        self.p.pcode().jump_patchtohere(Some(exitpc))?;

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
        self.p.pfscope().adjust_localvars(nvars);
        self.p.pfscope().reserver_regs(nvars);

        // parse block_stmt
        self.block_stmt()?;

        self.p.pfscope().leaveblock()?;

        let pc = self.p.emiter().pc();
        self.p.pcode().patch_forjump(prep, pc, false);
        if isgen {
            self.p.emiter().emit_tforcall(base, nvars);
        }
        let endfor = if isgen {
            self.p.emiter().emit_tforloop(base)
        } else {
            self.p.emiter().emit_forloop(base)
        };
        self.p.pcode().patch_forjump(endfor, prep + 1, true);

        Ok(())
    }

    // fornum_stmt ::=  name `=` exp `,` exp [`,` exp] forbody_stmt
    pub(super) fn fornum_stmt(&mut self, name: &str) -> Result<(), ParseErr> {
        log::debug!("parse fornum_stmt");

        let base = self.p.fs.prop().freereg;

        self.p.pfscope().pushloc(Var::new("(for state)"));
        self.p.pfscope().pushloc(Var::new("(for state)"));
        self.p.pfscope().pushloc(Var::new("(for state)"));
        self.p.pfscope().pushloc(Var::new(name));

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
            self.p.pfscope().reserver_regs(1);
        }

        self.p.pfscope().adjust_localvars(3);
        // parse forbody_stmt
        self.forbody_stmt(base, 1, false)?;

        Ok(())
    }

    // forlist_stmt ::= name { `,` name } `in` explist_exp forbody_stmt
    pub(super) fn forlist_stmt(&mut self, name: &str) -> Result<(), ParseErr> {
        log::debug!("parse forlist_stmt");

        let base = self.p.fs.prop().freereg;

        self.p.pfscope().pushloc(Var::new("(for state)"));
        self.p.pfscope().pushloc(Var::new("(for state)"));
        self.p.pfscope().pushloc(Var::new("(for state)"));
        self.p.pfscope().pushloc(Var::new("(for state)"));

        self.p.pfscope().pushloc(Var::new(name));

        let mut nvars = 5;

        // parse { `,` name }
        while match_token!(test_consume: self.p, Token::Operator(',')) {
            let name = self.p.plex().name()?;
            self.p.pfscope().pushloc(Var::new(&name));
            nvars += 1;
        }
        // parse `in`
        match_token!(consume: self.p, Token::In)?;
        // parse explist_exp
        let (nexps, exp) = self.p.pexp().exprlist_exp()?;

        self.adjust_assign(4, nexps, exp)?;
        self.p.pfscope().adjust_localvars(4);

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
            self.p.pcode().jump_patchtohere(Some(condexit))?;
            self.p.emiter().emit_close(nactvar);
            condexit = self.p.emiter().emit_jmp();
            self.p.pcode().jump_patchtohere(Some(exit))?;
        }

        self.p
            .pcode()
            .jump_patchlist(Some(condexit), Some(initpc))?;
        self.p.pfscope().leaveblock()?;

        Ok(())
    }

    // parlist_stmt ::= [ {name `,`} (name | `...`) ]
    pub(super) fn parlist_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse parlist_stmt");

        let mut _nparams = 0;
        let mut isvararg = false;
        if !match_token!(test: self.p, Token::Operator(')')) {
            loop {
                match self.p.lex(|x| x.token.clone()) {
                    // parse name
                    Token::Name(name) => {
                        self.p.pfscope().pushloc(Var::new(name.as_str()));
                        self.p.plex().skip()?;
                        _nparams += 1;
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

        Ok(())
    }

    // body_stmt ::= `(` parlist `)` block_stmt `end`
    pub(super) fn body_stmt(&mut self, ismethod: bool) -> Result<(), ParseErr> {
        log::debug!("parse body_stmt");

        // parse `(`
        match_token!(consume: self.p, Token::Operator('('))?;
        if ismethod {
            self.p.pfscope().pushloc(Var::new("self"));
        }
        // parse parlist_stmt
        self.parlist_stmt()?;
        // parse `)`
        match_token!(consume: self.p, Token::Operator(')'))?;
        // parse block_stmt
        self.block_stmt()?;
        // parse `end`
        match_token!(consume: self.p, Token::End)?;
        Ok(())
    }

    // fieldsel_stmt -> [`.` | `:`] name
    pub(super) fn fieldsel_stmt(&mut self) -> Result<Expr, ParseErr> {
        log::debug!("parse fieldsel_stmt");

        // parse [`.` | `:`]
        self.p.plex().skip()?;
        // parse name
        let _name = self.p.plex().name()?;

        Ok(Expr::todo())
    }

    // funcname_stmt -> name { fieldsel_stmt } [ `:` name ]
    pub(super) fn funcname_stmt(&mut self) -> Result<(bool, Expr), ParseErr> {
        log::debug!("parse funcname_stmt");

        // parse name
        let _name = self.p.plex().name()?;
        // parse { fieldsel_stmt }
        while match_token!(test: self.p, Token::Operator('.')) {
            self.fieldsel_stmt()?;
        }
        let mut ismethod = false;
        // parse [ `:` name ]
        if match_token!(test: self.p, Token::Operator(':')) {
            self.fieldsel_stmt()?;
            ismethod = true;
        }

        Ok((ismethod, Expr::todo()))
    }

    // func_stmt := `function` funcname_stmt body_stmt
    pub(super) fn func_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse func_stmt");

        // parse `function`
        self.p.plex().skip()?;
        // parse funcname_stmt
        let (ismethod, _) = self.funcname_stmt()?;
        // parse body_stmt
        self.body_stmt(ismethod)?;

        Ok(())
    }

    // localfunc_stmt ::= name body_stmt
    pub(super) fn localfunc_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse localfunc_stmt");

        // parse name
        let _name = self.p.plex().name()?;
        // parse body_stmt
        self.body_stmt(false)?;

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

            let vidx = self.p.pfscope().pushloc(var);
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

        let var = self.p.pfscope().getloc(vlatestidx);
        let latestvalue =
            if nvars == nexps && matches!(var.and_then(|v| Some(v.kind)), Some(VarKind::CONST)) {
                self.p.preg().exp_torefvalue(&exp)
            } else {
                Err(ParseErr::BadUsage)
            };

        if let Ok(val) = latestvalue {
            match self.p.pfscope().getloc_mut(vlatestidx) {
                Some(v) => {
                    v.kind = VarKind::CTC;
                    v.val = val;
                }
                _ => return Err(ParseErr::BadUsage),
            }

            self.p.pfscope().adjust_localvars(nvars - 1);
            self.p.fs.prop_mut().nactvar += 1;
        } else {
            self.adjust_assign(nvars, nexps, exp)?;
            self.p.pfscope().adjust_localvars(nvars);
        }

        if let Some(level) = toclose {
            self.p.fs.prop().block.prop_mut().upval = true;
            self.p.fs.prop().block.prop_mut().inside_tobeclosed = true;
            self.p.fs.prop().block.prop_mut().needclose = true;

            let vidx = self.p.pfscope().reglevel(level);
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

        self.p.plabel().repeated(&name)?;
        self.p.plabel().createlabel(&name, islast)?;

        Ok(())
    }

    // return_stmt ::= `return` [explist_exp] [`;`]
    pub(super) fn return_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse return_stmt");

        // parse `return`
        self.p.plex().skip()?;

        // parse exprlist_exp
        if !self.block_follow(true) && !match_token!(test: self.p, Token::Operator(';')) {
            self.p.pexp().exprlist_exp()?;
        }

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
        self.p.plabel().creategoto("break", pc)
    }

    // goto_stmt ::= `goto` name
    pub(super) fn goto_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse goto_stmt");

        // parse `goto`
        self.p.plex().skip()?;

        // parse name
        let name = self.p.plex().name()?;

        if let Some(label) = self.p.plabel().findlabel(&name) {
            let blnactvar = self.p.fs.prop().block.prop().nactvar;
            let bllevel = self.p.pfscope().reglevel(blnactvar);

            if self.p.pfscope().nvars_stack() > bllevel {
                self.p.emiter().emit_close(bllevel);
            }

            let pc = self.p.emiter().emit_jmp();
            self.p.pcode().jump_patchlist(Some(pc), Some(label.pc))?;
        } else {
            let pc = self.p.emiter().pc();
            self.p.plabel().creategoto(&name, pc)?;
        }

        Ok(())
    }

    // assigment_stmt ::= suffixed_exp restassign_stmt
    // restassign_stmt ::= ',' suffixed_exp restassign_stmt | `=` explist_exp
    pub(super) fn restassign_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse restassign_stmt");

        // parse `,`
        if match_token!(test_consume: self.p, Token::Operator(',')) {
            // parse suffixed_exp
            self.p.pexp().suffixed_exp()?;
            // parse restassign_stmt
            self.restassign_stmt()?;
        } else {
            // parse `=`
            match_token!(consume: self.p, Token::Operator('='))?;
            // parse exprlist_exp
            self.p.pexp().exprlist_exp()?;
        }
        Ok(())
    }

    // expr_stmt -> func_stmt | assigment_stmt
    pub(super) fn expr_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse expr_stmt");

        // parse suffixed_exp
        self.p.pexp().suffixed_exp()?;

        if match_token!(test: self.p, Token::Operator('=' | ',')) {
            self.restassign_stmt()?;
        } else {
            // func
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
