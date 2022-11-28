use crate::{
    code::codelimit,
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
    pub(super) fn test_then_block(&mut self, _escape: &mut usize) -> Result<(), ParseErr> {
        log::debug!("parse test_then_block_stmt");

        // parse `if` | `elseif`
        self.p.plex().skip();
        // parse cond_exp
        self.p.pexp().expr_exp()?;
        // parse `then`
        match_token!(consume: self.p, Token::Then)?;
        // parse block_stmt
        self.block_stmt()?;

        Ok(())
    }

    pub(super) fn block_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse block_stmt");

        self.stmtlist()?;

        Ok(())
    }

    // if_stmt ::=  `if` cond_exp `then` block_stmt
    //              { `elseif` cond_exp `then` block_stmt }
    //              [ `else` block_stmt ]
    //              `end`
    pub(super) fn if_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse if_stmt");

        let mut escape_list = codelimit::NO_JMP;

        // parse `if` cond_exp `then` block_stmt
        self.test_then_block(&mut escape_list)?;
        // parse `elseif` cond_exp `then` block_stmt
        while match_token!(test: self.p, Token::ElseIf) {
            self.test_then_block(&mut escape_list)?;
        }
        // parse `else` block
        if match_token!(test_consume: self.p, Token::Else) {
            self.block_stmt()?;
        }
        // parse `end`
        match_token!(consume: self.p, Token::End)?;

        Ok(())
    }

    // while_stmt ::= `while` cond_exp `do` block_stmt `end`
    pub(super) fn while_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse where_stmt");

        // parse `while`
        self.p.plex().skip();
        // parse cond_exp
        self.p.pexp().expr_exp()?;
        // parse `do`
        match_token!(consume: self.p, Token::Do)?;
        // parse block_stmt
        self.block_stmt()?;
        // parse `end`
        match_token!(consume: self.p, Token::End)?;

        Ok(())
    }

    // forbody_stmt ::= `do` block_stmt
    pub(super) fn forbody_stmt(
        &mut self,
        _base: u32,
        _nvars: u32,
        _isgen: bool,
    ) -> Result<(), ParseErr> {
        log::debug!("parse forbody_stmt");

        // parse `do`
        match_token!(consume: self.p, Token::Do)?;
        // parse block_stmt
        self.block_stmt()?;

        Ok(())
    }

    // fornum_stmt ::=  name `=` exp `,` exp [`,` exp] forbody_stmt
    pub(super) fn fornum_stmt(&mut self, _varname: &str) -> Result<(), ParseErr> {
        log::debug!("parse fornum_stmt");

        // parse `=`
        match_token!(consume: self.p, Token::Operator('='))?;
        // parse exp (initial value)
        self.p.pexp().expr_exp()?;
        // parse `,`
        match_token!(consume: self.p, Token::Operator(','))?;
        // parse exp (end value)
        self.p.pexp().expr_exp()?;
        // parse [ `,` exp ] (step value)
        if match_token!(test_consume: self.p, Token::Operator(',')) {
            self.p.pexp().expr_exp()?;
        }
        // parse forbody_stmt
        self.forbody_stmt(0, 1, false)?;

        Ok(())
    }

    // forlist_stmt ::= name { `,` name } `in` explist_exp forbody_stmt
    pub(super) fn forlist_stmt(&mut self, _indexname: &str) -> Result<(), ParseErr> {
        log::debug!("parse forlist_stmt");

        // parse { `,` name }
        while match_token!(test_consume: self.p, Token::Operator(',')) {
            let _name = self.p.plex().name()?;
        }
        // parse `in`
        match_token!(consume: self.p, Token::In)?;
        // parse explist_exp
        let _ne = self.p.pexp().exprlist_exp()?;
        // parse forbody_stmt
        self.forbody_stmt(0, 0, true)?;

        Ok(())
    }

    // for_stmt ::= `for` (fornum_stmt | forlist_stmt) `end`
    pub(super) fn for_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse for_stmt");

        // parse `for`
        self.p.plex().skip();
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

        Ok(())
    }

    // do_stmt ::= `do` block_stmt `end`
    pub(super) fn do_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse do_stmt");

        // parse `do`
        self.p.plex().skip();
        // parse block_stmt
        self.block_stmt()?;
        // parse `end`
        match_token!(consume: self.p, Token::End)?;

        Ok(())
    }

    // repeat_stmt ::= `repeat` block_stmt `until` cond_exp
    pub(super) fn repeat_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse repeat_stmt");

        // parse `repeat`
        self.p.plex().skip();
        // parse block_stmt
        self.block_stmt()?;
        // parse `until`
        match_token!(consume: self.p, Token::Until)?;
        // parse cond_exp
        self.p.pexp().expr_exp()?;

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
                        self.p.plex().skip();
                        _nparams += 1;
                    }
                    // parse `...`
                    Token::Dots => {
                        self.p.plex().skip();
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
        self.p.plex().skip();
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
        self.p.plex().skip();
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
            // TODO mark to be close

            let vidx = self.p.pfscope().reglevel(level);
            self.p.emiter().emit_tbc(vidx);
        }

        Ok(())
    }

    // local_stmt ::= `local` [localfunc_stmt | localvar_stmt]
    pub(super) fn local_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse local_stmt");

        // parse `local`
        self.p.plex().skip();
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
        self.p.plex().skip();
        // parse name
        let _name = self.p.plex().name()?;
        // parse `::`
        self.p.plex().skip();

        Ok(())
    }

    // return_stmt ::= `return` [explist_exp] [`;`]
    pub(super) fn return_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse return_stmt");

        // parse `return`
        self.p.plex().skip();

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
        self.p.plex().skip();

        Ok(())
    }

    // goto_stmt ::= `goto` name
    pub(super) fn goto_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse goto_stmt");

        // parse `goto`
        self.p.plex().skip();
        // parse name
        let _name = self.p.plex().name()?;

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
            Token::If => self.if_stmt()?,
            Token::While => self.while_stmt()?,
            Token::Do => self.do_stmt()?,
            Token::For => self.for_stmt()?,
            Token::Repeat => self.repeat_stmt()?,
            Token::Function => self.func_stmt()?,
            Token::Local => self.local_stmt()?,
            Token::Label => self.label_stmt()?,
            Token::Return => self.return_stmt()?,
            Token::Break => self.break_stmt()?,
            Token::Goto => self.goto_stmt()?,
            _ => self.expr_stmt()?,
        }

        Ok(())
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
