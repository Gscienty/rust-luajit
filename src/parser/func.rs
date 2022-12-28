use crate::{
    check_token,
    code::InterCode,
    consume_token,
    lexer::Token,
    matches_token,
    object::{Expr, ExprValue, Prototype, Var, VarKind},
};

use super::{fscope::FuncState, ParseErr, Parser};

impl Parser {
    fn enterfunc(&mut self) {
        let fscope = FuncState::new(Prototype::new());

        self.fs
            .prop()
            .proto
            .prop_mut()
            .children_proto
            .push(fscope.prop().proto.clone());

        fscope.prop_mut().prev = Some(self.fs.clone());
        fscope.prop_mut().first_local = self.actvar.len();
        fscope.prop_mut().first_label = self.label.len();

        fscope.prop().block.prop_mut().nactvar = self.fs.prop().nactvar - 1;
        fscope.prop().block.prop_mut().first_label = self.label.len();
        fscope.prop().block.prop_mut().first_goto = self.goto.len();

        self.fs = fscope;
    }

    fn ret(&mut self, first: usize, nret: usize) {
        match nret {
            0 => self.emit_return0(),
            1 => self.emit_return1(first),
            _ => self.emit_return(first, nret + 1),
        };
    }

    fn leavefunc(&mut self) -> Result<(), ParseErr> {
        let nvars = self.nvarstack();
        self.ret(nvars, 0);

        self.leaveblock()?;

        let prevfs = self.fs.prop().prev.clone();
        if let Some(fs) = prevfs {
            self.fs = fs;
        }

        Ok(())
    }

    fn setreturns(&mut self, exp: &Expr, nresults: usize) -> Result<(), ParseErr> {
        match exp.value {
            ExprValue::Call(pc) => {
                self.set_rc(pc, nresults + 1);
                Ok(())
            }
            ExprValue::VarArg(pc) => {
                let freereg = self.fs.prop().freereg;

                self.set_rc(pc, nresults + 1);
                self.set_ra(pc, freereg);

                self.reserve(1);

                Ok(())
            }
            _ => Err(ParseErr::BadUsage),
        }
    }

    // funcname_stmt ::= name { field_stmt } [ `:` name ]
    fn funcname_stmt(&mut self) -> Result<(bool, Expr), ParseErr> {
        let name = self.name()?;
        let mut exp = self.singlevar(&name)?;
        while matches_token!(self, Token::Operator('.')) {
            exp = self.field_stmt(exp)?;
        }

        let ismethod = if matches_token!(self, Token::Operator(':')) {
            exp = self.field_stmt(exp)?;
            true
        } else {
            false
        };

        Ok((ismethod, exp))
    }

    // parlist_stmt ::= [ {name `,`} (name | `...`) ]
    fn parlist_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse parlist_stmt");

        let mut nparams = 0;
        let mut isvararg = false;
        if !matches_token!(self, Token::Operator(')')) {
            loop {
                match &self.lexer.token {
                    // parse name
                    Token::Name(name) => {
                        self.new_localvar(Var::new(name.clone(), VarKind::REG));
                        self.skip()?;
                        nparams += 1;
                    }
                    // parse `...`
                    Token::Dots => {
                        self.skip()?;
                        isvararg = true;
                    }
                    _ => return Err(ParseErr::BadUsage),
                }

                // parse `,`
                if isvararg || !check_token!(self, Token::Operator(',')) {
                    break;
                }
            }
        }

        self.adjlocalvars(nparams);
        let nactvar = self.fs.prop().nactvar;

        self.fs.prop().proto.prop_mut().nparams = nactvar;
        if isvararg {
            self.fs.prop().proto.prop_mut().vararg = true;
            self.emit_varargprep(nparams);
        }
        self.reserve(nactvar);

        Ok(())
    }

    // body_stmt ::= `(` parlist `)` statlist `end`
    pub(super) fn body_stmt(&mut self, ismethod: bool) -> Result<Expr, ParseErr> {
        self.enterfunc();

        consume_token!(self, Token::Operator('('))?;
        if ismethod {
            self.new_localvar(Var::new(String::from("self"), VarKind::REG));
            self.adjlocalvars(1);
        }
        self.parlist_stmt()?;
        consume_token!(self, Token::Operator(')'))?;

        self.stmtlist()?;

        consume_token!(self, Token::End)?;

        let pfscope = self.fs.prop().prev.clone();
        let exp = if let Some(pfscope) = pfscope {
            let closureid = pfscope.prop().proto.prop().children_proto.len() - 1;
            let pc = self.emit_closure(closureid)?;

            Expr::reloc(pc)
        } else {
            return Err(ParseErr::BadUsage);
        };
        self.leavefunc()?;

        let exp = self.expnextreg(exp)?;

        Ok(exp)
    }

    // func_stmt ::= `function` funcname_stmt body_stmt
    pub(super) fn func_stmt(&mut self) -> Result<(), ParseErr> {
        self.skip()?;

        let (ismethod, name) = self.funcname_stmt()?;
        let body = self.body_stmt(ismethod)?;
        self.storevar(&name, body)?;

        if let ExprValue::IndexStr(reg, _) = name.value {
            self.freereg(reg)?;
        }

        Ok(())
    }

    // return_stmt ::= `return` [explist_exp] [`;`]
    pub(super) fn return_stmt(&mut self) -> Result<(), ParseErr> {
        self.skip()?;

        let mut first = self.nvarstack();
        let nret = if self.block_follow(true) || matches_token!(self, Token::Operator(';')) {
            0
        } else {
            // parse exprlist_exp
            let (mut nret, exp) = self.explist_exp()?;

            if exp.hasmultret() {
                self.setreturns(&exp, 254)?;
                match exp.value {
                    ExprValue::Call(pc)
                        if nret == 1 && !self.fs.prop().block.prop().inside_tobeclosed =>
                    {
                        self.modify_code(pc, |c| {
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
                    let exp = self.expanyreg(exp)?;
                    first = self.nonreloc(&exp)?;
                } else {
                    self.expanyreg(exp)?;
                }
            }

            nret
        };
        self.ret(first, nret);

        // parse `;`
        check_token!(self, Token::Operator(';'));

        Ok(())
    }
}
