use crate::{
    code::{codelimit, InterCode},
    lexer::Token,
    match_token,
    object::{Expr, ExprValue, LabelDesc, TableCtor},
};

use super::{block::Block, BinOpr, ParseErr, Parser, UnOpr};

pub(super) struct ParseExpr<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseExpr<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
    }

    // index_exp = `[` exp `]`
    fn index_exp(&mut self) -> Result<Expr, ParseErr> {
        self.p.plex().skip()?;
        let exp = self.expr_exp()?;
        match_token!(consume: self.p, Token::Operator(']'))?;

        Ok(exp)
    }

    // recfield_exp ::= (name | `[` exp `]`) = exp
    pub(super) fn recfield_exp(&mut self, tc: &mut TableCtor) -> Result<(), ParseErr> {
        log::debug!("parse recfield_exp");

        let memreg = self.p.fs.prop().freereg;

        let kexp = match self.p.lex(|x| x.token.clone()) {
            Token::Name(name) => {
                // parse name
                self.p.plex().skip()?;

                Expr::from(name.as_str())
            }
            Token::Operator('[') => self.index_exp()?,
            _ => return Err(ParseErr::BadUsage),
        };
        tc.nh += 1;

        // parse `=`
        match_token!(consume: self.p, Token::Operator('='))?;

        let tkexp = self.p.pvar().indexed(tc.texp.clone(), kexp)?;
        // parse exp
        let vexp = self.expr_exp()?;
        self.p.pvar().store_var(&tkexp, vexp)?;

        self.p.fs.prop_mut().freereg = memreg;

        Ok(())
    }

    // listfield_exp ::= expr_exp
    pub(super) fn listfield_exp(&mut self, tc: &mut TableCtor) -> Result<(), ParseErr> {
        log::debug!("parse listfield_exp");

        tc.lexp = self.expr_exp()?;
        tc.tostore += 1;

        Ok(())
    }

    // field_exp ::= listfield_exp | recfield_exp
    pub(super) fn field_exp(&mut self, tc: &mut TableCtor) -> Result<(), ParseErr> {
        log::debug!("parse field_exp");

        match self.p.lex(|x| x.token.clone()) {
            Token::Name(_) => {
                let lookahead = self.p.lex_mut(|x| x.token_lookahead())?;
                if matches!(lookahead, Token::Operator('=')) {
                    self.recfield_exp(tc)
                } else {
                    self.listfield_exp(tc)
                }
            }
            Token::Operator('[') => self.recfield_exp(tc),
            _ => self.listfield_exp(tc),
        }
    }

    // constructor_exp ::= `{` [ field_exp { sep field_exp } [ sep ] ] `}`
    // sep ::= `,` | `;`
    pub(super) fn constructor_exp(&mut self) -> Result<Expr, ParseErr> {
        log::debug!("parse constructor_exp");

        let mut tc = TableCtor::new();
        let pc = self.p.emiter().emit_newtable();
        self.p.emiter().emit_nop();

        let treg = self.p.fs.prop().freereg;
        tc.texp = Expr::nonreloc(treg);
        self.p.pvar().reserver_regs(1);

        // parse `{`
        match_token!(consume: self.p, Token::Operator('{'))?;
        loop {
            if match_token!(test: self.p, Token::Operator('}')) {
                break;
            }

            // close listfield
            if !matches!(tc.lexp.value, ExprValue::Void) {
                self.p.pvar().exp_tonextreg(tc.lexp.clone())?;
                tc.lexp = Expr::void();

                // flush, setlist
                self.p.emiter().emit_setlist(treg, tc.na, tc.tostore);
                tc.na += tc.tostore;
                tc.tostore = 0;
            }
            // parse field_exp
            self.field_exp(&mut tc)?;

            // parse sep
            if !match_token!(test_consume: self.p, Token::Operator(',' | ';')) {
                break;
            }
        }
        // parse `}`
        match_token!(consume: self.p, Token::Operator('}'))?;

        // last listfield
        if tc.tostore != 0 {
            if tc.lexp.hasmultret() {
                self.setreturns(&tc.lexp, 254)?;
                self.p.emiter().emit_setlist(treg, tc.na, 0);
                tc.na -= 1;
            } else {
                if !matches!(tc.lexp.value, ExprValue::Void) {
                    self.p.pvar().exp_tonextreg(tc.lexp.clone())?;
                }
                self.p.emiter().emit_setlist(treg, tc.na, tc.tostore);
            }

            tc.na += tc.tostore;
        }

        self.settablesize(pc, treg, tc.na, tc.nh);

        Ok(tc.texp)
    }

    // simple_exp -> FLT | INT | STRING | NIL | TRUE | FALSE | ... | constructor_exp | FUNCTION body
    //              | suffixed_exp
    pub(super) fn simple_exp(&mut self) -> Result<Expr, ParseErr> {
        log::debug!("parse simple_exp");

        let expr = match self.p.plex().current_token() {
            Token::Integer(val) => Expr::from(val),
            Token::Number(val) => Expr::from(val),
            Token::String(val) => Expr::from(val.as_str()),
            Token::Nil => Expr::nil(),
            Token::True => Expr::from(true),
            Token::False => Expr::from(false),
            Token::Dots => {
                if !self.p.fs.prop().proto.prop().vararg {
                    return Err(ParseErr::BadUsage);
                }
                Expr::vararg(self.p.emiter().emit_vararg())
            }
            Token::Operator('{') => {
                return self.constructor_exp();
            }
            Token::Function => {
                // parse `function`
                self.p.plex().skip()?;
                // parse body_stmt
                return self.p.pstmt().body_stmt(false);
            }
            _ => return self.suffixed_exp(),
        };
        self.p.plex().skip()?;

        Ok(expr)
    }

    // sub_exp -> (simple_exp | unop sub_exp) { binop sub_exp }
    pub(super) fn sub_exp(&mut self, limit: u8) -> Result<(BinOpr, Expr), ParseErr> {
        log::debug!("parse sub_exp");

        let unop = UnOpr::from(self.p.plex().current_token());

        let mut exp = if matches!(unop, UnOpr::NoOpr) {
            // parse simple_exp
            self.simple_exp()?
        } else {
            log::debug!("parse unop");

            // parse unop
            self.p.plex().skip()?;
            // parse sub_exp
            let (_, exp) = self.sub_exp(BinOpr::UNARY_PRI)?;
            self.prefix(unop, exp)?
        };
        log::debug!("sub_exp prefix exp {}", exp.value);

        let mut binop = BinOpr::from(self.p.plex().current_token());
        while !matches!(binop, BinOpr::NoOpr) && binop.lpri() > limit {
            // parse binop
            self.p.plex().skip()?;

            exp = self.infix(binop, exp)?;
            let (nbop, nexp) = self.sub_exp(binop.rpri())?;
            exp = self.posfix(binop, exp, nexp)?;

            binop = nbop;
        }

        Ok((binop, exp))
    }

    pub(super) fn expr_exp(&mut self) -> Result<Expr, ParseErr> {
        log::debug!("parse expr_exp");

        let (_, exp) = self.sub_exp(BinOpr::INIT_PRI)?;
        Ok(exp)
    }

    pub(super) fn exprtoreg_exp(&mut self) -> Result<(), ParseErr> {
        let exp = self.expr_exp()?;
        self.p.pvar().exp_tonextreg(exp)?;

        Ok(())
    }

    pub(super) fn cond_exp(&mut self) -> Result<usize, ParseErr> {
        let mut exp = self.expr_exp()?;

        if matches!(exp.value, ExprValue::Nil) {
            exp.value = ExprValue::Bool(false);
        }

        let exp = self.goiftrue(exp)?;

        match exp.false_jumpto {
            Some(pc) => Ok(pc),
            _ => Err(ParseErr::BadUsage),
        }
    }

    // exprlist_exp -> expr { `,` expr }
    pub(super) fn exprlist_exp(&mut self) -> Result<(usize, Expr), ParseErr> {
        log::debug!("parse exprlist_exp");

        let mut n = 1;
        let mut exp = self.expr_exp()?;

        while match_token!(test_consume: self.p, Token::Operator(',')) {
            self.p.pvar().exp_tonextreg(exp)?;

            exp = self.expr_exp()?;
            n += 1;
        }

        Ok((n, exp))
    }

    // primary_exp ::= name | `(` exp `)`
    pub(super) fn primary_exp(&mut self) -> Result<Expr, ParseErr> {
        log::debug!("parse primary_exp");

        match self.p.lex(|x| x.token.clone()) {
            Token::Operator('(') => {
                // parse `(`
                self.p.plex().skip()?;
                // parse exp
                let exp = self.expr_exp()?;
                // parse `)`
                match_token!(consume: self.p, Token::Operator(')'))?;

                log::debug!("parse priamry_exp exp: {}", exp.value);

                self.p.pvar().discharge_tovar(exp)
            }
            Token::Name(name) => {
                log::debug!("parse priamry_exp name: {}", name);
                // parse name
                self.p.plex().skip()?;

                self.p.pvar().single_var(name.as_str())
            }
            _ => Err(ParseErr::BadUsage),
        }
    }

    // funcargs_exp ::= ( `(` [ exprlist_exp ] `)` | constructor_exp | string )
    pub(super) fn funcargs_exp(&mut self, fexp: Expr) -> Result<Expr, ParseErr> {
        log::debug!("parse funcargs_exp");

        let argexp = match self.p.lex(|x| x.token.clone()) {
            Token::Operator('(') => {
                // parse `(`
                self.p.plex().skip()?;

                let exp = if match_token!(test: self.p, Token::Operator(')')) {
                    Expr::void()
                } else {
                    // parse exprlist_exp
                    let (_, exp) = self.exprlist_exp()?;
                    if exp.hasmultret() {
                        self.setreturns(&exp, 254)?;
                    }

                    exp
                };
                // parse `)`
                match_token!(consume: self.p, Token::Operator(')'))?;

                exp
            }
            Token::Operator('{') => self.constructor_exp()?,
            Token::String(val) => {
                // parse string
                self.p.plex().skip()?;

                Expr::from(val.as_str())
            }
            _ => return Err(ParseErr::BadUsage),
        };

        let base = self.p.pvar().locreg(&fexp)?;
        let nparams = if argexp.hasmultret() {
            255
        } else {
            if !matches!(argexp.value, ExprValue::Void) {
                self.p.pvar().exp_tonextreg(argexp)?;
            }
            self.p.fs.prop().freereg - (base + 1)
        };

        let pc = self.p.emiter().emit_call(base, nparams + 1, 2);

        self.p.fs.prop_mut().freereg = base + 1;

        Ok(Expr::call(pc))
    }

    // suffixed_exp ::= primary_exp { `.` name | `[` exp `]` | `:` name funcargs_exp | funcargs_exp }
    pub(super) fn suffixed_exp(&mut self) -> Result<Expr, ParseErr> {
        log::debug!("parse suffixed_exp");

        // parse primary_exp
        let mut exp = self.primary_exp()?;

        log::debug!("parse suffixed_exp primary_exp = {}", exp.value);

        loop {
            exp = match self.p.lex(|x| x.token.clone()) {
                Token::Operator('.') => self.p.pstmt().fieldsel_stmt(exp)?,
                Token::Operator('[') => {
                    let texp = self.p.pvar().exp_toanyregup(exp)?;
                    let kexp = self.index_exp()?;

                    self.p.pvar().indexed(texp, kexp)?
                }
                Token::Operator(':') => {
                    // parse `:`
                    self.p.plex().skip()?;
                    // parse name
                    let name = self.p.plex().name()?;
                    let kexp = Expr::from(name.as_str());

                    let exp = self.self_exp(exp, kexp)?;

                    // parse funcargs_exp
                    self.funcargs_exp(exp)?
                }
                Token::Operator('(') | Token::String(_) | Token::Operator('{') => {
                    log::debug!("parse funcargs_exp");
                    // parse funcargs_exp
                    let fexp = self.p.pvar().exp_tonextreg(exp)?;
                    self.funcargs_exp(fexp)?
                }
                _ => break Ok(exp),
            }
        }
    }

    fn arith_imm(&mut self, op: BinOpr, reg: usize, imm: u8) -> Result<usize, ParseErr> {
        Ok(match op {
            BinOpr::ADD => self.p.emiter().emit_addi(reg, imm),
            BinOpr::SHL => self.p.emiter().emit_shli(reg, imm),
            BinOpr::SHR => self.p.emiter().emit_shri(reg, imm),
            _ => return Err(ParseErr::BadUsage),
        })
    }

    fn arith_allreg(&mut self, op: BinOpr, r1: usize, r2: usize) -> Result<usize, ParseErr> {
        log::debug!("parse arith_allreg op: {}", op);

        Ok(match op {
            BinOpr::ADD => self.p.emiter().emit_add(r1, r2),
            BinOpr::SUB => self.p.emiter().emit_sub(r1, r2),
            BinOpr::MUL => self.p.emiter().emit_mul(r1, r2),
            BinOpr::DIV => self.p.emiter().emit_div(r1, r2),
            BinOpr::IDIV => self.p.emiter().emit_idiv(r1, r2),
            BinOpr::MOD => self.p.emiter().emit_mod(r1, r2),
            BinOpr::POW => self.p.emiter().emit_pow(r1, r2),
            BinOpr::SHL => self.p.emiter().emit_shl(r1, r2),
            BinOpr::SHR => self.p.emiter().emit_shr(r1, r2),
            BinOpr::BAND => self.p.emiter().emit_band(r1, r2),
            BinOpr::BOR => self.p.emiter().emit_bor(r1, r2),
            BinOpr::BXOR => self.p.emiter().emit_bxor(r1, r2),
            _ => return Err(ParseErr::BadUsage),
        })
    }

    fn arith_k(&mut self, op: BinOpr, reg: usize, k: usize) -> Result<usize, ParseErr> {
        log::debug!("parse arith_k op: {}", op);

        Ok(match op {
            BinOpr::ADD => self.p.emiter().emit_addk(reg, k),
            BinOpr::SUB => self.p.emiter().emit_subk(reg, k),
            BinOpr::MUL => self.p.emiter().emit_mulk(reg, k),
            BinOpr::DIV => self.p.emiter().emit_divk(reg, k),
            BinOpr::IDIV => self.p.emiter().emit_idivk(reg, k),
            BinOpr::MOD => self.p.emiter().emit_modk(reg, k),
            BinOpr::POW => self.p.emiter().emit_powk(reg, k),
            BinOpr::BAND => self.p.emiter().emit_bandk(reg, k),
            BinOpr::BOR => self.p.emiter().emit_bork(reg, k),
            BinOpr::BXOR => self.p.emiter().emit_bxork(reg, k),
            _ => return Err(ParseErr::BadUsage),
        })
    }

    fn arith_iimdir(&self, op: BinOpr, v1: i64, v2: i64) -> Result<Expr, ParseErr> {
        log::debug!("parse arith_iimdir op: {}", op);

        Ok(Expr::from(match op {
            BinOpr::ADD => v1 + v2,
            BinOpr::SUB => v1 - v2,
            BinOpr::MUL => v1 * v2,
            BinOpr::DIV => return Ok(Expr::from(v1 as f64 / v2 as f64)),
            BinOpr::IDIV => v1 / v2,
            BinOpr::MOD => v1 % v2,
            BinOpr::POW => v1.pow(v2 as u32),
            BinOpr::SHL => v1 << v2,
            BinOpr::SHR => v1 >> v2,
            BinOpr::BAND => v1 & v2,
            BinOpr::BOR => v1 | v2,
            BinOpr::BXOR => v1 ^ v2,
            _ => return Err(ParseErr::BadUsage),
        }))
    }

    fn arith_fimdir(&self, op: BinOpr, v1: f64, v2: f64) -> Result<Expr, ParseErr> {
        log::debug!("parse arith_fimdir op: {}", op);

        Ok(Expr::from(match op {
            BinOpr::ADD => v1 + v2,
            BinOpr::SUB => v1 - v2,
            BinOpr::MUL => v1 * v2,
            BinOpr::DIV => v1 / v2,
            BinOpr::IDIV => return Ok(Expr::from((v1 / v2) as i64)),
            BinOpr::MOD => v1 % v2,
            BinOpr::POW => v1.powf(v2),
            _ => return Err(ParseErr::BadUsage),
        }))
    }

    fn arith_inreg(&mut self, op: BinOpr, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        log::debug!("parse arith_inreg op: {}", op);

        let allowimm = matches!(op, BinOpr::ADD | BinOpr::SHL | BinOpr::SHR);
        let allowk = matches!(
            op,
            BinOpr::ADD
                | BinOpr::MUL
                | BinOpr::DIV
                | BinOpr::IDIV
                | BinOpr::SUB
                | BinOpr::MOD
                | BinOpr::POW
                | BinOpr::BAND
                | BinOpr::BOR
                | BinOpr::BXOR
        );

        match e2.value {
            ExprValue::Integer(value)
                if allowimm && 0 <= value && value as u32 <= codelimit::MAX_C =>
            {
                let e1 = self.p.pvar().exp_toanyreg(e1)?;
                let reg = self.p.pvar().locreg(&e1)?;
                let pc = self.arith_imm(op, reg, value as u8)?;

                self.p.pvar().exp_free(&e1)?;

                Ok(Expr::reloc(pc))
            }
            ExprValue::Integer(_) | ExprValue::Float(_) | ExprValue::String(_) => {
                let e2 = self.p.pvar().exp_tok(e2)?;
                self.arith_inreg(op, e1, e2)
            }
            ExprValue::K(k) if allowk => {
                let e1 = self.p.pvar().exp_toanyreg(e1)?;
                let reg = self.p.pvar().locreg(&e1)?;
                let pc = self.arith_k(op, reg, k)?;

                self.p.pvar().exp_free(&e1)?;

                Ok(Expr::reloc(pc))
            }
            _ => Err(ParseErr::BadUsage),
        }
    }

    fn arith(&mut self, op: BinOpr, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        log::debug!("parse arith op: {}", op);

        let swapable = matches!(
            op,
            BinOpr::ADD | BinOpr::MUL | BinOpr::BAND | BinOpr::BOR | BinOpr::BXOR
        );
        if !e1.inreg() && !e2.inreg() {
            if e1.numeric() && e2.numeric() {
                log::debug!("parse arith op: {}, all not inreg, all immediate", op);

                match e1.value {
                    ExprValue::Integer(v1) => match e2.value {
                        ExprValue::Integer(v2) => self.arith_iimdir(op, v1, v2),
                        ExprValue::Float(v2) => self.arith_fimdir(op, v1 as f64, v2),
                        _ => Err(ParseErr::BadUsage),
                    },
                    ExprValue::Float(v1) => match e2.value {
                        ExprValue::Integer(v2) => self.arith_fimdir(op, v1, v2 as f64),
                        ExprValue::Float(v2) => self.arith_fimdir(op, v1, v2),
                        _ => Err(ParseErr::BadUsage),
                    },
                    _ => Err(ParseErr::BadUsage),
                }
            } else if !e1.numeric() && e2.numeric() {
                log::debug!("parse arith op: {}, all not inreg, e1 not immediate", op);

                let e1 = self.p.pvar().exp_toanyreg(e1)?;
                self.arith(op, e1, e2)
            } else if e1.numeric() && !e2.numeric() && swapable {
                log::debug!("parse arith op: {}, all not inreg, e2 not immediate", op);

                let e2 = self.p.pvar().exp_toanyreg(e2)?;
                self.arith(op, e1, e2)
            } else {
                log::debug!("parse arith op: {}, all not inreg, all not immediate", op);

                let e1 = self.p.pvar().exp_toanyreg(e1)?;

                self.arith(op, e1, e2)
            }
        } else if e1.inreg() && !e2.inreg() {
            log::debug!("parse arith op: {}, e2 not inreg", op);

            let e1 = self.p.pvar().exp_toanyreg(e1)?;

            self.arith_inreg(op, e1, e2)
        } else if !e1.inreg() && e2.inreg() && swapable {
            log::debug!("parse arith op: {}, e1 not inreg", op);

            let e2 = self.p.pvar().exp_toanyreg(e2)?;

            self.arith_inreg(op, e2, e1)
        } else {
            log::debug!(
                "parse arith op: {}, e1: {}; e2: {}, all inreg",
                op,
                e1.value,
                e2.value
            );

            let e1 = self.p.pvar().exp_toanyreg(e1)?;
            let e2 = self.p.pvar().exp_toanyreg(e2)?;

            let r1 = self.p.pvar().locreg(&e1)?;
            let r2 = self.p.pvar().locreg(&e2)?;

            let pc = self.arith_allreg(op, r1, r2)?;

            self.p.pvar().exps_free(&e1, &e2)?;

            Ok(Expr::reloc(pc))
        }
    }

    pub(super) fn posfix(&mut self, op: BinOpr, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        log::debug!("parse posfix, op: {}", op);
        let e2 = self.p.pvar().discharge_tovar(e2)?;

        match op {
            BinOpr::AND => {
                let mut e2 = e2;
                self.jump_concatlist(&mut e2.false_jumpto, e1.false_jumpto)?;

                Ok(e2)
            }
            BinOpr::OR => {
                let mut e2 = e2;
                self.jump_concatlist(&mut e2.true_jumpto, e1.true_jumpto)?;

                Ok(e2)
            }
            BinOpr::EQ | BinOpr::NE => {
                if e1.inreg() {
                    self.cmp_eq(op, e1.clone(), e2.clone())
                } else {
                    self.cmp_eq(op, e2.clone(), e1.clone())
                }
            }
            BinOpr::LT | BinOpr::LE | BinOpr::GT | BinOpr::GE => self.cmp_order(op, e1, e2),
            BinOpr::CONCAT => {
                log::debug!("parse posfix concat");
                let e2 = self.p.pvar().exp_tonextreg(e2)?;

                let pc = self.p.emiter().pc() - 1;
                match self.p.emiter().get_code(pc) {
                    Some(InterCode::CONCAT(pra, prb)) => match e1.value {
                        ExprValue::Nonreloc(reg) if reg + 1 == pra as usize => {
                            self.p.emiter().set_ra(pc, reg);
                            self.p.emiter().set_rb(pc, prb + 1);

                            self.p.pvar().exp_free(&e2)?;

                            Ok(e1)
                        }
                        _ => Err(ParseErr::BadUsage),
                    },
                    _ => match e1.value {
                        ExprValue::Nonreloc(reg) => {
                            self.p.emiter().emit_concat(reg);

                            self.p.pvar().exp_free(&e2)?;

                            Ok(e1)
                        }
                        _ => Err(ParseErr::BadUsage),
                    },
                }
            }
            BinOpr::ADD
            | BinOpr::SUB
            | BinOpr::MUL
            | BinOpr::DIV
            | BinOpr::IDIV
            | BinOpr::MOD
            | BinOpr::POW
            | BinOpr::SHL
            | BinOpr::SHR
            | BinOpr::BAND
            | BinOpr::BOR
            | BinOpr::BXOR => self.arith(op, e1, e2),

            _ => Err(ParseErr::BadUsage),
        }
    }

    fn unarith(&mut self, op: UnOpr, exp: Expr) -> Result<Expr, ParseErr> {
        let exp = self.p.pvar().exp_toanyreg(exp)?;
        let r = self.p.pvar().locreg(&exp)?;
        self.p.pvar().exp_free(&exp)?;

        match op {
            UnOpr::MINUS => Ok(Expr::reloc(self.p.emiter().emit_unm(r))),
            UnOpr::BNOT => Ok(Expr::reloc(self.p.emiter().emit_bnot(r))),
            UnOpr::LEN => Ok(Expr::reloc(self.p.emiter().emit_len(r))),
            _ => Err(ParseErr::BadUsage),
        }
    }

    fn removevalues(&mut self, list: Option<usize>) {
        let mut list = list;
        while let Some(pc) = list {
            self.patch_testreg(pc, None);

            list = self.p.emiter().get_jump(pc);
        }
    }

    pub(super) fn infix(&mut self, op: BinOpr, exp: Expr) -> Result<Expr, ParseErr> {
        log::debug!("parse infix, op: {}, exp: {}", op, exp.value);

        let exp = self.p.pvar().discharge_tovar(exp)?;

        match op {
            BinOpr::AND => self.p.pexp().goiftrue(exp),
            BinOpr::OR => self.p.pexp().goiffalse(exp),
            BinOpr::ADD
            | BinOpr::SUB
            | BinOpr::MUL
            | BinOpr::DIV
            | BinOpr::IDIV
            | BinOpr::MOD
            | BinOpr::POW
            | BinOpr::BAND
            | BinOpr::BOR
            | BinOpr::BXOR
            | BinOpr::SHL
            | BinOpr::SHR => match &exp.value {
                ExprValue::Float(_) | ExprValue::Integer(_) => Ok(exp), // keep, use immediate
                _ => self.p.pvar().exp_toanyreg(exp),
            },
            BinOpr::CONCAT => self.p.pvar().exp_tonextreg(exp),
            BinOpr::EQ | BinOpr::NE => self.p.pvar().exp_tokreg(exp),
            BinOpr::LT | BinOpr::LE | BinOpr::GT | BinOpr::GE => self.p.pvar().exp_toanyreg(exp),
            _ => Err(ParseErr::BadUsage),
        }
    }

    pub(super) fn prefix(&mut self, op: UnOpr, exp: Expr) -> Result<Expr, ParseErr> {
        log::debug!("parse prefix, op: {}", op);

        let exp = self.p.pvar().discharge_tovar(exp)?;

        match op {
            UnOpr::MINUS => match exp.value {
                ExprValue::Float(v) => Ok(Expr::from(-v)),
                ExprValue::Integer(v) => Ok(Expr::from(-v)),
                _ => self.unarith(op, exp),
            },
            UnOpr::BNOT => match exp.value {
                ExprValue::Integer(v) => Ok(Expr::from(!v)),
                _ => self.unarith(op, exp),
            },
            UnOpr::LEN => self.unarith(op, exp),
            UnOpr::NOT => {
                let tj = exp.true_jumpto;
                let fj = exp.false_jumpto;

                let exp = match exp.value {
                    ExprValue::Nil | ExprValue::Bool(false) => Ok(Expr::from(true)),

                    ExprValue::K(_)
                    | ExprValue::Float(_)
                    | ExprValue::Integer(_)
                    | ExprValue::String(_)
                    | ExprValue::Bool(true) => Ok(Expr::from(false)),

                    ExprValue::Jump(pc) => {
                        self.p.emiter().negate_cond(pc);

                        Ok(exp)
                    }

                    ExprValue::Reloc(_) | ExprValue::Nonreloc(_) => {
                        let exp = self.p.pvar().discharge_toanyreg(exp)?;
                        let r = self.p.pvar().locreg(&exp)?;
                        self.p.pvar().exp_free(&exp)?;

                        Ok(Expr::reloc(self.p.emiter().emit_not(r)))
                    }
                    _ => Err(ParseErr::BadUsage),
                }?
                .tj(fj)
                .fj(tj);

                self.removevalues(exp.false_jumpto);
                self.removevalues(exp.true_jumpto);

                Ok(exp)
            }
            _ => Err(ParseErr::BadUsage),
        }
    }

    pub(super) fn jump_patchlist(
        &mut self,
        list: Option<usize>,
        pc: Option<usize>,
    ) -> Result<(), ParseErr> {
        self.jump_patchlistaux(list, pc, None, pc)
    }

    pub(super) fn jump_patchtohere(&mut self, list: Option<usize>) -> Result<(), ParseErr> {
        let here = self.p.emiter().mark_pc();
        self.jump_patchlist(list, Some(here))
    }

    pub(super) fn patch_forjump(&mut self, pc: usize, dest: usize, back: bool) {
        let offset = dest as i32 - (pc + 1) as i32;

        self.p
            .emiter()
            .set_bx(pc, if back { -offset } else { offset });
    }

    pub(super) fn patch_testreg(&mut self, node: usize, reg: Option<usize>) -> bool {
        let (pc, ins) = self.p.emiter().get_ctrljump(node);

        match ins {
            Some(InterCode::TESTSET(_, rb, k)) => {
                match reg {
                    Some(reg) if reg != rb as usize => self.p.emiter().set_ra(pc, reg),

                    _ => self
                        .p
                        .emiter()
                        .modify_code(pc, |c| *c = InterCode::TEST(rb, k)),
                }
                true
            }
            _ => false,
        }
    }

    pub(super) fn jump_patchlistaux(
        &mut self,
        list: Option<usize>,
        vtgt: Option<usize>,
        reg: Option<usize>,
        dtgt: Option<usize>,
    ) -> Result<(), ParseErr> {
        let mut list = list;

        while let Some(pc) = list {
            let next = self.p.emiter().get_jump(pc);

            if self.patch_testreg(pc, reg) {
                if let Some(vtgt) = vtgt {
                    self.jump_patch(pc, vtgt)?;
                }
            } else {
                if let Some(dtgt) = dtgt {
                    self.jump_patch(pc, dtgt)?;
                }
            }

            list = next;
        }
        Ok(())
    }

    fn jump_patch(&mut self, pc: usize, dpc: usize) -> Result<(), ParseErr> {
        let offset = dpc as i64 - (pc + 1) as i64;
        self.p.emiter().set_sj(pc, offset);

        Ok(())
    }

    pub(super) fn jump_concatlist(
        &mut self,
        l1: &mut Option<usize>,
        l2: Option<usize>,
    ) -> Result<(), ParseErr> {
        log::debug!("parse concat_jumplist");

        if l2.is_none() {
            Ok(())
        } else if l1.is_none() {
            *l1 = l2;
            Ok(())
        } else {
            let mut list = l1.unwrap();
            loop {
                if let Some(next) = self.p.emiter().get_jump(list) {
                    list = next
                } else {
                    break;
                }
            }
            self.jump_patch(list, l2.unwrap())?;

            Ok(())
        }
    }

    fn jump_oncond(&mut self, exp: Expr, cond: bool) -> Result<(usize, Expr), ParseErr> {
        match exp.value {
            ExprValue::Reloc(pc) => match self.p.emiter().get_code(pc) {
                Some(InterCode::NOT(_, rb)) => {
                    self.p.emiter().pop();

                    self.p.emiter().emit_testset(rb as usize, !cond);
                    return Ok((self.p.emiter().emit_jmp(), exp));
                }
                _ => {}
            },
            _ => {}
        };

        let exp = self.p.pvar().discharge_toanyreg(exp)?;
        self.p.pvar().exp_free(&exp)?;
        let reg = self.p.pvar().locreg(&exp)?;

        self.p.emiter().emit_testset(reg, cond);
        Ok((self.p.emiter().emit_jmp(), exp))
    }

    pub(super) fn goiftrue(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        let mut exp = self.p.pvar().discharge_tovar(exp)?;

        match exp.value {
            ExprValue::Jump(pc) => {
                self.p.emiter().negate_cond(pc);

                self.jump_concatlist(&mut exp.false_jumpto, Some(pc))?;
                self.jump_patchtohere(exp.true_jumpto)?;
                Ok(exp.tj(None))
            }
            ExprValue::K(_)
            | ExprValue::Float(_)
            | ExprValue::Integer(_)
            | ExprValue::String(_)
            | ExprValue::Bool(true) => {
                self.jump_concatlist(&mut exp.false_jumpto, None)?;
                self.jump_patchtohere(exp.true_jumpto)?;
                Ok(exp.tj(None))
            }
            _ => {
                let (pc, mut exp) = self.jump_oncond(exp, false)?;
                self.jump_concatlist(&mut exp.false_jumpto, Some(pc))?;
                self.jump_patchtohere(exp.true_jumpto)?;
                Ok(exp.tj(None))
            }
        }
    }

    pub(super) fn goiffalse(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        let mut exp = self.p.pvar().discharge_tovar(exp)?;

        match exp.value {
            ExprValue::Jump(pc) => {
                self.jump_concatlist(&mut exp.true_jumpto, Some(pc))?;
                self.jump_patchtohere(exp.false_jumpto)?;
                Ok(exp.fj(None))
            }
            ExprValue::Nil | ExprValue::Bool(false) => {
                self.jump_concatlist(&mut exp.true_jumpto, None)?;
                self.jump_patchtohere(exp.false_jumpto)?;
                Ok(exp.fj(None))
            }
            _ => {
                let (pc, mut exp) = self.jump_oncond(exp, true)?;
                self.jump_concatlist(&mut exp.true_jumpto, Some(pc))?;
                self.jump_patchtohere(exp.false_jumpto)?;
                Ok(exp.fj(None))
            }
        }
    }

    fn cmp_eq(&mut self, op: BinOpr, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        log::debug!("parse cmp eq: {}", op);

        let e1 = self.p.pvar().exp_toanyreg(e1)?;
        let ra = self.p.pvar().locreg(&e1)?;
        let k = matches!(op, BinOpr::EQ);

        match e2.value {
            ExprValue::Integer(value) if 0 <= value && value as u32 <= codelimit::MAX_SBX => {
                self.p.emiter().emit_eqi(ra, value as u32, k);

                self.p.pvar().exp_free(&e1)?;
            }
            _ => {
                let e2 = self.p.pvar().exp_tokreg(e2)?;
                match e2.value {
                    ExprValue::Nonreloc(rb) => {
                        self.p.emiter().emit_eq(ra, rb, k);

                        self.p.pvar().exps_free(&e1, &e2)?;
                    }
                    ExprValue::K(rb) => {
                        self.p.emiter().emit_eqk(ra, rb, k);

                        self.p.pvar().exp_free(&e1)?;
                    }
                    _ => return Err(ParseErr::BadUsage),
                }
            }
        };

        Ok(Expr::jmp(self.p.emiter().emit_jmp()))
    }

    fn cmp_order(&mut self, op: BinOpr, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        let (op, e1, e2) = match op {
            BinOpr::LT | BinOpr::LE => (op, e1, e2),
            BinOpr::GE => (BinOpr::LE, e2, e1),
            BinOpr::GT => (BinOpr::LT, e2, e1),
            _ => return Err(ParseErr::BadUsage),
        };

        if matches!(e2.value, ExprValue::Integer(v) if 0 <= v && v as u32 <= codelimit::MAX_SBX) {
            let e1 = self.p.pvar().exp_toanyreg(e1)?;
            let r1 = self.p.pvar().locreg(&e1)?;
            let imm = match e2.value {
                ExprValue::Integer(v) => v,
                _ => unreachable!(),
            };

            match op {
                BinOpr::LT => self.p.emiter().emit_lti(r1, imm as u32, true),
                BinOpr::LE => self.p.emiter().emit_lei(r1, imm as u32, true),
                _ => unreachable!(),
            };
            self.p.pvar().exp_free(&e1)?;
        } else if matches!(e1.value, ExprValue::Integer(v) if 0 <= v && v as u32 <= codelimit::MAX_SBX)
        {
            let e2 = self.p.pvar().exp_toanyreg(e2)?;
            let r2 = self.p.pvar().locreg(&e2)?;
            let imm = match e1.value {
                ExprValue::Integer(v) => v,
                _ => unreachable!(),
            };

            log::debug!("{}", op);
            match op {
                BinOpr::LT => self.p.emiter().emit_gti(r2, imm as u32, true),
                BinOpr::LE => self.p.emiter().emit_gei(r2, imm as u32, true),
                _ => unreachable!(),
            };
            self.p.pvar().exp_free(&e2)?;
        } else {
            let e1 = self.p.pvar().exp_toanyreg(e1)?;
            let e2 = self.p.pvar().exp_toanyreg(e2)?;
            let r1 = self.p.pvar().locreg(&e1)?;
            let r2 = self.p.pvar().locreg(&e2)?;

            match op {
                BinOpr::LT => self.p.emiter().emit_lt(r1, r2, true),
                BinOpr::LE => self.p.emiter().emit_le(r1, r2, true),
                _ => unreachable!(),
            };

            self.p.pvar().exps_free(&e1, &e2)?;
        }

        Ok(Expr::jmp(self.p.emiter().emit_jmp()))
    }

    pub(super) fn setreturns(&mut self, exp: &Expr, nresults: usize) -> Result<(), ParseErr> {
        match exp.value {
            ExprValue::Call(pc) => {
                log::debug!("set returns call, pc: {}", pc);

                self.p.emiter().set_rc(pc, nresults as u8 + 1);
                Ok(())
            }
            ExprValue::VarArg(pc) => {
                let freereg = self.p.fs.prop().freereg;

                self.p.emiter().set_rc(pc, nresults as u8 + 1);
                self.p.emiter().set_ra(pc, freereg);

                self.p.pvar().reserver_regs(1);

                Ok(())
            }
            _ => Err(ParseErr::BadUsage),
        }
    }

    pub(super) fn ret(&mut self, first: usize, nret: usize) {
        match nret {
            0 => self.p.emiter().emit_return0(),
            1 => self.p.emiter().emit_return1(first),
            _ => self.p.emiter().emit_return(first, nret + 1),
        };
    }

    pub(super) fn findlabel(&self, name: &str) -> Option<LabelDesc> {
        let firstlabel = self.p.fs.prop().first_label;
        let len = self.p.label.len();

        for i in firstlabel..len {
            match self.p.label.get(i) {
                Some(l) if l.name.eq(name) => return Some(l.clone()),
                _ => {}
            }
        }
        None
    }

    pub(super) fn repeated(&self, name: &str) -> Result<(), ParseErr> {
        if matches!(self.findlabel(name), Some(..)) {
            Err(ParseErr::BadUsage)
        } else {
            Ok(())
        }
    }

    pub(super) fn slovegoto(&mut self, goto_pc: usize, label_pc: usize) -> Result<(), ParseErr> {
        self.p
            .pexp()
            .jump_patchlistaux(Some(goto_pc), Some(label_pc), None, Some(label_pc))
    }

    pub(super) fn slovegotos(&mut self, label: &LabelDesc) -> Result<bool, ParseErr> {
        let mut i = self.p.fs.prop().block.prop().firstgoto;

        let mut needsclose = false;
        while i < self.p.goto.len() {
            match self.p.goto.get(i) {
                Some(goto) if goto.name.eq(&label.name) => {
                    needsclose |= goto.close;
                    self.slovegoto(goto.pc, label.pc)?;
                    self.p.goto.remove(i);
                }
                _ => {
                    i += 1;
                }
            }
        }

        Ok(needsclose)
    }

    pub(super) fn createlabel(&mut self, name: &str, islast: bool) -> Result<bool, ParseErr> {
        let labelpc = self.p.emiter().mark_pc();
        let nactvar = if islast {
            self.p.fs.prop().block.prop().nactvar
        } else {
            self.p.fs.prop().nactvar
        };

        let label = LabelDesc::new(name, labelpc, nactvar);
        let result = if self.slovegotos(&label)? {
            let nactvar = self.p.pvar().instack_nvars();
            self.p.emiter().emit_close(nactvar);

            true
        } else {
            false
        };
        self.p.label.push(label);

        Ok(result)
    }

    pub(super) fn creategoto(&mut self, name: &str, pc: usize) -> Result<(), ParseErr> {
        let nactvar = self.p.fs.prop().nactvar;
        let goto = LabelDesc::new(name, pc, nactvar);

        self.p.goto.push(goto);

        Ok(())
    }

    pub(super) fn movegotosout(&mut self, block: &Block) -> Result<(), ParseErr> {
        let block_level = self.p.pvar().reglevel(block.prop().nactvar);

        for i in block.prop().firstgoto..self.p.goto.len() {
            let goto = if let Some(goto) = self.p.goto.get(i) {
                goto.clone()
            } else {
                continue;
            };
            let goto_level = self.p.pvar().reglevel(goto.nactvar);

            if let Some(goto) = self.p.goto.get_mut(i) {
                if goto_level > block_level {
                    goto.close |= block.prop().upval;
                }
                goto.nactvar = block.prop().nactvar;
            }
        }

        Ok(())
    }

    fn settablesize(&mut self, pc: usize, ra: usize, asize: usize, hsize: usize) {
        // may use extra

        self.p.emiter().modify_code(pc, |c| {
            *c = match *c {
                InterCode::NEWTABLE(..) => {
                    InterCode::NEWTABLE(ra as u8, hsize as u8, asize as u8, false)
                }
                _ => *c,
            }
        })
    }

    fn self_exp(&mut self, texp: Expr, kexp: Expr) -> Result<Expr, ParseErr> {
        let texp = self.p.pvar().exp_toanyreg(texp)?;
        let treg = self.p.pvar().locreg(&texp)?;
        self.p.pvar().exp_free(&texp)?;

        let reg = self.p.fs.prop().freereg;
        let exp = Expr::nonreloc(reg);
        self.p.pvar().reserver_regs(2);

        let kexp = self.p.pvar().exp_tokreg(kexp)?;
        match kexp.value {
            ExprValue::Nonreloc(kreg) => self.p.emiter().emit_self(reg, treg, kreg, false),
            ExprValue::K(kreg) => self.p.emiter().emit_self(reg, treg, kreg, true),
            _ => unreachable!(),
        };
        self.p.pvar().exp_free(&kexp)?;

        Ok(exp)
    }
}
