use crate::{
    check_token,
    code::{codelimit, InterCode},
    consume_token,
    lexer::Token,
    matches_token,
    object::{Expr, ExprValue, TableCtor},
};

use super::{binopr::BinOpr, unopr::UnOpr, ParseErr, Parser};

impl Parser {
    fn selfexp(&mut self, texp: Expr, kexp: Expr) -> Result<Expr, ParseErr> {
        let texp = self.expanyreg(texp)?;
        let treg = self.nonreloc(&texp)?;
        self.freeexp(&texp)?;

        let reg = self.fs.prop().freereg;
        let exp = Expr::nonreloc(reg);
        self.reserve(2);

        let kexp = self.expkreg(kexp)?;
        match kexp.value {
            ExprValue::Nonreloc(kreg) => self.emit_self(reg, treg, kreg, false),
            ExprValue::K(kreg) => self.emit_self(reg, treg, kreg, true),
            _ => unreachable!(),
        };
        self.freeexp(&kexp)?;

        Ok(exp)
    }

    fn jump_patch(&mut self, pc: usize, dpc: usize) -> Result<(), ParseErr> {
        let offset = dpc as i64 - (pc + 1) as i64;
        self.set_sj(pc, offset);

        Ok(())
    }

    pub(super) fn patch_testreg(&mut self, node: usize, reg: Option<usize>) -> bool {
        let (pc, ins) = self.get_ctrljump(node);

        match ins {
            Some(InterCode::TESTSET(_, rb, k)) => {
                match reg {
                    Some(reg) if reg != rb as usize => self.set_ra(pc, reg),

                    _ => self.modify_code(pc, |c| *c = InterCode::TEST(rb, k)),
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
            let next = self.get_jump(pc);

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

    pub(super) fn jump_patchlist(
        &mut self,
        list: Option<usize>,
        pc: Option<usize>,
    ) -> Result<(), ParseErr> {
        self.jump_patchlistaux(list, pc, None, pc)
    }

    pub(super) fn jump_patchtohere(&mut self, list: Option<usize>) -> Result<(), ParseErr> {
        let here = self.mark_pc();
        self.jump_patchlist(list, Some(here))
    }

    pub(super) fn jump_concatlist(
        &mut self,
        l1: &mut Option<usize>,
        l2: Option<usize>,
    ) -> Result<(), ParseErr> {
        if l2.is_none() {
            Ok(())
        } else if l1.is_none() {
            *l1 = l2;
            Ok(())
        } else {
            let mut list = l1.unwrap();
            loop {
                if let Some(next) = self.get_jump(list) {
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
            ExprValue::Reloc(pc) => match self.get_code(pc) {
                Some(InterCode::NOT(_, rb)) => {
                    self.pop();

                    self.emit_testset(rb, !cond);
                    return Ok((self.emit_jmp(), exp));
                }
                _ => {}
            },
            _ => {}
        };

        let exp = self.discanyreg(exp)?;
        self.freeexp(&exp)?;
        let reg = self.nonreloc(&exp)?;

        self.emit_testset(reg, cond);
        Ok((self.emit_jmp(), exp))
    }

    pub(super) fn goiftrue(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        let mut exp = self.discvar(exp)?;

        match exp.value {
            ExprValue::Jump(pc) => {
                self.negate_cond(pc);

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
        let mut exp = self.discvar(exp)?;

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

    pub(super) fn patch_forjump(&mut self, pc: usize, dest: usize, back: bool) {
        let offset = dest as i32 - (pc + 1) as i32;

        self.set_bx(pc, if back { -offset } else { offset });
    }

    fn unarith(&mut self, op: UnOpr, exp: Expr) -> Result<Expr, ParseErr> {
        let exp = self.expanyreg(exp)?;
        let r = self.nonreloc(&exp)?;
        self.freeexp(&exp)?;

        match op {
            UnOpr::MINUS => Ok(Expr::reloc(self.emit_unm(r))),
            UnOpr::BNOT => Ok(Expr::reloc(self.emit_bnot(r))),
            UnOpr::LEN => Ok(Expr::reloc(self.emit_len(r))),
            _ => Err(ParseErr::BadUsage),
        }
    }

    fn rmvalues(&mut self, list: Option<usize>) {
        let mut list = list;
        while let Some(pc) = list {
            self.patch_testreg(pc, None);

            list = self.get_jump(pc);
        }
    }

    fn prefix(&mut self, op: UnOpr, exp: Expr) -> Result<Expr, ParseErr> {
        let exp = self.discvar(exp)?;

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
                        self.negate_cond(pc);

                        Ok(exp)
                    }

                    ExprValue::Reloc(_) | ExprValue::Nonreloc(_) => {
                        let exp = self.discanyreg(exp)?;
                        let r = self.nonreloc(&exp)?;
                        self.freeexp(&exp)?;

                        Ok(Expr::reloc(self.emit_not(r)))
                    }
                    _ => Err(ParseErr::BadUsage),
                }?
                .tj(fj)
                .fj(tj);

                self.rmvalues(exp.false_jumpto);
                self.rmvalues(exp.true_jumpto);

                Ok(exp)
            }
            _ => Err(ParseErr::BadUsage),
        }
    }

    fn infix(&mut self, op: BinOpr, exp: Expr) -> Result<Expr, ParseErr> {
        let exp = self.discvar(exp)?;

        match op {
            BinOpr::AND => self.goiftrue(exp),
            BinOpr::OR => self.goiffalse(exp),
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
                _ => self.expanyreg(exp),
            },
            BinOpr::CONCAT => self.expnextreg(exp),
            BinOpr::EQ | BinOpr::NE => self.expkreg(exp),
            BinOpr::LT | BinOpr::LE | BinOpr::GT | BinOpr::GE => self.expanyreg(exp),
            _ => Err(ParseErr::BadUsage),
        }
    }

    fn cmp_eq(&mut self, op: BinOpr, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        let e1 = self.expanyreg(e1)?;
        let ra = self.nonreloc(&e1)?;
        let k = matches!(op, BinOpr::EQ);

        match e2.value {
            ExprValue::Integer(value) if 0 <= value && value as u32 <= codelimit::MAX_SBX => {
                self.emit_eqi(ra, value as u32, k);

                self.freeexp(&e1)?;
            }
            _ => {
                let e2 = self.expkreg(e2)?;
                match e2.value {
                    ExprValue::Nonreloc(rb) => {
                        self.emit_eq(ra, rb, k);

                        self.freeexps(&e1, &e2)?;
                    }
                    ExprValue::K(rb) => {
                        self.emit_eqk(ra, rb, k);

                        self.freeexp(&e1)?;
                    }
                    _ => return Err(ParseErr::BadUsage),
                }
            }
        };

        Ok(Expr::jmp(self.emit_jmp()))
    }

    fn cmp_order(&mut self, op: BinOpr, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        let (op, e1, e2) = match op {
            BinOpr::LT | BinOpr::LE => (op, e1, e2),
            BinOpr::GE => (BinOpr::LE, e2, e1),
            BinOpr::GT => (BinOpr::LT, e2, e1),
            _ => return Err(ParseErr::BadUsage),
        };

        if matches!(e2.value, ExprValue::Integer(v) if 0 <= v && v as u32 <= codelimit::MAX_SBX) {
            let e1 = self.expanyreg(e1)?;
            let r1 = self.nonreloc(&e1)?;
            let imm = match e2.value {
                ExprValue::Integer(v) => v,
                _ => unreachable!(),
            };

            match op {
                BinOpr::LT => self.emit_lti(r1, imm as u32, true),
                BinOpr::LE => self.emit_lei(r1, imm as u32, true),
                _ => unreachable!(),
            };
            self.freeexp(&e1)?;
        } else if matches!(e1.value, ExprValue::Integer(v) if 0 <= v && v as u32 <= codelimit::MAX_SBX)
        {
            let e2 = self.expanyreg(e2)?;
            let r2 = self.nonreloc(&e2)?;
            let imm = match e1.value {
                ExprValue::Integer(v) => v,
                _ => unreachable!(),
            };

            match op {
                BinOpr::LT => self.emit_gti(r2, imm as u32, true),
                BinOpr::LE => self.emit_gei(r2, imm as u32, true),
                _ => unreachable!(),
            };
            self.freeexp(&e2)?;
        } else {
            let e1 = self.expanyreg(e1)?;
            let e2 = self.expanyreg(e2)?;
            let r1 = self.nonreloc(&e1)?;
            let r2 = self.nonreloc(&e2)?;

            match op {
                BinOpr::LT => self.emit_lt(r1, r2, true),
                BinOpr::LE => self.emit_le(r1, r2, true),
                _ => unreachable!(),
            };

            self.freeexps(&e1, &e2)?;
        }

        Ok(Expr::jmp(self.emit_jmp()))
    }

    fn arith_imm(&mut self, op: BinOpr, reg: usize, imm: u8) -> Result<usize, ParseErr> {
        Ok(match op {
            BinOpr::ADD => self.emit_addi(reg, imm),
            BinOpr::SHL => self.emit_shli(reg, imm),
            BinOpr::SHR => self.emit_shri(reg, imm),
            _ => return Err(ParseErr::BadUsage),
        })
    }

    fn arith_allreg(&mut self, op: BinOpr, r1: usize, r2: usize) -> Result<usize, ParseErr> {
        Ok(match op {
            BinOpr::ADD => self.emit_add(r1, r2),
            BinOpr::SUB => self.emit_sub(r1, r2),
            BinOpr::MUL => self.emit_mul(r1, r2),
            BinOpr::DIV => self.emit_div(r1, r2),
            BinOpr::IDIV => self.emit_idiv(r1, r2),
            BinOpr::MOD => self.emit_mod(r1, r2),
            BinOpr::POW => self.emit_pow(r1, r2),
            BinOpr::SHL => self.emit_shl(r1, r2),
            BinOpr::SHR => self.emit_shr(r1, r2),
            BinOpr::BAND => self.emit_band(r1, r2),
            BinOpr::BOR => self.emit_bor(r1, r2),
            BinOpr::BXOR => self.emit_bxor(r1, r2),
            _ => return Err(ParseErr::BadUsage),
        })
    }

    fn arith_k(&mut self, op: BinOpr, reg: usize, k: usize) -> Result<usize, ParseErr> {
        Ok(match op {
            BinOpr::ADD => self.emit_addk(reg, k),
            BinOpr::SUB => self.emit_subk(reg, k),
            BinOpr::MUL => self.emit_mulk(reg, k),
            BinOpr::DIV => self.emit_divk(reg, k),
            BinOpr::IDIV => self.emit_idivk(reg, k),
            BinOpr::MOD => self.emit_modk(reg, k),
            BinOpr::POW => self.emit_powk(reg, k),
            BinOpr::BAND => self.emit_bandk(reg, k),
            BinOpr::BOR => self.emit_bork(reg, k),
            BinOpr::BXOR => self.emit_bxork(reg, k),
            _ => return Err(ParseErr::BadUsage),
        })
    }

    fn arith_iimdir(&self, op: BinOpr, v1: i64, v2: i64) -> Result<Expr, ParseErr> {
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
                let e1 = self.expanyreg(e1)?;
                let reg = self.nonreloc(&e1)?;
                let pc = self.arith_imm(op, reg, value as u8)?;

                self.freeexp(&e1)?;

                Ok(Expr::reloc(pc))
            }
            ExprValue::Integer(_) | ExprValue::Float(_) | ExprValue::String(_) => {
                let e2 = self.expk(e2)?;
                self.arith_inreg(op, e1, e2)
            }
            ExprValue::K(k) if allowk => {
                let e1 = self.expanyreg(e1)?;
                let reg = self.nonreloc(&e1)?;
                let pc = self.arith_k(op, reg, k)?;

                self.freeexp(&e1)?;

                Ok(Expr::reloc(pc))
            }
            _ => Err(ParseErr::BadUsage),
        }
    }

    fn arith(&mut self, op: BinOpr, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        let swapable = matches!(
            op,
            BinOpr::ADD | BinOpr::MUL | BinOpr::BAND | BinOpr::BOR | BinOpr::BXOR
        );
        if !e1.inreg() && !e2.inreg() {
            if e1.numeric() && e2.numeric() {
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
                let e1 = self.expanyreg(e1)?;
                self.arith(op, e1, e2)
            } else if e1.numeric() && !e2.numeric() && swapable {
                let e2 = self.expanyreg(e2)?;
                self.arith(op, e1, e2)
            } else {
                let e1 = self.expanyreg(e1)?;
                self.arith(op, e1, e2)
            }
        } else if e1.inreg() && !e2.inreg() {
            let e1 = self.expanyreg(e1)?;
            self.arith_inreg(op, e1, e2)
        } else if !e1.inreg() && e2.inreg() && swapable {
            let e2 = self.expanyreg(e2)?;
            self.arith_inreg(op, e2, e1)
        } else {
            let e1 = self.expanyreg(e1)?;
            let e2 = self.expanyreg(e2)?;

            let r1 = self.nonreloc(&e1)?;
            let r2 = self.nonreloc(&e2)?;

            let pc = self.arith_allreg(op, r1, r2)?;

            self.freeexps(&e1, &e2)?;

            Ok(Expr::reloc(pc))
        }
    }

    fn posfix(&mut self, op: BinOpr, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        let e2 = self.discvar(e2)?;

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
                let e2 = self.expnextreg(e2)?;

                let pc = self.pc() - 1;
                match self.get_code(pc) {
                    Some(InterCode::CONCAT(pra, prb)) => match e1.value {
                        ExprValue::Nonreloc(reg) if reg + 1 == pra as usize => {
                            self.set_ra(pc, reg);
                            self.set_rb(pc, prb + 1);

                            self.freeexp(&e2)?;

                            Ok(e1)
                        }
                        _ => Err(ParseErr::BadUsage),
                    },
                    _ => match e1.value {
                        ExprValue::Nonreloc(reg) => {
                            self.emit_concat(reg);

                            self.freeexp(&e2)?;

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

    fn settablesize(&mut self, pc: usize, ra: usize, asize: usize, hsize: usize) {
        // may use extra

        self.modify_code(pc, |c| {
            *c = match *c {
                InterCode::NEWTABLE(..) => InterCode::NEWTABLE(ra, hsize, asize, false),
                _ => *c,
            }
        })
    }

    pub(super) fn expsetreg(&mut self) -> Result<(), ParseErr> {
        let exp = self.exp()?;
        self.expnextreg(exp)?;

        Ok(())
    }

    // primary_exp ::= name | `(` exp `)`
    fn primary_exp(&mut self) -> Result<Expr, ParseErr> {
        match &self.lexer.token {
            Token::Operator('(') => {
                self.skip()?;
                let exp = self.exp()?;
                consume_token!(self, Token::Operator(')'))?;

                self.discvar(exp)
            }
            Token::Name(name) => {
                let name = name.clone();
                self.skip()?;

                self.singlevar(name.as_str())
            }
            _ => Err(ParseErr::BadUsage),
        }
    }

    // index_exp = `[` exp `]`
    fn index_exp(&mut self) -> Result<Expr, ParseErr> {
        self.skip()?;
        let exp = self.exp()?;
        consume_token!(self, Token::Operator(']'))?;

        Ok(exp)
    }

    // explist_exp -> expr { `,` expr }
    pub(super) fn explist_exp(&mut self) -> Result<(usize, Expr), ParseErr> {
        let mut n = 1;
        let mut exp = self.exp()?;

        while check_token!(self, Token::Operator(',')) {
            self.expnextreg(exp)?;

            exp = self.exp()?;
            n += 1;
        }

        Ok((n, exp))
    }

    // args_exp ::= ( `(` [ exprlist ] `)` | ctor_exp | string )
    fn args_exp(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        let argexp = match &self.lexer.token {
            Token::Operator('(') => {
                // parse `(`
                self.skip()?;

                let exp = if matches_token!(self, Token::Operator(')')) {
                    Expr::void()
                } else {
                    let (_, exp) = self.explist_exp()?;
                    if exp.hasmultret() {
                        self.setret(&exp, 254)?;
                    }

                    exp
                };
                consume_token!(self, Token::Operator(')'))?;

                exp
            }
            Token::Operator('{') => self.ctor_exp()?,
            Token::String(val) => {
                let exp = Expr::from(val);

                self.skip()?;
                exp
            }
            _ => return Err(ParseErr::BadUsage),
        };

        let base = self.nonreloc(&exp)?;
        let nparams = if argexp.hasmultret() {
            255
        } else {
            if !matches!(argexp.value, ExprValue::Void) {
                self.expnextreg(argexp)?;
            }
            self.fs.prop().freereg - (base + 1)
        };

        let pc = self.emit_call(base, nparams + 1, 2);

        self.fs.prop_mut().freereg = base + 1;

        Ok(Expr::call(pc))
    }

    // suffixed_exp ::= primary_exp { `.` name | `[` exp `]` | `:` name arg_exp | arg_exp }
    fn suffixed_exp(&mut self) -> Result<Expr, ParseErr> {
        let mut exp = self.primary_exp()?;

        loop {
            exp = match &self.lexer.token {
                Token::Operator('.') => self.field_stmt(exp)?,
                Token::Operator('[') => {
                    let texp = self.expanyregup(exp)?;
                    let kexp = self.index_exp()?;

                    self.indexed(texp, kexp)?
                }
                Token::Operator(':') => {
                    self.skip()?;
                    let name = self.name()?;
                    let kexp = Expr::from(name.as_str());
                    let exp = self.selfexp(exp, kexp)?;

                    self.args_exp(exp)?
                }
                Token::Operator('(' | '{') | Token::String(..) => {
                    let exp = self.expnextreg(exp)?;

                    self.args_exp(exp)?
                }
                _ => break Ok(exp),
            }
        }
    }

    // recfield_exp ::= ( name | index_exp ) = exp
    pub(super) fn recfield_exp(&mut self, tc: &mut TableCtor) -> Result<(), ParseErr> {
        let memreg = self.fs.prop().freereg;

        let kexp = match &self.lexer.token {
            Token::Name(name) => {
                let exp = Expr::from(name);

                // parse name
                self.skip()?;
                exp
            }
            Token::Operator('[') => self.index_exp()?,
            _ => return Err(ParseErr::BadUsage),
        };
        tc.nh += 1;

        // parse `=`
        consume_token!(self, Token::Operator('='))?;

        let tkexp = self.indexed(tc.texp.clone(), kexp)?;
        // parse exp
        let vexp = self.exp()?;
        self.storevar(&tkexp, vexp)?;

        self.fs.prop_mut().freereg = memreg;

        Ok(())
    }

    // listfield_exp ::= exp
    pub(super) fn listfield_exp(&mut self, tc: &mut TableCtor) -> Result<(), ParseErr> {
        tc.lexp = self.exp()?;
        tc.tostore += 1;

        Ok(())
    }

    // field_exp ::= listfield_exp | recfield_exp
    pub(super) fn field_exp(&mut self, tc: &mut TableCtor) -> Result<(), ParseErr> {
        match &self.lexer.token {
            Token::Name(..) => {
                let lookahead = self.lookahead()?;
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

    // ctor_exp ::= `{` [ field_exp { sep field_exp } [ sep ] ] `}`
    // sep ::= `,` | `;`
    fn ctor_exp(&mut self) -> Result<Expr, ParseErr> {
        let mut tc = TableCtor::new();
        let pc = self.emit_newtable();
        self.emit_nop();

        let treg = self.fs.prop().freereg;
        tc.texp = Expr::nonreloc(treg);
        self.reserve(1);

        // parse `{`
        consume_token!(self, Token::Operator('{'))?;
        loop {
            if matches_token!(self, Token::Operator('}')) {
                break;
            }

            // close listfield
            if !matches!(tc.lexp.value, ExprValue::Void) {
                self.expnextreg(tc.lexp.clone())?;
                tc.lexp = Expr::void();

                if tc.tostore == 50 {
                    // flush, setlist
                    self.emit_setlist(treg, tc.tostore, tc.na);
                    tc.na += tc.tostore;
                    tc.tostore = 0;
                }
            }
            // parse field_exp
            self.field_exp(&mut tc)?;

            // parse sep
            if !check_token!(self, Token::Operator(',' | ';')) {
                break;
            }
        }
        consume_token!(self, Token::Operator('}'))?;

        // last listfield
        if tc.tostore != 0 {
            if tc.lexp.hasmultret() {
                self.setret(&tc.lexp, 254)?;
                self.emit_setlist(treg, 0, tc.na);
                tc.na -= 1;
            } else {
                if !matches!(tc.lexp.value, ExprValue::Void) {
                    self.expnextreg(tc.lexp.clone())?;
                }
                self.emit_setlist(treg, tc.tostore, tc.na);
            }

            tc.na += tc.tostore;
        }

        self.settablesize(pc, treg, tc.na, tc.nh);

        Ok(tc.texp)
    }

    // simple_exp ::= FLT | INT | STRING | NIL | TRUE | FALSE | ... | ctor_exp
    //              | `function` body_stmt | suffix_exp
    fn simple_exp(&mut self) -> Result<Expr, ParseErr> {
        let exp = match &self.lexer.token {
            Token::Integer(v) => Expr::from(v),
            Token::Number(v) => Expr::from(v),
            Token::String(v) => Expr::from(v),
            Token::Nil => Expr::nil(),
            Token::True => Expr::from(true),
            Token::False => Expr::from(false),
            Token::Dots => {
                if !self.fs.prop().proto.prop().vararg {
                    return Err(ParseErr::BadUsage);
                }
                Expr::vararg(self.emit_vararg())
            }
            Token::Operator('{') => return self.ctor_exp(),
            Token::Function => {
                self.skip()?;

                return self.body_stmt(false);
            }
            _ => return self.suffixed_exp(),
        };
        self.skip()?;

        Ok(exp)
    }

    // sub_exp ::= (simple_exp | unop sub_exp) { binop sub_exp }
    fn sub_exp(&mut self, limit: u8) -> Result<(BinOpr, Expr), ParseErr> {
        let unop = UnOpr::from(&self.lexer.token);

        let mut exp = if matches!(unop, UnOpr::NoOpr) {
            self.simple_exp()?
        } else {
            self.skip()?;
            let (_, exp) = self.sub_exp(BinOpr::UNARY_PRI)?;
            self.prefix(unop, exp)?
        };

        let mut binop = BinOpr::from(&self.lexer.token);
        while !matches!(binop, BinOpr::NoOpr) && binop.lpri() > limit {
            // parse binop
            self.skip()?;

            exp = self.infix(binop, exp)?;
            let (nbop, nexp) = self.sub_exp(binop.rpri())?;
            exp = self.posfix(binop, exp, nexp)?;

            binop = nbop;
        }

        Ok((binop, exp))
    }

    // cond_exp ::= exp
    pub(super) fn cond_exp(&mut self) -> Result<usize, ParseErr> {
        let mut exp = self.exp()?;
        if matches!(exp.value, ExprValue::Nil) {
            exp.value = ExprValue::Bool(false);
        }

        let exp = self.goiftrue(exp)?;
        exp.false_jumpto.ok_or(ParseErr::BadUsage)
    }

    // exp ::= sub_exp
    pub(super) fn exp(&mut self) -> Result<Expr, ParseErr> {
        let (_, exp) = self.sub_exp(BinOpr::INIT_PRI)?;

        Ok(exp)
    }

    // assigment_stmt ::= suffixed_exp restassign_stmt
    // restassign_stmt ::= ',' suffixed_exp restassign_stmt | `=` explist_exp
    fn restassign_stmt(&mut self, assignlist: &mut Vec<Expr>) -> Result<(), ParseErr> {
        // parse `,`
        if check_token!(self, Token::Operator(',')) {
            // parse suffixed_exp
            let exp = self.suffixed_exp()?;
            assignlist.push(exp);

            // parse restassign_stmt
            self.restassign_stmt(assignlist)?;
        } else {
            // parse `=`
            consume_token!(self, Token::Operator('='))?;

            // parse exprlist_exp
            let (nexps, exp) = self.explist_exp()?;
            if nexps != assignlist.len() {
                self.adjassign(assignlist.len(), nexps, exp)?;
            } else {
                let exp = self.setoneret(exp)?;
                match assignlist.last() {
                    Some(var) => self.storevar(var, exp)?,
                    _ => return Err(ParseErr::BadUsage),
                }

                return Ok(());
            }
        }

        let exp = Expr::nonreloc(self.fs.prop().freereg - 1);
        match assignlist.last() {
            Some(var) => self.storevar(var, exp)?,
            _ => return Err(ParseErr::BadUsage),
        }

        Ok(())
    }

    // exp_stmt -> func_stmt | assigment_stmt
    pub(super) fn exp_stmt(&mut self) -> Result<(), ParseErr> {
        let mut assignlist = Vec::<Expr>::new();

        // parse suffixed_exp
        let exp = self.suffixed_exp()?;
        assignlist.push(exp);

        if matches_token!(self, Token::Operator('=' | ',')) {
            // assignment_stmt

            self.restassign_stmt(&mut assignlist)?;
        } else {
            // func_stmt

            match assignlist.last().and_then(|e| Some(&e.value)) {
                Some(ExprValue::Call(pc)) => self.set_rc(*pc, 1),
                _ => {}
            }
        }

        Ok(())
    }
}
