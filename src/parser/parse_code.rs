use crate::{
    code::{codelimit, InterCode},
    object::{Expr, ExprValue},
};

use super::{BinOpr, ParseErr, Parser, UnOpr};

pub(super) struct ParseCode<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseCode<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
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
                let e1 = self.p.preg().exp_toanyreg(e1)?;
                let reg = self.p.preg().locreg(&e1)?;
                let pc = self.arith_imm(op, reg, value as u8)?;

                self.p.pfscope().exp_free(&e1)?;

                Ok(Expr::reloc(pc))
            }
            ExprValue::Integer(_) | ExprValue::Float(_) | ExprValue::String(_) => {
                let e2 = self.p.preg().exp_tok(e2)?;
                self.arith_inreg(op, e1, e2)
            }
            ExprValue::K(k) if allowk => {
                let e1 = self.p.preg().exp_toanyreg(e1)?;
                let reg = self.p.preg().locreg(&e1)?;
                let pc = self.arith_k(op, reg, k)?;

                self.p.pfscope().exp_free(&e1)?;

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

                let e1 = self.p.preg().exp_toanyreg(e1)?;
                self.arith(op, e1, e2)
            } else if e1.numeric() && !e2.numeric() && swapable {
                log::debug!("parse arith op: {}, all not inreg, e2 not immediate", op);

                let e2 = self.p.preg().exp_toanyreg(e2)?;
                self.arith(op, e1, e2)
            } else {
                log::debug!("parse arith op: {}, all not inreg, all not immediate", op);

                let e1 = self.p.preg().exp_toanyreg(e1)?;

                self.arith(op, e1, e2)
            }
        } else if e1.inreg() && !e2.inreg() {
            log::debug!("parse arith op: {}, e2 not inreg", op);

            let e1 = self.p.preg().exp_toanyreg(e1)?;

            self.arith_inreg(op, e1, e2)
        } else if !e1.inreg() && e2.inreg() && swapable {
            log::debug!("parse arith op: {}, e1 not inreg", op);

            let e2 = self.p.preg().exp_toanyreg(e2)?;

            self.arith_inreg(op, e2, e1)
        } else {
            log::debug!(
                "parse arith op: {}, e1: {}; e2: {}, all inreg",
                op,
                e1.value,
                e2.value
            );

            let e1 = self.p.preg().exp_toanyreg(e1)?;
            let e2 = self.p.preg().exp_toanyreg(e2)?;

            let r1 = self.p.preg().locreg(&e1)?;
            let r2 = self.p.preg().locreg(&e2)?;

            let pc = self.arith_allreg(op, r1, r2)?;

            self.p.pfscope().exps_free(&e1, &e2)?;

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
                let e2 = self.p.preg().exp_tonextreg(e2)?;

                let pc = self.p.emiter().pc() - 1;
                match self.p.emiter().get_code(pc) {
                    Some(InterCode::CONCAT(pra, prb)) => match e1.value {
                        ExprValue::Nonreloc(reg) if reg + 1 == pra as usize => {
                            self.p.emiter().set_ra(pc, reg);
                            self.p.emiter().set_rb(pc, prb + 1);

                            self.p.pfscope().exp_free(&e2)?;

                            Ok(e1)
                        }
                        _ => Err(ParseErr::BadUsage),
                    },
                    _ => match e1.value {
                        ExprValue::Nonreloc(reg) => {
                            self.p.emiter().emit_concat(reg);

                            self.p.pfscope().exp_free(&e2)?;

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
        let exp = self.p.preg().exp_toanyreg(exp)?;
        let r = self.p.preg().locreg(&exp)?;
        self.p.pfscope().exp_free(&exp)?;

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
                        let exp = self.p.preg().discharge_toanyreg(exp)?;
                        let r = self.p.preg().locreg(&exp)?;
                        self.p.pfscope().exp_free(&exp)?;

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

        let exp = self.p.preg().discharge_toanyreg(exp)?;
        self.p.pfscope().exp_free(&exp)?;
        let reg = self.p.preg().locreg(&exp)?;

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

        let e1 = self.p.preg().exp_toanyreg(e1)?;
        let ra = self.p.preg().locreg(&e1)?;
        let k = matches!(op, BinOpr::EQ);

        match e2.value {
            ExprValue::Integer(value) if 0 <= value && value as u32 <= codelimit::MAX_SBX => {
                self.p.emiter().emit_eqi(ra, value as u32, k);

                self.p.pfscope().exp_free(&e1)?;
            }
            _ => {
                let e2 = self.p.preg().exp_tokreg(e2)?;
                match e2.value {
                    ExprValue::Nonreloc(rb) => {
                        self.p.emiter().emit_eq(ra, rb, k);

                        self.p.pfscope().exps_free(&e1, &e2)?;
                    }
                    ExprValue::K(rb) => {
                        self.p.emiter().emit_eqk(ra, rb, k);

                        self.p.pfscope().exp_free(&e1)?;
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
            let e1 = self.p.preg().exp_toanyreg(e1)?;
            let r1 = self.p.preg().locreg(&e1)?;
            let imm = match e2.value {
                ExprValue::Integer(v) => v,
                _ => unreachable!(),
            };

            match op {
                BinOpr::LT => self.p.emiter().emit_lti(r1, imm as u32, true),
                BinOpr::LE => self.p.emiter().emit_lei(r1, imm as u32, true),
                _ => unreachable!(),
            };
            self.p.pfscope().exp_free(&e1)?;
        } else if matches!(e1.value, ExprValue::Integer(v) if 0 <= v && v as u32 <= codelimit::MAX_SBX)
        {
            let e2 = self.p.preg().exp_toanyreg(e2)?;
            let r2 = self.p.preg().locreg(&e2)?;
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
            self.p.pfscope().exp_free(&e2)?;
        } else {
            let e1 = self.p.preg().exp_toanyreg(e1)?;
            let e2 = self.p.preg().exp_toanyreg(e2)?;
            let r1 = self.p.preg().locreg(&e1)?;
            let r2 = self.p.preg().locreg(&e2)?;

            match op {
                BinOpr::LT => self.p.emiter().emit_lt(r1, r2, true),
                BinOpr::LE => self.p.emiter().emit_le(r1, r2, true),
                _ => unreachable!(),
            };

            self.p.pfscope().exps_free(&e1, &e2)?;
        }

        Ok(Expr::jmp(self.p.emiter().emit_jmp()))
    }

    pub(super) fn setreturns(&mut self, exp: Expr, nresults: usize) -> Result<(), ParseErr> {
        match exp.value {
            ExprValue::Call(pc) => {
                self.p.emiter().set_rc(pc, nresults as u8 + 1);

                Ok(())
            }
            ExprValue::VarArg(pc) => {
                let freereg = self.p.fs.prop().freereg;

                self.p.emiter().set_rc(pc, nresults as u8 + 1);
                self.p.emiter().set_ra(pc, freereg);

                self.p.pfscope().reserver_regs(1);

                Ok(())
            }
            _ => Err(ParseErr::BadUsage),
        }
    }
}
