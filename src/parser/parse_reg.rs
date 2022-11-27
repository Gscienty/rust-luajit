use crate::code::{codelimit, InterCode};

use super::{BinOpr, Expr, ExprValue, ParseErr, Parser};

pub(super) struct ParseReg<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseReg<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
    }

    pub(super) fn stok(&mut self, value: &str) -> Result<Expr, ParseErr> {
        let k = self.p.constant_pool.borrow_mut().adds(value);

        Ok(Expr::k(k))
    }

    pub(super) fn ftok(&mut self, value: f64) -> Result<Expr, ParseErr> {
        let k = self.p.constant_pool.borrow_mut().addf(value);

        Ok(Expr::k(k))
    }

    pub(super) fn itok(&mut self, value: i64) -> Result<Expr, ParseErr> {
        let k = self.p.constant_pool.borrow_mut().addi(value);

        Ok(Expr::k(k))
    }

    pub(super) fn ntok(&mut self) -> Result<Expr, ParseErr> {
        let k = self.p.constant_pool.borrow_mut().addn();

        Ok(Expr::k(k))
    }

    pub(super) fn btok(&mut self, value: bool) -> Result<Expr, ParseErr> {
        let k = self.p.constant_pool.borrow_mut().addb(value);

        Ok(Expr::k(k))
    }

    pub(super) fn exp_tok(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        match exp.value {
            ExprValue::Bool(v) => self.btok(v),
            ExprValue::Nil => self.ntok(),
            ExprValue::Integer(v) => self.itok(v),
            ExprValue::Float(v) => self.ftok(v),
            ExprValue::String(v) => self.stok(v.as_str()),
            _ => Err(ParseErr::BadUsage),
        }
    }

    pub(super) fn exp_tokreg(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        let istok = matches!(
            exp.value,
            ExprValue::Bool(_)
                | ExprValue::Nil
                | ExprValue::Integer(_)
                | ExprValue::Float(_)
                | ExprValue::String(_)
        );

        if istok {
            self.exp_tok(exp)
        } else {
            self.exp_toanyreg(exp)
        }
    }

    pub(super) fn infix(&mut self, op: BinOpr, exp: Expr) -> Result<Expr, ParseErr> {
        log::debug!("parse infix, op: {}", op);
        // TODO

        match op {
            BinOpr::AND => self.p.parse_code().goiftrue(exp),
            BinOpr::OR => self.p.parse_code().goiffalse(exp),
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
                _ => self.exp_toanyreg(exp),
            },
            BinOpr::CONCAT => self.exp_tonextreg(exp),
            BinOpr::EQ | BinOpr::NE => self.exp_tokreg(exp),
            BinOpr::LT | BinOpr::LE | BinOpr::GT | BinOpr::GE => self.exp_toanyreg(exp),
            _ => Err(ParseErr::BadUsage),
        }
    }

    pub(super) fn discharge_toreg(&mut self, exp: Expr, reg: usize) -> Result<Expr, ParseErr> {
        match exp.value {
            ExprValue::Nil => {
                self.p.parse_code().emit_loadnil(reg, 1);

                Ok(Expr::nonreloc(reg))
            }
            ExprValue::Bool(value) => {
                self.p.parse_code().emit_loadbool(reg, value);

                Ok(Expr::nonreloc(reg))
            }
            ExprValue::String(value) => {
                let exp = self.stok(&value)?;
                self.discharge_toreg(exp, reg)
            }
            ExprValue::Float(value) => {
                if 0f64 <= value
                    && value as u32 <= codelimit::MAX_SBX
                    && value as u32 as f64 == value
                {
                    self.p.parse_code().emit_loadfloat(reg, value);
                    Ok(Expr::nonreloc(reg))
                } else {
                    let exp = self.ftok(value)?;
                    self.discharge_toreg(exp, reg)
                }
            }
            ExprValue::Integer(value) => {
                if 0 <= value && value as u32 <= codelimit::MAX_SBX {
                    self.p.parse_code().emit_loadint(reg, value);
                    Ok(Expr::nonreloc(reg))
                } else {
                    let exp = self.itok(value)?;
                    self.discharge_toreg(exp, reg)
                }
            }
            ExprValue::K(kidx) => {
                self.p.parse_code().emit_loadk(reg, kidx);

                Ok(Expr::nonreloc(reg))
            }
            ExprValue::Reloc(pc) => {
                self.p.parse_code().set_ra(pc, reg);
                Ok(Expr::nonreloc(reg))
            }
            ExprValue::Nonreloc(nreg) => {
                if reg != nreg {
                    self.p.parse_code().emit_move(reg, nreg);
                }
                Ok(Expr::nonreloc(reg))
            }
            ExprValue::Jump(_) => Ok(exp),
            _ => Err(ParseErr::BadUsage),
        }
    }

    pub(super) fn discharge_toanyreg(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        match exp.value {
            ExprValue::Nonreloc(_) => Ok(exp),
            _ => {
                self.p.reserver_regs(1);
                self.discharge_toreg(exp, self.p.freereg - 1)
            }
        }
    }

    pub(super) fn exps_free(&mut self, e1: Expr, e2: Expr) -> Result<(), ParseErr> {
        match e1.value {
            ExprValue::Nonreloc(r1) => match e2.value {
                ExprValue::Nonreloc(r2) => {
                    if r1 < r2 {
                        self.p.free_reg(r2)?;
                        self.p.free_reg(r1)?;
                    } else {
                        self.p.free_reg(r1)?;
                        self.p.free_reg(r2)?;
                    }
                }
                _ => self.p.free_reg(r1)?,
            },
            _ => match e2.value {
                ExprValue::Nonreloc(r2) => self.p.free_reg(r2)?,
                _ => {}
            },
        };

        Ok(())
    }

    pub(super) fn exp_free(&mut self, exp: Expr) -> Result<(), ParseErr> {
        match exp.value {
            ExprValue::Nonreloc(reg) => self.p.free_reg(reg),
            _ => Ok(()),
        }
    }

    // why use mut?
    fn need_value(&self, list: Option<usize>) -> bool {
        let mut list = list;
        while let Some(pc) = list {
            let (_, ins) = self.p.get_ctrljump(pc);
            match ins {
                Some(ins) if !matches!(ins, InterCode::TESTSET(_, _, _)) => return true,
                _ => list = self.p.get_jump(pc),
            }
        }

        false
    }

    pub(super) fn exp_toreg(&mut self, exp: Expr, reg: usize) -> Result<Expr, ParseErr> {
        let mut exp = self.discharge_toreg(exp, reg)?;
        match exp.value {
            ExprValue::Jump(pc) => self
                .p
                .parse_code()
                .jump_concatlist(&mut exp.true_jumpto, Some(pc))?,
            _ => {}
        };

        if exp.hasjump() {
            let mut pf = None;
            let mut pt = None;
            if self.need_value(exp.true_jumpto) || self.need_value(exp.false_jumpto) {
                let fj = match exp.value {
                    ExprValue::Jump(_) => None,
                    _ => Some(self.p.parse_code().emit_jmp()),
                };

                pf = Some(self.p.parse_code().emit_lfalseskip(reg));
                pt = Some(self.p.parse_code().emit_loadbool(reg, true));

                self.p.parse_code().jump_patchtohere(fj)?;
            }

            let final_pc = self.p.mark_label();

            self.p.parse_code().jump_patchlistaux(
                exp.false_jumpto,
                Some(final_pc),
                Some(reg),
                pf,
            )?;
            self.p.parse_code().jump_patchlistaux(
                exp.true_jumpto,
                Some(final_pc),
                Some(reg),
                pt,
            )?;
        }

        Ok(Expr::nonreloc(reg))
    }

    pub(super) fn exp_tonextreg(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        // TODO

        self.exp_free(exp.clone())?;
        self.p.reserver_regs(1);
        self.exp_toreg(exp, self.p.freereg - 1)
    }

    pub(super) fn exp_toanyreg(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        if matches!(exp.value, ExprValue::Nonreloc(_)) {
            // TODO

            Ok(exp)
        } else {
            self.exp_tonextreg(exp)
        }
    }

    pub(super) fn locreg(&mut self, exp: Expr) -> Result<usize, ParseErr> {
        match exp.value {
            ExprValue::Nonreloc(reg) => Ok(reg),
            _ => Err(ParseErr::BadUsage),
        }
    }
}
