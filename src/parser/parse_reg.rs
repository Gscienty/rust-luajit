use crate::code::{codelimit, InterCode};

use crate::object::{Expr, ExprValue, RefValue};

use super::{BinOpr, ParseErr, Parser};

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
        let tj = exp.true_jumpto;
        let fj = exp.false_jumpto;

        match exp.value {
            ExprValue::Bool(v) => self.btok(v),
            ExprValue::Nil => self.ntok(),
            ExprValue::Integer(v) => self.itok(v),
            ExprValue::Float(v) => self.ftok(v),
            ExprValue::String(v) => self.stok(v.as_str()),
            _ => Err(ParseErr::BadUsage),
        }
        .and_then(|exp| Ok(exp.tj(tj).fj(fj)))
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

    pub(super) fn exp_torefvalue(&self, exp: &Expr) -> Result<RefValue, ParseErr> {
        if exp.hasjump() {
            return Err(ParseErr::BadUsage);
        }

        match &exp.value {
            ExprValue::Bool(v) => Ok(RefValue::from(*v)),
            ExprValue::Nil => Ok(RefValue::new()),
            ExprValue::String(v) => Ok(RefValue::from(v.as_str())),
            ExprValue::Integer(v) => Ok(RefValue::from(*v)),
            ExprValue::Float(v) => Ok(RefValue::from(*v)),
            ExprValue::Const(idx) => match self.p.actvar.get(*idx) {
                Some(v) => Ok(v.val.clone()),
                _ => Err(ParseErr::BadUsage),
            },
            _ => Err(ParseErr::BadUsage),
        }
    }

    pub(super) fn infix(&mut self, op: BinOpr, exp: Expr) -> Result<Expr, ParseErr> {
        log::debug!("parse infix, op: {}, exp: {}", op, exp.value);

        let exp = self.p.pvar().discharge_tovar(exp)?;

        match op {
            BinOpr::AND => self.p.pcode().goiftrue(exp),
            BinOpr::OR => self.p.pcode().goiffalse(exp),
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
        let exp = self.p.pvar().discharge_tovar(exp)?;

        let tj = exp.true_jumpto;
        let fj = exp.false_jumpto;

        match exp.value {
            ExprValue::Nil => {
                self.p.emiter().emit_loadnil(reg, 1);

                Ok(Expr::nonreloc(reg))
            }
            ExprValue::Bool(value) => {
                self.p.emiter().emit_loadbool(reg, value);

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
                    self.p.emiter().emit_loadfloat(reg, value);
                    Ok(Expr::nonreloc(reg))
                } else {
                    let exp = self.ftok(value)?;
                    self.discharge_toreg(exp, reg)
                }
            }
            ExprValue::Integer(value) => {
                if 0 <= value && value as u32 <= codelimit::MAX_SBX {
                    self.p.emiter().emit_loadint(reg, value);
                    Ok(Expr::nonreloc(reg))
                } else {
                    let exp = self.itok(value)?;
                    self.discharge_toreg(exp, reg)
                }
            }
            ExprValue::K(kidx) => {
                self.p.emiter().emit_loadk(reg, kidx);

                Ok(Expr::nonreloc(reg))
            }
            ExprValue::Reloc(pc) => {
                self.p.emiter().set_ra(pc, reg);
                Ok(Expr::nonreloc(reg))
            }
            ExprValue::Nonreloc(nreg) => {
                if reg != nreg {
                    self.p.emiter().emit_move(reg, nreg);
                }
                Ok(Expr::nonreloc(reg))
            }
            ExprValue::Jump(_) => Ok(exp),
            _ => Err(ParseErr::BadUsage),
        }
        .and_then(|exp| Ok(exp.tj(tj).fj(fj)))
    }

    pub(super) fn discharge_toanyreg(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        match exp.value {
            ExprValue::Nonreloc(_) => Ok(exp),
            _ => {
                self.reserver_regs(1);

                let reg = self.p.fs.prop().freereg - 1;
                self.discharge_toreg(exp, reg)
            }
        }
    }

    fn need_value(&mut self, list: Option<usize>) -> bool {
        let mut list = list;
        while let Some(pc) = list {
            let (_, ins) = self.p.emiter().get_ctrljump(pc);
            match ins {
                Some(ins) if !matches!(ins, InterCode::TESTSET(_, _, _)) => return true,
                _ => list = self.p.emiter().get_jump(pc),
            }
        }

        false
    }

    pub(super) fn exp_toanyregup(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        if !matches!(exp.value, ExprValue::Upval(..)) || exp.hasjump() {
            self.exp_toanyreg(exp)
        } else {
            Ok(exp)
        }
    }

    pub(super) fn exp_toreg(&mut self, exp: Expr, reg: usize) -> Result<Expr, ParseErr> {
        log::debug!("exp toreg: exp {}, reg: {}", exp.value, reg);
        let mut exp = self.discharge_toreg(exp, reg)?;

        if let ExprValue::Jump(pc) = exp.value {
            self.p
                .pcode()
                .jump_concatlist(&mut exp.true_jumpto, Some(pc))?;
        }

        if exp.hasjump() {
            let mut pf = None;
            let mut pt = None;
            if self.need_value(exp.true_jumpto) || self.need_value(exp.false_jumpto) {
                let fj = match exp.value {
                    ExprValue::Jump(_) => None,
                    _ => Some(self.p.emiter().emit_jmp()),
                };

                pf = Some(self.p.emiter().emit_lfalseskip(reg));
                pt = Some(self.p.emiter().emit_loadbool(reg, true));

                self.p.pcode().jump_patchtohere(fj)?;
            }

            let final_pc = self.p.emiter().mark_pc();

            self.p
                .pcode()
                .jump_patchlistaux(exp.false_jumpto, Some(final_pc), Some(reg), pf)?;
            self.p
                .pcode()
                .jump_patchlistaux(exp.true_jumpto, Some(final_pc), Some(reg), pt)?;
        }

        Ok(Expr::nonreloc(reg))
    }

    pub(super) fn exp_tonextreg(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        log::debug!("exp tonextreg exp: {}", exp.value);

        let exp = self.p.pvar().discharge_tovar(exp)?;

        self.exp_free(&exp)?;
        self.reserver_regs(1);

        let reg = self.p.fs.prop().freereg - 1;
        self.exp_toreg(exp, reg)
    }

    pub(super) fn exp_toanyreg(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        let exp = self.p.pvar().discharge_tovar(exp)?;

        if matches!(exp.value, ExprValue::Nonreloc(_)) {
            Ok(exp)
        } else {
            self.exp_tonextreg(exp)
        }
    }

    pub(super) fn locreg(&mut self, exp: &Expr) -> Result<usize, ParseErr> {
        match exp.value {
            ExprValue::Nonreloc(reg) => Ok(reg),
            _ => Err(ParseErr::BadUsage),
        }
    }

    pub(super) fn reserver_regs(&mut self, n: usize) {
        self.p.fs.prop_mut().freereg += n;
    }

    pub(super) fn free_reg(&mut self, reg: usize) -> Result<(), ParseErr> {
        if reg < self.p.pvar().nvars_stack() {
            return Ok(());
        }

        self.p.fs.prop_mut().freereg -= 1;
        if self.p.fs.prop().freereg == reg {
            Ok(())
        } else {
            Err(ParseErr::BadUsage)
        }
    }

    pub(super) fn exp_free(&mut self, exp: &Expr) -> Result<(), ParseErr> {
        match exp.value {
            ExprValue::Nonreloc(reg) => self.free_reg(reg),
            _ => Ok(()),
        }
    }

    pub(super) fn exps_free(&mut self, e1: &Expr, e2: &Expr) -> Result<(), ParseErr> {
        log::debug!("exps free, e1: {}; e2: {}", e1.value, e2.value);

        match e1.value {
            ExprValue::Nonreloc(r1) => match e2.value {
                ExprValue::Nonreloc(r2) => {
                    if r1 < r2 {
                        self.free_reg(r2)?;
                        self.free_reg(r1)?;
                    } else {
                        self.free_reg(r1)?;
                        self.free_reg(r2)?;
                    }
                }
                _ => self.free_reg(r1)?,
            },
            _ => match e2.value {
                ExprValue::Nonreloc(r2) => self.free_reg(r2)?,
                _ => {}
            },
        };

        Ok(())
    }
}
