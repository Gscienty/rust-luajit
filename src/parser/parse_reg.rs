use crate::code::codelimit;

use super::{BinOpr, Expr, ExprValue, ParseErr, Parser};

pub(super) struct ParseReg<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseReg<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
    }

    pub(super) fn str_tok(&mut self, _value: &str) -> Result<Expr, ParseErr> {
        // TODO

        Ok(Expr::k(17))
    }

    pub(super) fn float_tok(&mut self, _value: f64) -> Result<Expr, ParseErr> {
        // TODO

        Ok(Expr::k(17))
    }

    pub(super) fn int_tok(&mut self, _value: i64) -> Result<Expr, ParseErr> {
        // TODO

        Ok(Expr::k(17))
    }

    pub(super) fn infix(&mut self, op: BinOpr, exp: Expr) -> Result<Expr, ParseErr> {
        // TODO

        match op {
            BinOpr::AND => Ok(Expr::todo()),
            BinOpr::OR => Ok(Expr::todo()),
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
            BinOpr::EQ | BinOpr::NE => Ok(Expr::todo()),
            BinOpr::LT | BinOpr::LE | BinOpr::GT | BinOpr::GE => Ok(Expr::todo()),
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
                let exp = self.str_tok(&value)?;
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
                    let exp = self.float_tok(value)?;
                    self.discharge_toreg(exp, reg)
                }
            }
            ExprValue::Integer(value) => {
                if 0 <= value && value as u32 <= codelimit::MAX_SBX {
                    self.p.parse_code().emit_loadint(reg, value);
                    Ok(Expr::nonreloc(reg))
                } else {
                    let exp = self.int_tok(value)?;
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

    pub(super) fn exp_free(&mut self, exp: Expr) -> Result<(), ParseErr> {
        match exp.value {
            ExprValue::Nonreloc(reg) => self.p.free_reg(reg),
            _ => Ok(()),
        }
    }

    pub(super) fn exp_toreg(&mut self, exp: Expr, reg: usize) -> Result<Expr, ParseErr> {
        let _exp = self.discharge_toreg(exp, reg)?;
        // TODO
        Ok(Expr::nonreloc(reg))
    }

    pub(super) fn exp_tonextreg(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        // TODO

        self.exp_free(exp.clone())?;
        self.p.reserver_regs(1);
        self.exp_toreg(exp, self.p.freereg - 1)
    }

    pub(super) fn exp_toanyreg(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        // TODO
        self.exp_tonextreg(exp)
    }

    pub(super) fn locreg(&mut self, exp: Expr) -> Result<usize, ParseErr> {
        match self.exp_toanyreg(exp)?.value {
            ExprValue::Nonreloc(reg) => Ok(reg),
            _ => Err(ParseErr::BadUsage),
        }
    }
}
