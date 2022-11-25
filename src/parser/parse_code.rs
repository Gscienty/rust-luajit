use crate::code::{codelimit, IntermediateCode};

use super::{BinOpr, Expr, ExprValue, ParseErr, Parser};

pub(super) struct ParseCode<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseCode<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
    }

    pub(super) fn emit_vararg(&mut self) -> usize {
        self.p.emit(IntermediateCode::VARARG(0, 1))
    }

    pub(super) fn emit_loadnil(&mut self, freg: usize, n: usize) -> usize {
        self.p.emit(IntermediateCode::LOADNIL(freg as u8, n as u8))
    }

    pub(super) fn emit_loadbool(&mut self, reg: usize, value: bool) -> usize {
        if value {
            self.p.emit(IntermediateCode::LOADTRUE(reg as u8))
        } else {
            self.p.emit(IntermediateCode::LOADFALSE(reg as u8))
        }
    }

    pub(super) fn emit_loadint(&mut self, reg: usize, value: i64) -> usize {
        self.p
            .emit(IntermediateCode::LOADINT(reg as u8, value as u32))
    }

    pub(super) fn emit_loadfloat(&mut self, reg: usize, value: f64) -> usize {
        self.p
            .emit(IntermediateCode::LOADFLOAT(reg as u8, value as u32))
    }

    pub(super) fn emit_loadk(&mut self, reg: usize, kidx: usize) -> usize {
        self.p.emit(IntermediateCode::LOADK(reg as u8, kidx as u32))
    }

    pub(super) fn emit_move(&mut self, tgt: usize, src: usize) -> usize {
        self.p.emit(IntermediateCode::MOVE(tgt as u8, src as u8))
    }

    pub(super) fn emit_concat(&mut self, reg: usize) -> usize {
        self.p.emit(IntermediateCode::CONCAT(reg as u8, 2))
    }

    pub(super) fn emit_addi(&mut self, reg: usize, imm: u8) -> usize {
        self.p.emit(IntermediateCode::ADDI(255, reg as u8, imm))
    }

    pub(super) fn emit_addk(&mut self, reg: usize, k: usize) -> usize {
        self.p.emit(IntermediateCode::ADDK(255, reg as u8, k as u8))
    }

    pub(super) fn emit_add(&mut self, r1: usize, r2: usize) -> usize {
        self.p.emit(IntermediateCode::ADD(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_mulk(&mut self, reg: usize, k: usize) -> usize {
        self.p.emit(IntermediateCode::MULK(255, reg as u8, k as u8))
    }

    pub(super) fn emit_mul(&mut self, r1: usize, r2: usize) -> usize {
        self.p.emit(IntermediateCode::MUL(255, r1 as u8, r2 as u8))
    }

    pub(crate) fn negate_cond(&mut self, _pc: usize) {}

    pub(super) fn set_ra(&mut self, pc: usize, reg: usize) {
        self.p.modify_code(pc, |c| c.set_ra(reg as u8))
    }

    fn get_jump(&self, pc: usize) -> Option<usize> {
        if let Some(IntermediateCode::JMP(offset)) = self.p.get_code(pc) {
            Some(pc + 1 + *offset as usize)
        } else {
            None
        }
    }

    fn fix_jump(&mut self, pc: usize, dpc: usize) -> Result<(), ParseErr> {
        let offset = dpc - (pc + 1);
        self.p
            .modify_code(pc, |c| *c = IntermediateCode::JMP(offset as u32));

        Ok(())
    }

    pub(super) fn concat_jumplist(
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
            self.fix_jump(list, l2.unwrap())?;

            Ok(())
        }
    }

    fn direct_add(&mut self, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        if e1.is_immediate() && e2.is_immediate() {
            match e1.value {
                ExprValue::Integer(v1) => match e2.value {
                    ExprValue::Integer(v2) => Ok(Expr::from(v1 + v2)),
                    ExprValue::Float(v2) => Ok(Expr::from(v1 as f64 + v2)),
                    _ => Err(ParseErr::BadUsage),
                },
                ExprValue::Float(v1) => match e2.value {
                    ExprValue::Integer(v2) => Ok(Expr::from(v1 + v2 as f64)),
                    ExprValue::Float(v2) => Ok(Expr::from(v1 + v2)),
                    _ => Err(ParseErr::BadUsage),
                },
                _ => Err(ParseErr::BadUsage),
            }
        } else if !e1.is_immediate() && e2.is_immediate() {
            let e1 = self.p.parse_reg().exp_toanyreg(e1)?;
            self.inreg_add(e1, e2)
        } else if e1.is_immediate() && !e2.is_immediate() {
            let e2 = self.p.parse_reg().exp_toanyreg(e2)?;
            self.inreg_add(e2, e1)
        } else {
            let e1 = self.p.parse_reg().exp_toanyreg(e1)?;
            self.inreg_add(e1, e2)
        }
    }

    fn direct_mul(&mut self, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        if e1.is_immediate() && e2.is_immediate() {
            match e1.value {
                ExprValue::Integer(v1) => match e2.value {
                    ExprValue::Integer(v2) => Ok(Expr::from(v1 * v2)),
                    ExprValue::Float(v2) => Ok(Expr::from(v1 as f64 * v2)),
                    _ => Err(ParseErr::BadUsage),
                },
                ExprValue::Float(v1) => match e2.value {
                    ExprValue::Integer(v2) => Ok(Expr::from(v1 * v2 as f64)),
                    ExprValue::Float(v2) => Ok(Expr::from(v1 * v2)),
                    _ => Err(ParseErr::BadUsage),
                },
                _ => Err(ParseErr::BadUsage),
            }
        } else if !e1.is_immediate() && e2.is_immediate() {
            let e1 = self.p.parse_reg().exp_toanyreg(e1)?;
            self.inreg_mul(e1, e2)
        } else if e1.is_immediate() && !e2.is_immediate() {
            let e2 = self.p.parse_reg().exp_toanyreg(e2)?;
            self.inreg_mul(e2, e1)
        } else {
            let e1 = self.p.parse_reg().exp_toanyreg(e1)?;
            self.inreg_mul(e1, e2)
        }
    }

    fn inreg_add(&mut self, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        // TODO metadata overwrite

        match e2.value {
            ExprValue::Integer(value) if 0 <= value && value as u32 <= codelimit::MAX_C => {
                let reg = self.p.parse_reg().locreg(e1)?;
                let pc = self.emit_addi(reg, value as u8);
                self.p.free_reg(reg)?;

                Ok(Expr::reloc(pc))
            }
            ExprValue::Integer(value) => {
                let e2 = self.p.parse_reg().int_tok(value)?;
                self.inreg_add(e1, e2)
            }
            ExprValue::Float(value) => {
                let e2 = self.p.parse_reg().float_tok(value)?;
                self.inreg_add(e1, e2)
            }
            ExprValue::String(value) => {
                let e2 = self.p.parse_reg().str_tok(&value)?;
                self.inreg_add(e1, e2)
            }
            ExprValue::K(k) => {
                let reg = self.p.parse_reg().locreg(e1)?;
                let pc = self.emit_addk(reg, k);
                self.p.free_reg(reg)?;

                Ok(Expr::reloc(pc))
            }
            _ => Err(ParseErr::BadUsage),
        }
    }

    fn inreg_mul(&mut self, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        // TODO metadata overwrite

        match e2.value {
            ExprValue::Integer(value) => {
                let e2 = self.p.parse_reg().int_tok(value)?;
                self.inreg_mul(e1, e2)
            }
            ExprValue::Float(value) => {
                let e2 = self.p.parse_reg().float_tok(value)?;
                self.inreg_mul(e1, e2)
            }
            ExprValue::String(value) => {
                let e2 = self.p.parse_reg().str_tok(&value)?;
                self.inreg_mul(e1, e2)
            }
            ExprValue::K(k) => {
                let reg = self.p.parse_reg().locreg(e1)?;
                let pc = self.emit_mulk(reg, k);
                self.p.free_reg(reg)?;

                Ok(Expr::reloc(pc))
            }
            _ => Err(ParseErr::BadUsage),
        }
    }

    fn posfix_add(&mut self, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        // case 1: imm + imm
        // case 2: imm + k
        // case 3: imm + reloc
        // case 4: imm + nonreloc
        // case 5: k + k
        // case 6: k + reloc
        // case 7: k + nonreloc
        // case 8: reloc + reloc
        // case 9: reloc + nonreloc
        // case 10: nonreloc + nonreloc

        if e1.result_inreg() && e2.result_inreg() {
            let e1 = self.p.parse_reg().exp_toanyreg(e1)?;
            let e2 = self.p.parse_reg().exp_toanyreg(e2)?;

            let r1 = self.p.parse_reg().locreg(e1)?;
            let r2 = self.p.parse_reg().locreg(e2)?;

            let pc = self.emit_add(r1, r2);
            self.p.free_reg(r2)?;
            self.p.free_reg(r1)?;

            Ok(Expr::reloc(pc))
        } else if e1.result_inreg() && !e2.result_inreg() {
            let e1 = self.p.parse_reg().exp_toanyreg(e1)?;

            self.inreg_add(e1, e2)
        } else if !e1.result_inreg() && e2.result_inreg() {
            let e2 = self.p.parse_reg().exp_toanyreg(e2)?;

            self.inreg_add(e2, e1)
        } else {
            self.direct_add(e1, e2)
        }
    }

    fn posfix_mul(&mut self, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        // case 1: imm * imm
        // case 2: imm * k
        // case 3: imm * reloc
        // case 4: imm * nonreloc
        // case 5: k * k
        // case 6: k * reloc
        // case 7: k * nonreloc
        // case 8: reloc * reloc
        // case 9: reloc * nonreloc
        // case 10: nonreloc * nonreloc

        if e1.result_inreg() && e2.result_inreg() {
            let e1 = self.p.parse_reg().exp_toanyreg(e1)?;
            let e2 = self.p.parse_reg().exp_toanyreg(e2)?;

            let r1 = self.p.parse_reg().locreg(e1)?;
            let r2 = self.p.parse_reg().locreg(e2)?;

            let pc = self.emit_mul(r1, r2);
            self.p.free_reg(r2)?;
            self.p.free_reg(r1)?;

            Ok(Expr::reloc(pc))
        } else if e1.result_inreg() && !e2.result_inreg() {
            let e1 = self.p.parse_reg().exp_toanyreg(e1)?;

            self.inreg_mul(e1, e2)
        } else if !e1.result_inreg() && e2.result_inreg() {
            let e2 = self.p.parse_reg().exp_toanyreg(e2)?;

            self.inreg_mul(e2, e1)
        } else {
            self.direct_add(e1, e2)
        }
    }

    pub(super) fn posfix(&mut self, op: BinOpr, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        match op {
            BinOpr::AND => {
                // TODO

                Ok(e2)
            }
            BinOpr::OR => {
                // TODO

                Ok(e2)
            }
            BinOpr::CONCAT => {
                let e2 = self.p.parse_reg().exp_tonextreg(e2)?;

                // TODO optim

                match &e1.value {
                    ExprValue::Nonreloc(reg) => {
                        self.emit_concat(*reg);
                        self.p.parse_reg().exp_free(e2)?;

                        Ok(Expr::nonreloc(*reg))
                    }
                    _ => Err(ParseErr::BadUsage),
                }
            }
            BinOpr::ADD => self.posfix_add(e1, e2),
            BinOpr::MUL => self.posfix_mul(e1, e2),

            _ => Err(ParseErr::BadUsage),
        }
    }
}
