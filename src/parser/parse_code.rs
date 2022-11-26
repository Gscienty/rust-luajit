use crate::code::{codelimit, InterCode};

use super::{BinOpr, Expr, ExprValue, ParseErr, Parser};

pub(super) struct ParseCode<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseCode<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
    }

    pub(super) fn emit_vararg(&mut self) -> usize {
        log::debug!("emit VARARG 0, 1");

        self.p.emit(InterCode::VARARG(0, 1))
    }

    pub(super) fn emit_loadnil(&mut self, freg: usize, n: usize) -> usize {
        log::debug!("emit LOADNIL {}, {}", freg, n);

        self.p.emit(InterCode::LOADNIL(freg as u8, n as u8))
    }

    pub(super) fn emit_loadbool(&mut self, reg: usize, value: bool) -> usize {
        if value {
            log::debug!("emit LOADTRUE {}", reg);

            self.p.emit(InterCode::LOADTRUE(reg as u8))
        } else {
            log::debug!("emit LOADFALSE {}", reg);

            self.p.emit(InterCode::LOADFALSE(reg as u8))
        }
    }

    pub(super) fn emit_loadint(&mut self, reg: usize, value: i64) -> usize {
        log::debug!("emit LOADINT {}, {}", reg, value);

        self.p.emit(InterCode::LOADINT(reg as u8, value as u32))
    }

    pub(super) fn emit_loadfloat(&mut self, reg: usize, value: f64) -> usize {
        log::debug!("emit LOADFLOAT {}, {}", reg, value);

        self.p.emit(InterCode::LOADFLOAT(reg as u8, value as u32))
    }

    pub(super) fn emit_loadk(&mut self, reg: usize, kidx: usize) -> usize {
        log::debug!("emit LOADK {}, {}", reg, kidx);

        self.p.emit(InterCode::LOADK(reg as u8, kidx as u32))
    }

    pub(super) fn emit_move(&mut self, tgt: usize, src: usize) -> usize {
        log::debug!("emit MOVE {}, {}", tgt, src);

        self.p.emit(InterCode::MOVE(tgt as u8, src as u8))
    }

    pub(super) fn emit_concat(&mut self, reg: usize) -> usize {
        log::debug!("emit CONCAT {}, 2", reg);

        self.p.emit(InterCode::CONCAT(reg as u8, 2))
    }

    pub(super) fn emit_addi(&mut self, reg: usize, imm: u8) -> usize {
        log::debug!("emit ADDI 255, {}", reg);

        self.p.emit(InterCode::ADDI(255, reg as u8, imm))
    }

    pub(super) fn emit_addk(&mut self, reg: usize, k: usize) -> usize {
        log::debug!("emit ADDK 255, {}, {}", reg, k);

        self.p.emit(InterCode::ADDK(255, reg as u8, k as u8))
    }

    pub(super) fn emit_add(&mut self, r1: usize, r2: usize) -> usize {
        log::debug!("emit ADD 255, {}, {}", r1, r2);

        self.p.emit(InterCode::ADD(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_subk(&mut self, reg: usize, k: usize) -> usize {
        log::debug!("emit SUBK 255, {}, {}", reg, k);

        self.p.emit(InterCode::SUBK(255, reg as u8, k as u8))
    }

    pub(super) fn emit_sub(&mut self, r1: usize, r2: usize) -> usize {
        log::debug!("emit SUB 255, {}, {}", r1, r2);

        self.p.emit(InterCode::SUB(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_mulk(&mut self, reg: usize, k: usize) -> usize {
        log::debug!("emit MULK 255, {}, {}", reg, k);

        self.p.emit(InterCode::MULK(255, reg as u8, k as u8))
    }

    pub(super) fn emit_mul(&mut self, r1: usize, r2: usize) -> usize {
        log::debug!("emit MUL 255, {}, {}", r1, r2);

        self.p.emit(InterCode::MUL(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_divk(&mut self, reg: usize, k: usize) -> usize {
        log::debug!("emit DIVK 255, {}, {}", reg, k);

        self.p.emit(InterCode::DIVK(255, reg as u8, k as u8))
    }

    pub(super) fn emit_div(&mut self, r1: usize, r2: usize) -> usize {
        log::debug!("emit DIV 255, {}, {}", r1, r2);

        self.p.emit(InterCode::DIV(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_idivk(&mut self, reg: usize, k: usize) -> usize {
        log::debug!("emit IDIVK 255, {}, {}", reg, k);

        self.p.emit(InterCode::IDIVK(255, reg as u8, k as u8))
    }

    pub(super) fn emit_idiv(&mut self, r1: usize, r2: usize) -> usize {
        log::debug!("emit IDIV 255, {}, {}", r1, r2);

        self.p.emit(InterCode::IDIV(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_modk(&mut self, reg: usize, k: usize) -> usize {
        log::debug!("emit MODK 255, {}, {}", reg, k);

        self.p.emit(InterCode::MODK(255, reg as u8, k as u8))
    }

    pub(super) fn emit_mod(&mut self, r1: usize, r2: usize) -> usize {
        log::debug!("emit MOD 255, {}, {}", r1, r2);

        self.p.emit(InterCode::MOD(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_powk(&mut self, reg: usize, k: usize) -> usize {
        log::debug!("emit POWK 255, {}, {}", reg, k);

        self.p.emit(InterCode::POWK(255, reg as u8, k as u8))
    }

    pub(super) fn emit_pow(&mut self, r1: usize, r2: usize) -> usize {
        log::debug!("emit POW 255, {}, {}", r1, r2);

        self.p.emit(InterCode::POW(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_shli(&mut self, reg: usize, imm: u8) -> usize {
        log::debug!("emit SHLI 255, {}, {}", reg, imm);

        self.p.emit(InterCode::SHLI(255, reg as u8, imm))
    }

    pub(super) fn emit_shl(&mut self, r1: usize, r2: usize) -> usize {
        log::debug!("emit SHL 255, {}, {}", r1, r2);

        self.p.emit(InterCode::SHL(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_shri(&mut self, reg: usize, imm: u8) -> usize {
        log::debug!("emit SHRI 255, {}, {}", reg, imm);

        self.p.emit(InterCode::SHRI(255, reg as u8, imm))
    }

    pub(super) fn emit_shr(&mut self, r1: usize, r2: usize) -> usize {
        log::debug!("emit SHR 255, {}, {}", r1, r2);

        self.p.emit(InterCode::SHR(255, r1 as u8, r2 as u8))
    }

    pub(super) fn set_ra(&mut self, pc: usize, ra: usize) {
        let ra = ra as u8;

        self.p.modify_code(pc, |c| {
            *c = match *c {
                InterCode::CONCAT(_, rb) => InterCode::CONCAT(ra, rb),
                InterCode::ADDI(_, rb, rc) => InterCode::ADDI(ra, rb, rc),
                InterCode::ADDK(_, rb, rc) => InterCode::ADDK(ra, rb, rc),
                InterCode::ADD(_, rb, rc) => InterCode::ADD(ra, rb, rc),
                InterCode::MULK(_, rb, rc) => InterCode::MULK(ra, rb, rc),
                InterCode::MUL(_, rb, rc) => InterCode::MUL(ra, rb, rc),
                InterCode::DIVK(_, rb, rc) => InterCode::DIVK(ra, rb, rc),
                InterCode::DIV(_, rb, rc) => InterCode::DIV(ra, rb, rc),
                InterCode::IDIVK(_, rb, rc) => InterCode::IDIVK(ra, rb, rc),
                InterCode::IDIV(_, rb, rc) => InterCode::IDIV(ra, rb, rc),
                InterCode::SUBK(_, rb, rc) => InterCode::SUBK(ra, rb, rc),
                InterCode::SUB(_, rb, rc) => InterCode::SUB(ra, rb, rc),
                InterCode::MODK(_, rb, rc) => InterCode::MODK(ra, rb, rc),
                InterCode::MOD(_, rb, rc) => InterCode::MOD(ra, rb, rc),
                InterCode::POWK(_, rb, rc) => InterCode::POWK(ra, rb, rc),
                InterCode::POW(_, rb, rc) => InterCode::POW(ra, rb, rc),
                InterCode::SHLI(_, rb, rc) => InterCode::SHLI(ra, rb, rc),
                InterCode::SHL(_, rb, rc) => InterCode::SHL(ra, rb, rc),
                InterCode::SHRI(_, rb, rc) => InterCode::SHRI(ra, rb, rc),
                InterCode::SHR(_, rb, rc) => InterCode::SHR(ra, rb, rc),
                _ => *c,
            }
        });
    }

    pub(super) fn set_rb(&mut self, pc: usize, rb: u8) {
        self.p.modify_code(pc, |c| {
            *c = match *c {
                InterCode::CONCAT(ra, _) => InterCode::CONCAT(ra, rb),
                _ => *c,
            }
        })
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
        log::debug!("parse arith_allreg op: {}", op);

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
            _ => return Err(ParseErr::BadUsage),
        })
    }

    fn arith_k(&mut self, op: BinOpr, reg: usize, k: usize) -> Result<usize, ParseErr> {
        log::debug!("parse arith_k op: {}", op);

        Ok(match op {
            BinOpr::ADD => self.emit_addk(reg, k),
            BinOpr::SUB => self.emit_subk(reg, k),
            BinOpr::MUL => self.emit_mulk(reg, k),
            BinOpr::DIV => self.emit_divk(reg, k),
            BinOpr::IDIV => self.emit_idivk(reg, k),
            BinOpr::MOD => self.emit_modk(reg, k),
            BinOpr::POW => self.emit_powk(reg, k),
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

    pub(super) fn posfix(&mut self, op: BinOpr, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        log::debug!("parse posfix, op: {}", op);
        match op {
            BinOpr::AND => {
                // TODO

                Ok(e2)
            }
            BinOpr::OR => {
                // TODO

                Ok(e2)
            }
            BinOpr::EQ => {
                // TODO

                Ok(e2)
            }
            BinOpr::NE => {
                // TODO

                Ok(e2)
            }
            BinOpr::CONCAT => {
                log::debug!("parse posfix concat");
                let e2 = self.p.parse_reg().exp_tonextreg(e2)?;

                let pc = self.p.get_codelen() - 1;
                let code = self.p.get_code(pc).and_then(|c| Some(*c));

                match code {
                    Some(InterCode::CONCAT(pra, prb)) => match e1.value {
                        ExprValue::Nonreloc(reg) if reg + 1 == pra as usize => {
                            self.set_ra(pc, reg);
                            self.set_rb(pc, prb + 1);

                            self.p.parse_reg().exp_free(e2)?;

                            Ok(e1)
                        }
                        _ => Err(ParseErr::BadUsage),
                    },
                    _ => match e1.value {
                        ExprValue::Nonreloc(reg) => {
                            self.emit_concat(reg);
                            self.p.parse_reg().exp_free(e2)?;

                            Ok(Expr::nonreloc(reg))
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
            | BinOpr::SHR => self.arith(op, e1, e2),

            _ => Err(ParseErr::BadUsage),
        }
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
        );

        match e2.value {
            ExprValue::Integer(value)
                if allowimm && 0 <= value && value as u32 <= codelimit::MAX_C =>
            {
                let e1 = self.p.parse_reg().exp_toanyreg(e1)?;
                let reg = self.p.parse_reg().locreg(e1)?;
                let pc = self.arith_imm(op, reg, value as u8)?;
                self.p.free_reg(reg)?;

                Ok(Expr::reloc(pc))
            }
            ExprValue::Integer(value) => {
                let e2 = self.p.parse_reg().int_tok(value)?;
                self.arith_inreg(op, e1, e2)
            }
            ExprValue::Float(value) => {
                let e2 = self.p.parse_reg().float_tok(value)?;
                self.arith_inreg(op, e1, e2)
            }
            ExprValue::String(value) => {
                let e2 = self.p.parse_reg().str_tok(&value)?;
                self.arith_inreg(op, e1, e2)
            }
            ExprValue::K(k) if allowk => {
                let e1 = self.p.parse_reg().exp_toanyreg(e1)?;
                let reg = self.p.parse_reg().locreg(e1)?;
                let pc = self.arith_k(op, reg, k)?;
                self.p.free_reg(reg)?;

                Ok(Expr::reloc(pc))
            }
            _ => Err(ParseErr::BadUsage),
        }
    }

    fn arith(&mut self, op: BinOpr, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        log::debug!("parse arith op: {}", op);

        let swapable = matches!(op, BinOpr::ADD | BinOpr::MUL);
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

                let e1 = self.p.parse_reg().exp_toanyreg(e1)?;
                self.arith(op, e1, e2)
            } else if e1.numeric() && !e2.numeric() && swapable {
                log::debug!("parse arith op: {}, all not inreg, e2 not immediate", op);

                let e2 = self.p.parse_reg().exp_toanyreg(e2)?;
                self.arith(op, e1, e2)
            } else {
                log::debug!("parse arith op: {}, all not inreg, all not immediate", op);

                let e1 = self.p.parse_reg().exp_toanyreg(e1)?;

                self.arith(op, e1, e2)
            }
        } else if e1.inreg() && !e2.inreg() {
            log::debug!("parse arith op: {}, e2 not inreg", op);

            let e1 = self.p.parse_reg().exp_toanyreg(e1)?;

            self.arith_inreg(op, e1, e2)
        } else if !e1.inreg() && e2.inreg() && swapable {
            log::debug!("parse arith op: {}, e1 not inreg", op);

            let e2 = self.p.parse_reg().exp_toanyreg(e2)?;

            self.arith_inreg(op, e2, e1)
        } else {
            log::debug!("parse arith op: {}, all inreg", op);

            let e1 = self.p.parse_reg().exp_toanyreg(e1)?;
            let e2 = self.p.parse_reg().exp_toanyreg(e2)?;

            log::debug!("{} {}", e1.value.clone(), self.p.freereg - 1);
            let r1 = self.p.parse_reg().locreg(e1)?;
            let r2 = self.p.parse_reg().locreg(e2)?;

            let pc = self.arith_allreg(op, r1, r2)?;
            self.p.free_reg(r2)?;
            self.p.free_reg(r1)?;

            Ok(Expr::reloc(pc))
        }
    }

    pub(crate) fn negate_cond(&mut self, _pc: usize) {}

    fn get_jump(&self, pc: usize) -> Option<usize> {
        if let Some(InterCode::JMP(offset)) = self.p.get_code(pc) {
            Some(pc + 1 + *offset as usize)
        } else {
            None
        }
    }

    fn fix_jump(&mut self, pc: usize, dpc: usize) -> Result<(), ParseErr> {
        let offset = dpc - (pc + 1);
        self.p
            .modify_code(pc, |c| *c = InterCode::JMP(offset as u32));

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
}
