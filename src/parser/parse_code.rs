use crate::code::{codelimit, InterCode};

use super::{BinOpr, Expr, ExprValue, ParseErr, Parser, UnOpr};

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

    pub(super) fn emit_lfalseskip(&mut self, reg: usize) -> usize {
        log::debug!("emit LFALSESKIP {}", reg);

        self.p.emit(InterCode::LFALSESKIP(reg as u8))
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

    pub(super) fn emit_band(&mut self, r1: usize, r2: usize) -> usize {
        log::debug!("emit BAND 255, {}, {}", r1, r2);

        self.p.emit(InterCode::BAND(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_bor(&mut self, r1: usize, r2: usize) -> usize {
        log::debug!("emit BOR 255, {}, {}", r1, r2);

        self.p.emit(InterCode::BOR(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_bxor(&mut self, r1: usize, r2: usize) -> usize {
        log::debug!("emit BXOR 255, {}, {}", r1, r2);

        self.p.emit(InterCode::BXOR(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_bandk(&mut self, r1: usize, k: usize) -> usize {
        log::debug!("emit BANDK 255, {}, {}", r1, k);

        self.p.emit(InterCode::BANDK(255, r1 as u8, k as u8))
    }

    pub(super) fn emit_bork(&mut self, r1: usize, k: usize) -> usize {
        log::debug!("emit BORK 255, {}, {}", r1, k);

        self.p.emit(InterCode::BORK(255, r1 as u8, k as u8))
    }

    pub(super) fn emit_bxork(&mut self, r1: usize, k: usize) -> usize {
        log::debug!("emit BXORK 255, {}, {}", r1, k);

        self.p.emit(InterCode::BXORK(255, r1 as u8, k as u8))
    }

    pub(super) fn emit_eqi(&mut self, reg: usize, sb: u32, k: bool) -> usize {
        log::debug!("emit EQI {}, {}, {}", reg, sb, k);

        self.p.emit(InterCode::EQI(reg as u8, sb, k))
    }

    pub(super) fn emit_eq(&mut self, ra: usize, rb: usize, k: bool) -> usize {
        log::debug!("emit EQ {}, {}, {}", ra, rb, k);

        self.p.emit(InterCode::EQ(ra as u8, rb as u8, k))
    }

    pub(super) fn emit_eqk(&mut self, ra: usize, rb: usize, k: bool) -> usize {
        log::debug!("emit EQK {}, {}, {}", ra, rb, k);

        self.p.emit(InterCode::EQK(ra as u8, rb as u8, k))
    }

    pub(super) fn emit_jmp(&mut self) -> usize {
        log::debug!("emit JMP NONE");

        self.p.emit(InterCode::JMP(None))
    }

    pub(super) fn emit_lt(&mut self, ra: usize, rb: usize, k: bool) -> usize {
        log::debug!("emit LT {}, {}, {}", ra, rb, k);

        self.p.emit(InterCode::LT(ra as u8, rb as u8, k))
    }

    pub(super) fn emit_le(&mut self, ra: usize, rb: usize, k: bool) -> usize {
        log::debug!("emit LE {}, {}, {}", ra, rb, k);

        self.p.emit(InterCode::LE(ra as u8, rb as u8, k))
    }

    pub(super) fn emit_lti(&mut self, ra: usize, rb: u32, k: bool) -> usize {
        log::debug!("emit LTI {}, {}, {}", ra, rb, k);

        self.p.emit(InterCode::LTI(ra as u8, rb, k))
    }

    pub(super) fn emit_lei(&mut self, ra: usize, rb: u32, k: bool) -> usize {
        log::debug!("emit LEI {}, {}, {}", ra, rb, k);

        self.p.emit(InterCode::LEI(ra as u8, rb, k))
    }

    pub(super) fn emit_gti(&mut self, ra: usize, rb: u32, k: bool) -> usize {
        log::debug!("emit GTI {}, {}, {}", ra, rb, k);

        self.p.emit(InterCode::GTI(ra as u8, rb, k))
    }

    pub(super) fn emit_gei(&mut self, ra: usize, rb: u32, k: bool) -> usize {
        log::debug!("emit GEI {}, {}, {}", ra, rb, k);

        self.p.emit(InterCode::GEI(ra as u8, rb, k))
    }

    pub(super) fn emit_testset(&mut self, rb: usize, k: bool) -> usize {
        log::debug!("emit TESTSET 255, {}, {}", rb, k);

        self.p.emit(InterCode::TESTSET(255, rb as u8, k))
    }

    pub(super) fn emit_not(&mut self, rb: usize) -> usize {
        log::debug!("emit NOT 255, {}", rb);

        self.p.emit(InterCode::NOT(255, rb as u8))
    }

    pub(super) fn emit_unm(&mut self, rb: usize) -> usize {
        log::debug!("emit UNM 255, {}", rb);

        self.p.emit(InterCode::UNM(255, rb as u8))
    }

    pub(super) fn emit_bnot(&mut self, rb: usize) -> usize {
        log::debug!("emit BNOT 255, {}", rb);

        self.p.emit(InterCode::BNOT(255, rb as u8))
    }

    pub(super) fn emit_len(&mut self, rb: usize) -> usize {
        log::debug!("emit LEN 255, {}", rb);

        self.p.emit(InterCode::LEN(255, rb as u8))
    }

    pub(super) fn emit_getupval(&mut self, rb: usize) -> usize {
        log::debug!("emit GETUPVAL 255, {}", rb);

        self.p.emit(InterCode::GETUPVAL(255, rb as u8))
    }

    pub(super) fn emit_gettabup(&mut self, rb: usize, rc: usize) -> usize {
        log::debug!("emit GETTABUP 255, {}, {}", rb, rc);

        self.p.emit(InterCode::GETTABUP(255, rb as u8, rc as u8))
    }

    pub(super) fn emit_geti(&mut self, rb: usize, rc: usize) -> usize {
        log::debug!("emit GETI 255, {}, {}", rb, rc);

        self.p.emit(InterCode::GETI(255, rb as u8, rc as u8))
    }

    pub(super) fn emit_getfield(&mut self, rb: usize, rc: usize) -> usize {
        log::debug!("emit GETFIELD 255, {}, {}", rb, rc);

        self.p.emit(InterCode::GETFIELD(255, rb as u8, rc as u8))
    }

    pub(super) fn emit_gettable(&mut self, rb: usize, rc: usize) -> usize {
        log::debug!("emit GETTABLE 255, {}, {}", rb, rc);

        self.p.emit(InterCode::GETTABLE(255, rb as u8, rc as u8))
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
                InterCode::BAND(_, rb, rc) => InterCode::BAND(ra, rb, rc),
                InterCode::BANDK(_, rb, rc) => InterCode::BANDK(ra, rb, rc),
                InterCode::BOR(_, rb, rc) => InterCode::BOR(ra, rb, rc),
                InterCode::BORK(_, rb, rc) => InterCode::BORK(ra, rb, rc),
                InterCode::BXOR(_, rb, rc) => InterCode::BXOR(ra, rb, rc),
                InterCode::BXORK(_, rb, rc) => InterCode::BXORK(ra, rb, rc),
                InterCode::TESTSET(_, rb, rc) => InterCode::TESTSET(ra, rb, rc),
                InterCode::NOT(_, rb) => InterCode::NOT(ra, rb),
                InterCode::UNM(_, rb) => InterCode::UNM(ra, rb),
                InterCode::BNOT(_, rb) => InterCode::BNOT(ra, rb),
                InterCode::LEN(_, rb) => InterCode::LEN(ra, rb),
                InterCode::GETUPVAL(_, rb) => InterCode::GETUPVAL(ra, rb),
                InterCode::GETTABUP(_, rb, rc) => InterCode::GETTABUP(ra, rb, rc),
                InterCode::GETI(_, rb, rc) => InterCode::GETI(ra, rb, rc),
                InterCode::GETFIELD(_, rb, rc) => InterCode::GETFIELD(ra, rb, rc),
                InterCode::GETTABLE(_, rb, rc) => InterCode::GETTABLE(ra, rb, rc),
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

    pub(super) fn set_rc(&mut self, pc: usize, rc: u8) {
        self.p.modify_code(pc, |c| {
            *c = match *c {
                InterCode::VARARG(ra, _) => InterCode::VARARG(ra, rc),
                _ => *c,
            }
        })
    }

    pub(super) fn set_sj(&mut self, pc: usize, sj: usize) {
        self.p.modify_code(pc, |c| {
            *c = match *c {
                InterCode::JMP(_) => InterCode::JMP(Some(sj as u32)),
                _ => *c,
            }
        })
    }

    pub(crate) fn negate_cond(&mut self, pc: usize) {
        let (pc, _) = self.p.get_ctrljump(pc);
        self.p.modify_code(pc, |c| {
            *c = match *c {
                InterCode::EQI(ra, rb, k) => InterCode::EQI(ra, rb, !k),
                InterCode::EQ(ra, rb, k) => InterCode::EQ(ra, rb, !k),
                InterCode::EQK(ra, rb, k) => InterCode::EQK(ra, rb, !k),
                InterCode::LT(ra, rb, k) => InterCode::LT(ra, rb, !k),
                InterCode::LE(ra, rb, k) => InterCode::LE(ra, rb, !k),
                InterCode::LTI(ra, rb, k) => InterCode::LTI(ra, rb, !k),
                InterCode::LEI(ra, rb, k) => InterCode::LEI(ra, rb, !k),
                InterCode::GTI(ra, rb, k) => InterCode::GTI(ra, rb, !k),
                InterCode::GEI(ra, rb, k) => InterCode::GEI(ra, rb, !k),
                _ => *c,
            }
        });
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
            BinOpr::BAND => self.emit_band(r1, r2),
            BinOpr::BOR => self.emit_bor(r1, r2),
            BinOpr::BXOR => self.emit_bxor(r1, r2),
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
            BinOpr::BAND => self.emit_bandk(reg, k),
            BinOpr::BOR => self.emit_bork(reg, k),
            BinOpr::BXOR => self.emit_bxork(reg, k),
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
                let e1 = self.p.parse_reg().exp_toanyreg(e1)?;
                let reg = self.p.parse_reg().locreg(e1)?;
                let pc = self.arith_imm(op, reg, value as u8)?;
                self.p.free_reg(reg)?;

                Ok(Expr::reloc(pc))
            }
            ExprValue::Integer(_) | ExprValue::Float(_) | ExprValue::String(_) => {
                let e2 = self.p.parse_reg().exp_tok(e2)?;
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

    pub(super) fn posfix(&mut self, op: BinOpr, e1: Expr, e2: Expr) -> Result<Expr, ParseErr> {
        log::debug!("parse posfix, op: {}", op);
        let e2 = self.p.parse_var().discharge_tovar(e2)?;

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
        let exp = self.p.parse_reg().exp_toanyreg(exp)?;
        let r = self.p.parse_reg().locreg(exp)?;
        self.p.free_reg(r)?;

        match op {
            UnOpr::MINUS => Ok(Expr::reloc(self.emit_unm(r))),
            UnOpr::BNOT => Ok(Expr::reloc(self.emit_bnot(r))),
            UnOpr::LEN => Ok(Expr::reloc(self.emit_len(r))),
            _ => Err(ParseErr::BadUsage),
        }
    }

    fn removevalues(&mut self, list: Option<usize>) {
        let mut list = list;
        while let Some(pc) = list {
            self.patch_testreg(pc, None);

            list = self.p.get_jump(pc);
        }
    }

    pub(super) fn prefix(&mut self, op: UnOpr, exp: Expr) -> Result<Expr, ParseErr> {
        log::debug!("parse prefix, op: {}", op);

        let exp = self.p.parse_var().discharge_tovar(exp)?;

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
                        let exp = self.p.parse_reg().discharge_toanyreg(exp)?;
                        let r = self.p.parse_reg().locreg(exp)?;
                        self.p.free_reg(r)?;

                        Ok(Expr::reloc(self.emit_not(r)))
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

    pub(super) fn jump_patchtohere(&mut self, list: Option<usize>) -> Result<(), ParseErr> {
        let here = self.p.mark_label();
        self.jump_patchlistaux(list, Some(here), None, Some(here))
    }

    pub(super) fn patch_testreg(&mut self, node: usize, reg: Option<usize>) -> bool {
        let (pc, ins) = self.p.get_ctrljump(node);
        let ins = match ins {
            Some(ins) => Some(*ins),
            _ => None,
        };

        match ins.clone() {
            Some(InterCode::TESTSET(_, rb, k)) => {
                match reg {
                    Some(reg) if reg != rb as usize => self
                        .p
                        .modify_code(pc, |c| *c = InterCode::TESTSET(reg as u8, rb, k)),

                    _ => self.p.modify_code(pc, |c| *c = InterCode::TEST(rb, k)),
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
            let next = self.p.get_jump(pc);

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
        let offset = dpc - (pc + 1);
        self.set_sj(pc, offset);

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
                if let Some(next) = self.p.get_jump(list) {
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
            ExprValue::Reloc(pc) => match self.p.get_code(pc).and_then(|c| Some(*c)) {
                Some(InterCode::NOT(_, rb)) => {
                    self.p.remove_lastcode();

                    self.emit_testset(rb as usize, !cond);
                    return Ok((self.emit_jmp(), exp));
                }
                _ => {}
            },
            _ => {}
        };

        let exp = self.p.parse_reg().discharge_toanyreg(exp)?;
        self.p.parse_reg().exp_free(exp.clone())?;
        let reg = self.p.parse_reg().locreg(exp.clone())?;

        self.emit_testset(reg, cond);
        Ok((self.emit_jmp(), exp))
    }

    pub(super) fn goiftrue(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        let mut exp = self.p.parse_var().discharge_tovar(exp)?;

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
        let mut exp = self.p.parse_var().discharge_tovar(exp)?;

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

        let e1 = self.p.parse_reg().exp_toanyreg(e1)?;
        let ra = self.p.parse_reg().locreg(e1)?;
        let k = matches!(op, BinOpr::EQ);

        match e2.value {
            ExprValue::Integer(value) if 0 <= value && value as u32 <= codelimit::MAX_SBX => {
                self.emit_eqi(ra, value as u32, k);
                self.p.free_reg(ra)?;
            }
            _ => {
                let e2 = self.p.parse_reg().exp_tokreg(e2)?;
                match e2.value {
                    ExprValue::Nonreloc(rb) => {
                        self.emit_eq(ra, rb, k);
                        if ra < rb {
                            self.p.free_reg(rb)?;
                            self.p.free_reg(ra)?;
                        } else {
                            self.p.free_reg(ra)?;
                            self.p.free_reg(rb)?;
                        }
                    }
                    ExprValue::K(rb) => {
                        self.p.free_reg(ra)?;
                        self.emit_eqk(ra, rb, k);
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
            BinOpr::GT => (BinOpr::GT, e2, e1),
            _ => return Err(ParseErr::BadUsage),
        };

        if matches!(e2.value, ExprValue::Integer(v) if 0 <= v && v as u32 <= codelimit::MAX_SBX) {
            let e1 = self.p.parse_reg().exp_toanyreg(e1)?;
            let r1 = self.p.parse_reg().locreg(e1)?;
            let imm = match e2.value {
                ExprValue::Integer(v) => v,
                _ => unreachable!(),
            };

            match op {
                BinOpr::LT => self.emit_lti(r1, imm as u32, true),
                BinOpr::LE => self.emit_lei(r1, imm as u32, true),
                _ => unreachable!(),
            };
            self.p.free_reg(r1)?;
        } else if matches!(e1.value, ExprValue::Integer(v) if 0 <= v && v as u32 <= codelimit::MAX_SBX)
        {
            let e2 = self.p.parse_reg().exp_toanyreg(e2)?;
            let r2 = self.p.parse_reg().locreg(e2)?;
            let imm = match e1.value {
                ExprValue::Integer(v) => v,
                _ => unreachable!(),
            };

            match op {
                BinOpr::LT => self.emit_gti(r2, imm as u32, true),
                BinOpr::LE => self.emit_gei(r2, imm as u32, true),
                _ => unreachable!(),
            };
            self.p.free_reg(r2)?;
        } else {
            let e1 = self.p.parse_reg().exp_toanyreg(e1)?;
            let e2 = self.p.parse_reg().exp_toanyreg(e2)?;
            let r1 = self.p.parse_reg().locreg(e1)?;
            let r2 = self.p.parse_reg().locreg(e2)?;

            match op {
                BinOpr::LT => self.emit_lt(r1, r2, true),
                BinOpr::LE => self.emit_le(r1, r2, true),
                _ => unreachable!(),
            };

            if r1 < r2 {
                self.p.free_reg(r2)?;
                self.p.free_reg(r1)?;
            } else {
                self.p.free_reg(r1)?;
                self.p.free_reg(r2)?;
            }
        }

        Ok(Expr::jmp(self.emit_jmp()))
    }
}
