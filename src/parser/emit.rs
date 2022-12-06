use crate::code::InterCode;

use super::{ParseErr, Parser};

pub(super) struct Emiter<'s> {
    p: &'s mut Parser,
}

impl<'s> Emiter<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
    }

    fn prevemit(&mut self, code: InterCode) -> Result<usize, ParseErr> {
        log::debug!("prev emit code: {}", code);

        if let Some(pfs) = self.p.fs.prop().prev.clone() {
            pfs.prop_mut().proto.prop_mut().codes.push(code);

            Ok(pfs.prop().proto.prop().codes.len() - 1)
        } else {
            Err(ParseErr::BadUsage)
        }
    }

    fn emit(&mut self, code: InterCode) -> usize {
        log::debug!("emit code: {}", code);

        self.p.fs.prop_mut().proto.prop_mut().codes.push(code);
        self.p.fs.prop().proto.prop().codes.len() - 1
    }

    pub(super) fn pop(&mut self) {
        self.p.fs.prop_mut().proto.prop_mut().codes.pop();
    }

    pub(super) fn emit_vararg(&mut self) -> usize {
        self.emit(InterCode::VARARG(0, 1))
    }

    pub(super) fn emit_varargprep(&mut self, nparams: usize) -> usize {
        self.emit(InterCode::VARARGPREP(nparams as u8))
    }

    pub(super) fn emit_loadnil(&mut self, freg: usize, n: usize) -> usize {
        self.emit(InterCode::LOADNIL(freg as u8, n as u8))
    }

    pub(super) fn emit_loadbool(&mut self, reg: usize, value: bool) -> usize {
        if value {
            self.emit(InterCode::LOADTRUE(reg as u8))
        } else {
            self.emit(InterCode::LOADFALSE(reg as u8))
        }
    }

    pub(super) fn emit_lfalseskip(&mut self, reg: usize) -> usize {
        self.emit(InterCode::LFALSESKIP(reg as u8))
    }

    pub(super) fn emit_loadint(&mut self, reg: usize, value: i64) -> usize {
        self.emit(InterCode::LOADINT(reg as u8, value as u32))
    }

    pub(super) fn emit_loadfloat(&mut self, reg: usize, value: f64) -> usize {
        self.emit(InterCode::LOADFLOAT(reg as u8, value as u32))
    }

    pub(super) fn emit_loadk(&mut self, reg: usize, kidx: usize) -> usize {
        self.emit(InterCode::LOADK(reg as u8, kidx as u32))
    }

    pub(super) fn emit_move(&mut self, tgt: usize, src: usize) -> usize {
        self.emit(InterCode::MOVE(tgt as u8, src as u8))
    }

    pub(super) fn emit_concat(&mut self, reg: usize) -> usize {
        self.emit(InterCode::CONCAT(reg as u8, 2))
    }

    pub(super) fn emit_addi(&mut self, reg: usize, imm: u8) -> usize {
        self.emit(InterCode::ADDI(255, reg as u8, imm))
    }

    pub(super) fn emit_addk(&mut self, reg: usize, k: usize) -> usize {
        self.emit(InterCode::ADDK(255, reg as u8, k as u8))
    }

    pub(super) fn emit_add(&mut self, r1: usize, r2: usize) -> usize {
        self.emit(InterCode::ADD(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_subk(&mut self, reg: usize, k: usize) -> usize {
        self.emit(InterCode::SUBK(255, reg as u8, k as u8))
    }

    pub(super) fn emit_sub(&mut self, r1: usize, r2: usize) -> usize {
        self.emit(InterCode::SUB(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_mulk(&mut self, reg: usize, k: usize) -> usize {
        self.emit(InterCode::MULK(255, reg as u8, k as u8))
    }

    pub(super) fn emit_mul(&mut self, r1: usize, r2: usize) -> usize {
        self.emit(InterCode::MUL(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_divk(&mut self, reg: usize, k: usize) -> usize {
        self.emit(InterCode::DIVK(255, reg as u8, k as u8))
    }

    pub(super) fn emit_div(&mut self, r1: usize, r2: usize) -> usize {
        self.emit(InterCode::DIV(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_idivk(&mut self, reg: usize, k: usize) -> usize {
        self.emit(InterCode::IDIVK(255, reg as u8, k as u8))
    }

    pub(super) fn emit_idiv(&mut self, r1: usize, r2: usize) -> usize {
        self.emit(InterCode::IDIV(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_modk(&mut self, reg: usize, k: usize) -> usize {
        self.emit(InterCode::MODK(255, reg as u8, k as u8))
    }

    pub(super) fn emit_mod(&mut self, r1: usize, r2: usize) -> usize {
        self.emit(InterCode::MOD(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_powk(&mut self, reg: usize, k: usize) -> usize {
        self.emit(InterCode::POWK(255, reg as u8, k as u8))
    }

    pub(super) fn emit_pow(&mut self, r1: usize, r2: usize) -> usize {
        self.emit(InterCode::POW(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_shli(&mut self, reg: usize, imm: u8) -> usize {
        self.emit(InterCode::SHLI(255, reg as u8, imm))
    }

    pub(super) fn emit_shl(&mut self, r1: usize, r2: usize) -> usize {
        self.emit(InterCode::SHL(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_shri(&mut self, reg: usize, imm: u8) -> usize {
        self.emit(InterCode::SHRI(255, reg as u8, imm))
    }

    pub(super) fn emit_shr(&mut self, r1: usize, r2: usize) -> usize {
        self.emit(InterCode::SHR(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_band(&mut self, r1: usize, r2: usize) -> usize {
        self.emit(InterCode::BAND(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_bor(&mut self, r1: usize, r2: usize) -> usize {
        self.emit(InterCode::BOR(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_bxor(&mut self, r1: usize, r2: usize) -> usize {
        self.emit(InterCode::BXOR(255, r1 as u8, r2 as u8))
    }

    pub(super) fn emit_bandk(&mut self, r1: usize, k: usize) -> usize {
        self.emit(InterCode::BANDK(255, r1 as u8, k as u8))
    }

    pub(super) fn emit_bork(&mut self, r1: usize, k: usize) -> usize {
        self.emit(InterCode::BORK(255, r1 as u8, k as u8))
    }

    pub(super) fn emit_bxork(&mut self, r1: usize, k: usize) -> usize {
        self.emit(InterCode::BXORK(255, r1 as u8, k as u8))
    }

    pub(super) fn emit_eqi(&mut self, reg: usize, sb: u32, k: bool) -> usize {
        self.emit(InterCode::EQI(reg as u8, sb, k))
    }

    pub(super) fn emit_eq(&mut self, ra: usize, rb: usize, k: bool) -> usize {
        self.emit(InterCode::EQ(ra as u8, rb as u8, k))
    }

    pub(super) fn emit_eqk(&mut self, ra: usize, rb: usize, k: bool) -> usize {
        self.emit(InterCode::EQK(ra as u8, rb as u8, k))
    }

    pub(super) fn emit_jmp(&mut self) -> usize {
        self.emit(InterCode::JMP(None))
    }

    pub(super) fn emit_lt(&mut self, ra: usize, rb: usize, k: bool) -> usize {
        self.emit(InterCode::LT(ra as u8, rb as u8, k))
    }

    pub(super) fn emit_le(&mut self, ra: usize, rb: usize, k: bool) -> usize {
        self.emit(InterCode::LE(ra as u8, rb as u8, k))
    }

    pub(super) fn emit_lti(&mut self, ra: usize, rb: u32, k: bool) -> usize {
        self.emit(InterCode::LTI(ra as u8, rb, k))
    }

    pub(super) fn emit_lei(&mut self, ra: usize, rb: u32, k: bool) -> usize {
        self.emit(InterCode::LEI(ra as u8, rb, k))
    }

    pub(super) fn emit_gti(&mut self, ra: usize, rb: u32, k: bool) -> usize {
        self.emit(InterCode::GTI(ra as u8, rb, k))
    }

    pub(super) fn emit_gei(&mut self, ra: usize, rb: u32, k: bool) -> usize {
        self.emit(InterCode::GEI(ra as u8, rb, k))
    }

    pub(super) fn emit_testset(&mut self, rb: usize, k: bool) -> usize {
        self.emit(InterCode::TESTSET(255, rb as u8, k))
    }

    pub(super) fn emit_not(&mut self, rb: usize) -> usize {
        self.emit(InterCode::NOT(255, rb as u8))
    }

    pub(super) fn emit_unm(&mut self, rb: usize) -> usize {
        self.emit(InterCode::UNM(255, rb as u8))
    }

    pub(super) fn emit_bnot(&mut self, rb: usize) -> usize {
        self.emit(InterCode::BNOT(255, rb as u8))
    }

    pub(super) fn emit_len(&mut self, rb: usize) -> usize {
        self.emit(InterCode::LEN(255, rb as u8))
    }

    pub(super) fn emit_getupval(&mut self, rb: usize) -> usize {
        self.emit(InterCode::GETUPVAL(255, rb as u8))
    }

    pub(super) fn emit_gettabup(&mut self, rb: usize, rc: usize) -> usize {
        self.emit(InterCode::GETTABUP(255, rb as u8, rc as u8))
    }

    pub(super) fn emit_geti(&mut self, rb: usize, rc: i64) -> usize {
        self.emit(InterCode::GETI(255, rb as u8, rc as u8))
    }

    pub(super) fn emit_getfield(&mut self, rb: usize, rc: usize) -> usize {
        self.emit(InterCode::GETFIELD(255, rb as u8, rc as u8))
    }

    pub(super) fn emit_gettable(&mut self, rb: usize, rc: usize) -> usize {
        self.emit(InterCode::GETTABLE(255, rb as u8, rc as u8))
    }

    pub(super) fn emit_tbc(&mut self, ra: usize) -> usize {
        self.emit(InterCode::TBC(ra as u8))
    }

    pub(super) fn emit_close(&mut self, ra: usize) -> usize {
        self.emit(InterCode::CLOSE(ra as u8))
    }

    pub(super) fn emit_forprep(&mut self, ra: usize) -> usize {
        self.emit(InterCode::FORPREP(ra as u8, 0))
    }

    pub(super) fn emit_tforprep(&mut self, ra: usize) -> usize {
        self.emit(InterCode::TFORPREP(ra as u8, 0))
    }

    pub(super) fn emit_tforcall(&mut self, ra: usize, rc: usize) -> usize {
        self.emit(InterCode::TFORCALL(ra as u8, rc as u8))
    }

    pub(super) fn emit_forloop(&mut self, ra: usize) -> usize {
        self.emit(InterCode::FORLOOP(ra as u8, 0))
    }

    pub(super) fn emit_tforloop(&mut self, ra: usize) -> usize {
        self.emit(InterCode::TFORLOOP(ra as u8, 0))
    }

    pub(super) fn emit_setupval(&mut self, ra: usize, rb: usize) -> usize {
        self.emit(InterCode::SETUPVAL(ra as u8, rb as u8))
    }

    pub(super) fn emit_settabup(&mut self, ra: usize, rb: usize, rc: usize, k: bool) -> usize {
        self.emit(InterCode::SETTABUP(ra as u8, rb as u8, rc as u8, k))
    }

    pub(super) fn emit_settable(&mut self, ra: usize, rb: usize, rc: usize, k: bool) -> usize {
        self.emit(InterCode::SETTABLE(ra as u8, rb as u8, rc as u8, k))
    }

    pub(super) fn emit_seti(&mut self, ra: usize, rb: i64, rc: usize, k: bool) -> usize {
        self.emit(InterCode::SETI(ra as u8, rb as u8, rc as u8, k))
    }

    pub(super) fn emit_setfield(&mut self, ra: usize, rb: usize, rc: usize, k: bool) -> usize {
        self.emit(InterCode::SETFIELD(ra as u8, rb as u8, rc as u8, k))
    }

    pub(super) fn emit_return(&mut self, ra: usize, rb: usize) -> usize {
        self.emit(InterCode::RETURN(ra as u8, rb as u8))
    }

    pub(super) fn emit_return1(&mut self, ra: usize) -> usize {
        self.emit(InterCode::RETURN1(ra as u8))
    }

    pub(super) fn emit_return0(&mut self) -> usize {
        self.emit(InterCode::RETURN0)
    }

    pub(super) fn emit_call(&mut self, ra: usize, rb: usize, rc: usize) -> usize {
        self.emit(InterCode::CALL(ra as u8, rb as u8, rc as u8))
    }

    pub(super) fn emit_newtable(&mut self) -> usize {
        self.emit(InterCode::NEWTABLE(255, 255, 255, false))
    }

    pub(super) fn emit_setlist(&mut self, ra: usize, rb: usize, rc: usize) -> usize {
        self.emit(InterCode::SETLIST(ra as u8, rb as u8, rc as u8, false))
    }

    pub(super) fn emit_nop(&mut self) -> usize {
        self.emit(InterCode::NOP)
    }

    pub(super) fn emit_self(&mut self, ra: usize, rb: usize, rc: usize, k: bool) -> usize {
        self.emit(InterCode::SELF(ra as u8, rb as u8, rc as u8, k))
    }

    pub(super) fn emit_closure(&mut self, rb: usize) -> Result<usize, ParseErr> {
        self.prevemit(InterCode::CLOSURE(255, rb as u32))
    }

    pub(super) fn set_ra(&mut self, pc: usize, ra: usize) {
        let ra = ra as u8;

        self.modify_code(pc, |c| {
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
                InterCode::VARARG(_, rb) => InterCode::VARARG(ra, rb),
                InterCode::CLOSURE(_, rb) => InterCode::CLOSURE(ra, rb),
                _ => *c,
            };

            log::debug!("set ra: {}", *c);
        });
    }

    pub(super) fn set_rb(&mut self, pc: usize, rb: u8) {
        self.modify_code(pc, |c| {
            *c = match *c {
                InterCode::CONCAT(ra, _) => InterCode::CONCAT(ra, rb),
                _ => *c,
            };

            log::debug!("set rb: {}", *c);
        })
    }

    pub(super) fn set_bx(&mut self, pc: usize, bx: i32) {
        self.modify_code(pc, |c| {
            *c = match *c {
                InterCode::FORPREP(ra, _) => InterCode::FORPREP(ra, bx),
                InterCode::TFORPREP(ra, _) => InterCode::TFORPREP(ra, bx),
                InterCode::FORLOOP(ra, _) => InterCode::FORLOOP(ra, bx),
                InterCode::TFORLOOP(ra, _) => InterCode::TFORLOOP(ra, bx),
                _ => *c,
            }
        })
    }

    pub(super) fn set_rc(&mut self, pc: usize, rc: u8) {
        self.modify_code(pc, |c| {
            *c = match *c {
                InterCode::VARARG(ra, _) => InterCode::VARARG(ra, rc),
                InterCode::CALL(ra, rb, _) => InterCode::CALL(ra, rb, rc),
                _ => *c,
            };

            log::debug!("set rc: {}", *c);
        })
    }

    pub(super) fn set_sj(&mut self, pc: usize, sj: i64) {
        self.modify_code(pc, |c| {
            *c = match *c {
                InterCode::JMP(_) => InterCode::JMP(Some(sj as i32)),
                _ => *c,
            };

            log::debug!("set sj: {}", *c);
        })
    }

    pub(crate) fn negate_cond(&mut self, pc: usize) {
        let (pc, _) = self.get_ctrljump(pc);
        self.modify_code(pc, |c| {
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
            };

            log::debug!("negate cond: {}", *c);
        });
    }

    pub(super) fn modify_code<F>(&mut self, pc: usize, f: F)
    where
        F: FnOnce(&mut InterCode),
    {
        if let Some(code) = self.p.fs.prop_mut().proto.prop_mut().codes.get_mut(pc) {
            f(code)
        }
    }

    pub(super) fn pc(&self) -> usize {
        self.p.fs.prop().proto.prop().codes.len()
    }

    pub(super) fn mark_pc(&mut self) -> usize {
        let pc = self.pc();
        self.p.last_target = pc;

        pc
    }

    pub(super) fn get_code(&self, pc: usize) -> Option<InterCode> {
        self.p
            .fs
            .prop()
            .proto
            .prop()
            .codes
            .get(pc)
            .and_then(|c| Some(*c))
    }

    pub(super) fn get_jump(&self, pc: usize) -> Option<usize> {
        match self.get_code(pc) {
            Some(InterCode::JMP(Some(offset))) => Some(pc + 1 + offset as usize),
            _ => None,
        }
    }

    pub(super) fn get_ctrljump(&self, pc: usize) -> (usize, Option<InterCode>) {
        match self.get_code(pc) {
            Some(ins) if ins.test_mode() => (pc - 1, self.get_code(pc - 1)),
            Some(ins) => (pc, Some(ins)),
            _ => (pc, None),
        }
    }
}
