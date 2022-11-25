use std::ops::Deref;

use crate::{
    code::{codelimit, Code, OpCode},
    object::{ExprDesc, ExprKind, RefValue, Value},
};

use super::{FuncState, ParseErr, Parser};

pub(super) struct ParseCode<'s, 't> {
    fs: &'s mut FuncState,
    p: &'t mut Parser,
}

impl<'s, 't> ParseCode<'s, 't> {
    pub(super) fn new(fs: &'s mut FuncState, p: &'t mut Parser) -> Self {
        Self { fs, p }
    }

    pub(super) fn mark_label(&mut self) -> usize {
        self.fs.prop_mut().last_target = self.fs.prop().pc;

        self.fs.prop().pc
    }

    pub(super) fn patch_testreg(&mut self, node: usize, reg: u32) -> Result<bool, ParseErr> {
        let node = self.jmp_control(node)?;
        let code = self.fs.code(node, |code| code.clone());

        match code {
            Some(code) => {
                if matches!(code.op, OpCode::TESTSET) {
                    return Ok(false);
                }
                if reg != codelimit::NO_REG && reg != code.rb {
                    self.fs.code_mut(node, |code| code.ra = reg);
                } else {
                    let new_code = Code::new_abc(OpCode::TEST, code.rb, 0, 0, code.k)?;
                    self.fs.code_mut(node, |code| *code = new_code);
                }
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    pub(super) fn patch_listaux(
        &mut self,
        list: usize,
        vtgt: usize,
        reg: u32,
        dtgt: usize,
    ) -> Result<(), ParseErr> {
        let mut list = list;
        while list != codelimit::NO_JMP {
            let next = self.get_jmp(list);

            if self.patch_testreg(list, reg)? {
                self.patch_jmp(list, vtgt)?
            } else {
                self.patch_jmp(list, dtgt)?
            }

            match next {
                Some(next) => list = next,
                _ => break,
            }
        }

        Ok(())
    }

    pub(super) fn patch_tohere(&mut self, list: usize) -> Result<(), ParseErr> {
        let pc = self.mark_label();

        self.patch_listaux(list, pc, codelimit::NO_REG, pc)
    }

    pub(super) fn get_jmp(&self, pc: usize) -> Option<usize> {
        match self.fs.code(pc, |code| code.j as usize) {
            Some(offset) if offset != codelimit::NO_JMP => Some(pc + 1 + offset),
            _ => None,
        }
    }

    pub(super) fn patch_jmp(&mut self, pc: usize, dest: usize) -> Result<(), ParseErr> {
        let offset = dest - (pc + 1);

        // TODO check

        self.fs.code_mut(pc, |code| code.j = offset as u32);
        Ok(())
    }

    pub(super) fn patch_forjmp(
        &mut self,
        pc: usize,
        dest: usize,
        back: bool,
    ) -> Result<(), ParseErr> {
        let offset = if back {
            (pc + 1) - dest
        } else {
            dest - (pc + 1)
        };

        self.fs.code_mut(pc, |code| code.rb = offset as u32);

        Ok(())
    }

    pub(super) fn jmp_concat(&mut self, list1: &mut usize, list2: usize) -> Result<(), ParseErr> {
        if list2 == codelimit::NO_JMP {
            return Ok(());
        } else if *list1 == codelimit::NO_JMP {
            *list1 = list2;
        } else {
            let mut list = *list1;
            loop {
                match self.get_jmp(list) {
                    Some(next) => list = next,
                    _ => break,
                }
            }

            self.patch_jmp(list, list2)?;
        }

        Ok(())
    }

    pub(super) fn discharge_vars(&mut self, expr: &mut ExprDesc) -> Result<(), ParseErr> {
        match expr.kind {
            ExprKind::CONST => {
                let val = self.p.getloc_abs(expr.info);
                if let Some(val) = val {
                    match val.k.get().deref() {
                        Value::Integer(val) => {
                            expr.kind = ExprKind::KINT;
                            expr.val = Some(RefValue::from(*val));
                        }
                        Value::Number(val) => {
                            expr.kind = ExprKind::KFLT;
                            expr.val = Some(RefValue::from(*val));
                        }
                        Value::String(val) => {
                            expr.kind = ExprKind::KSTR;
                            expr.val = Some(RefValue::from(val.as_str()));
                        }
                        Value::Boolean(false) => expr.kind = ExprKind::FALSE,
                        Value::Boolean(true) => expr.kind = ExprKind::TRUE,
                        Value::Nil => expr.kind = ExprKind::NIL,
                        _ => return Err(ParseErr::BadUsage),
                    }
                }
            }
            ExprKind::LOCAL => {
                expr.info = expr.r_idx;
                expr.kind = ExprKind::NONRELOC;
            }
            ExprKind::UPVAL => {
                let code = Code::new_abc(OpCode::GETUPVAL, 0, expr.info as u32, 0, false)?;

                expr.info = self.fs.emit(code);
                expr.kind = ExprKind::RELOC;
            }
            ExprKind::INDEXUP => {
                let code = Code::new_abc(OpCode::GETTABUP, 0, expr.ind_tab, expr.ind_idx, false)?;

                expr.info = self.fs.emit(code);
                expr.kind = ExprKind::RELOC;
            }
            ExprKind::INDEXI => {
                self.p.parse_var(self.fs).freereg(expr.ind_tab)?;

                let code = Code::new_abc(OpCode::GETTABUP, 0, expr.ind_tab, expr.ind_idx, false)?;

                expr.info = self.fs.emit(code);
                expr.kind = ExprKind::RELOC;
            }
            ExprKind::INDEXSTR => {
                self.p.parse_var(self.fs).freereg(expr.ind_tab)?;

                let code = Code::new_abc(OpCode::GETTABUP, 0, expr.ind_tab, expr.ind_idx, false)?;

                expr.info = self.fs.emit(code);
                expr.kind = ExprKind::RELOC;
            }
            ExprKind::INDEXED => {
                self.p.parse_var(self.fs).freereg(expr.ind_tab)?;

                let code = Code::new_abc(OpCode::GETTABUP, 0, expr.ind_tab, expr.ind_idx, false)?;

                expr.info = self.fs.emit(code);
                expr.kind = ExprKind::RELOC;
            }
            ExprKind::VARARG => {
                expr.kind = ExprKind::RELOC;
                self.fs.code_mut(expr.info, |code| code.rc = 2);
            }
            ExprKind::CALL => {
                expr.kind = ExprKind::NONRELOC;
                let nextinfo = self.fs.code(expr.info, |code| code.ra as usize);
                expr.info = nextinfo.ok_or(ParseErr::BadUsage)?;
            }
            _ => {}
        }
        Ok(())
    }

    pub(super) fn emit_nil(&mut self, from: u32, n: u32) -> Result<(), ParseErr> {
        let prevpc = self.fs.prop().pc - 1;

        if let Some(prevcode) = self.fs.code(prevpc, |code| code.clone()) {
            if matches!(prevcode.op, OpCode::LOADNIL) {
                let pfrom = prevcode.ra;
                let pl = pfrom + prevcode.rb;

                let mut from = from;
                let mut l = from + n - 1;

                if (pfrom <= from && from <= pl + 1) || (from <= pfrom && pfrom <= l + 1) {
                    if pfrom < from {
                        from = pfrom;
                    }
                    if pl > l {
                        l = pl;
                    }

                    self.fs.code_mut(prevpc, |code| {
                        code.ra = from;
                        code.rb = l - from;
                    });
                    return Ok(());
                }
            }
        }

        let code = Code::new_abc(OpCode::LOADNIL, from, n - 1, 0, false)?;
        self.fs.emit(code);
        Ok(())
    }

    pub(super) fn emit_loadconstant(&mut self, reg: u32, k: u32) -> Result<usize, ParseErr> {
        if k <= codelimit::MAX_BX {
            let code = Code::new_abx(OpCode::LOADK, reg, k)?;

            Ok(self.fs.emit(code))
        } else {
            let code = Code::new_abx(OpCode::LOADKX, reg, 0)?;
            let pc = self.fs.emit(code);
            self.fs.emit(Code::new_ax(OpCode::EXTRARG, k)?);
            Ok(pc)
        }
    }

    pub(super) fn emit_number(&mut self, reg: u32, value: RefValue) -> Result<(), ParseErr> {
        match value.get().deref() {
            Value::Number(val)
                if *val as i64 as f64 == *val
                    && (-(codelimit::MAX_OFFSET_SBX as i64) <= *val as i64
                        && *val as i64
                            <= (codelimit::MAX_SBX - codelimit::MAX_OFFSET_SBX) as i64) =>
            {
                let val = *val as i64;

                let code = Code::new_asbx(OpCode::LOADF, reg, val as u32)?;
                self.fs.emit(code);
                Ok(())
            }
            Value::Integer(val)
                if (-(codelimit::MAX_OFFSET_SBX as i64) <= *val
                    && *val <= (codelimit::MAX_SBX - codelimit::MAX_OFFSET_SBX) as i64) =>
            {
                let code = Code::new_asbx(OpCode::LOADI, reg, *val as u32)?;
                self.fs.emit(code);
                Ok(())
            }
            Value::Number(val) => {
                let val = self.p.parse_gtab().set_val(RefValue::from(*val));

                self.emit_loadconstant(reg, val as u32)?;
                Ok(())
            }
            Value::Integer(val) => {
                let val = self.p.parse_gtab().set_val(RefValue::from(*val));

                self.emit_loadconstant(reg, val as u32)?;
                Ok(())
            }
            _ => Err(ParseErr::BadUsage),
        }
    }

    pub(super) fn discharge_reg(&mut self, expr: &mut ExprDesc, reg: u32) -> Result<(), ParseErr> {
        self.discharge_vars(expr)?;

        match expr.kind {
            ExprKind::NIL => self.emit_nil(reg, 1)?,
            ExprKind::FALSE => {
                let code = Code::new_abc(OpCode::LOADFALSE, reg, 0, 0, false)?;

                self.fs.emit(code);
            }
            ExprKind::TRUE => {
                let code = Code::new_abc(OpCode::LOADTRUE, reg, 0, 0, false)?;

                self.fs.emit(code);
            }
            ExprKind::KSTR => {
                let val = expr.val.clone().ok_or(ParseErr::BadUsage)?;

                expr.info = self.p.parse_gtab().set_val(val);
                expr.kind = ExprKind::K;

                self.emit_loadconstant(reg, expr.info as u32)?;
            }
            ExprKind::K => {
                self.emit_loadconstant(reg, expr.info as u32)?;
            }
            ExprKind::KFLT | ExprKind::KINT => {
                let val = expr.val.clone().ok_or(ParseErr::BadUsage)?;

                self.emit_number(reg, val)?;
            }
            ExprKind::RELOC => {
                self.fs.code_mut(expr.info, |code| code.ra = reg);
            }
            ExprKind::NONRELOC => {
                if reg != expr.info as u32 {
                    let code = Code::new_abc(OpCode::MOVE, reg, expr.info as u32, 0, false)?;

                    self.fs.emit(code);
                }
            }
            ExprKind::JMP => {}
            _ => return Err(ParseErr::BadUsage),
        }

        expr.info = reg as usize;
        expr.kind = ExprKind::NONRELOC;

        Ok(())
    }

    pub(super) fn discharge_anyreg(&mut self, expr: &mut ExprDesc) -> Result<(), ParseErr> {
        if !matches!(expr.kind, ExprKind::NONRELOC) {
            self.p.parse_var(self.fs).reserve_regs(1);

            let reg = self.fs.prop().freereg - 1;
            self.discharge_reg(expr, reg)?;
        }
        Ok(())
    }

    pub(super) fn jmp_control(&self, pc: usize) -> Result<usize, ParseErr> {
        if pc >= 1 && matches!(self.fs.code(pc - 1, |code| code.op.t_mode()), Some(true)) {
            Ok(pc - 1)
        } else {
            Ok(pc)
        }
    }

    pub(super) fn negate_cond(&mut self, expr: &ExprDesc) -> Result<(), ParseErr> {
        let pc = self.jmp_control(expr.info)?;
        if !matches!(
            self.fs.code(pc, |code| code.op.t_mode()
                && !matches!(code.op, OpCode::TEST | OpCode::TESTSET)),
            Some(true)
        ) {
            return Err(ParseErr::BadUsage);
        }

        self.fs.code_mut(pc, |code| code.k = !code.k);

        Ok(())
    }

    pub(super) fn jmp(&mut self) -> Result<usize, ParseErr> {
        let code = Code::new_j(OpCode::JMP, codelimit::NO_JMP as u32)?;

        Ok(self.fs.emit(code))
    }

    pub(super) fn cond_jmp(&mut self, code: Code) -> Result<usize, ParseErr> {
        self.fs.emit(code);

        self.jmp()
    }

    pub(super) fn jmp_oncond(
        &mut self,
        expr: &mut ExprDesc,
        cond: bool,
    ) -> Result<usize, ParseErr> {
        if matches!(expr.kind, ExprKind::RELOC) {
            if matches!(self.fs.code(expr.info, |code| code.op), Some(OpCode::NOT)) {
                let rb = self
                    .fs
                    .code(expr.info, |code| code.rb)
                    .ok_or(ParseErr::BadUsage)?;

                self.fs.pop_code();
                return self.cond_jmp(Code::new_abc(OpCode::TEST, rb, 0, 0, !cond)?);
            }
        }

        self.discharge_anyreg(expr)?;
        self.p.parse_var(self.fs).freeexp(expr)?;

        let code = Code::new_abc(
            OpCode::TESTSET,
            codelimit::NO_REG,
            expr.info as u32,
            0,
            cond,
        )?;
        self.cond_jmp(code)
    }

    pub(super) fn jmp_iffalse(&mut self, expr: &mut ExprDesc) -> Result<(), ParseErr> {
        self.discharge_vars(expr)?;

        let pc = match expr.kind {
            ExprKind::JMP => expr.info,
            ExprKind::NIL | ExprKind::FALSE => codelimit::NO_JMP,
            _ => self.jmp_oncond(expr, true)?,
        };

        self.jmp_concat(&mut expr.f, pc)?;
        self.patch_tohere(expr.t)?;
        expr.t = codelimit::NO_JMP;

        Ok(())
    }

    pub(super) fn jmp_iftrue(&mut self, expr: &mut ExprDesc) -> Result<(), ParseErr> {
        self.discharge_vars(expr)?;

        let pc = match expr.kind {
            ExprKind::JMP => {
                self.negate_cond(expr)?;
                expr.info
            }
            ExprKind::K | ExprKind::KFLT | ExprKind::KINT | ExprKind::KSTR | ExprKind::TRUE => {
                codelimit::NO_JMP
            }
            _ => self.jmp_oncond(expr, false)?,
        };

        self.jmp_concat(&mut expr.t, pc)?;
        self.patch_tohere(expr.f)?;
        expr.f = codelimit::NO_JMP;

        Ok(())
    }

    pub(super) fn need_value(&self, list: usize) -> bool {
        let mut list = list;
        while list != codelimit::NO_JMP {
            if matches!(self.fs.code(list, |code| code.op), Some(OpCode::TESTSET)) {
                return true;
            }

            list = if let Some(list) = self.get_jmp(list) {
                list
            } else {
                return false;
            }
        }

        return false;
    }

    pub(super) fn loadbool(&mut self, op: OpCode, ra: u32) -> Result<usize, ParseErr> {
        self.mark_label();
        let code = Code::new_abc(op, ra, 0, 0, false)?;

        Ok(self.fs.emit(code))
    }

    pub(super) fn expr_toreg(&mut self, expr: &mut ExprDesc, reg: u32) -> Result<(), ParseErr> {
        self.discharge_reg(expr, reg)?;

        if matches!(expr.kind, ExprKind::JMP) {
            self.jmp_concat(&mut expr.t, expr.info)?;
        }

        if expr.has_jmp() {
            let mut pf = codelimit::NO_JMP;
            let mut pt = codelimit::NO_JMP;

            if self.need_value(expr.t) || self.need_value(expr.f) {
                let fj = match expr.kind {
                    ExprKind::JMP => codelimit::NO_JMP,
                    _ => self.jmp()?,
                };

                pf = self.loadbool(OpCode::LFALSESKIP, reg)?;
                pt = self.loadbool(OpCode::LOADTRUE, reg)?;

                self.patch_tohere(fj)?;
            }

            let final_pc = self.mark_label();

            self.patch_listaux(expr.f, final_pc, reg, pf)?;
            self.patch_listaux(expr.t, final_pc, reg, pt)?;
        }
        expr.t = codelimit::NO_JMP;
        expr.f = codelimit::NO_JMP;
        expr.info = reg as usize;
        expr.kind = ExprKind::NONRELOC;

        Ok(())
    }
}
