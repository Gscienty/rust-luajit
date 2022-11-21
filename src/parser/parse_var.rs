use crate::{
    code::{codelimit, Code, OpCode},
    lexer::Token,
    object::{ExprDesc, ExprKind, VarKind},
};

use super::{FuncState, ParseErr, Parser};

pub(super) struct ParseVar<'s, 't> {
    fs: &'s mut FuncState,
    p: &'t mut Parser,
}

impl<'s, 't> ParseVar<'s, 't> {
    pub(super) fn new(fs: &'s mut FuncState, p: &'t mut Parser) -> Self {
        ParseVar { fs, p }
    }

    pub(super) fn reglevel(&self, nvar: usize) -> usize {
        for nvar in (0..nvar).rev() {
            if let Some(v) = self.p.getloc(self.fs, nvar) {
                if !matches!(v.kind, VarKind::CTC) {
                    return v.r_idx + 1;
                }
            }
        }

        0
    }

    pub(super) fn nvarstack(&self) -> usize {
        self.reglevel(self.fs.prop().nactvar)
    }

    pub(super) fn freereg(&mut self, reg: u32) -> Result<(), ParseErr> {
        if reg >= self.nvarstack() as u32 {
            self.fs.prop_mut().freereg -= 1;

            if reg != self.fs.prop().freereg {
                return Err(ParseErr::BadUsage);
            }
        }
        Ok(())
    }

    pub(super) fn freeexp(&mut self, expr: &ExprDesc) -> Result<(), ParseErr> {
        if matches!(expr.kind, ExprKind::NONRELOC) {
            self.freereg(expr.info as u32)?;
        }
        Ok(())
    }

    pub(super) fn reserve_regs(&mut self, nreg: u32) {
        self.fs.prop_mut().freereg += nreg;
    }

    pub(super) fn to_nextreg(&mut self, expr: &mut ExprDesc) -> Result<(), ParseErr> {
        self.p.parse_code(self.fs).discharge_vars(expr)?;
        self.freeexp(expr)?;
        self.reserve_regs(1);
        let freereg = self.fs.prop().freereg;
        self.p.parse_code(self.fs).expr_toreg(expr, freereg - 1)?;

        Ok(())
    }

    pub(super) fn adjust_localvars(&mut self, nvars: u32) {
        let mut reg_level = self.reglevel(self.fs.prop().nactvar);

        for _ in 0..nvars {
            let vidx = self.fs.prop().nactvar;
            self.fs.prop_mut().nactvar += 1;

            if let Some(var) = self.p.getloc_mut(self.fs, vidx) {
                var.r_idx = reg_level;
                reg_level += 1;
                var.p_idx = self.fs.register_locvar(&var.name);
            }
        }
    }

    pub(super) fn closure(&mut self, expr: &mut ExprDesc) -> Result<(), ParseErr> {
        expr.f = codelimit::NO_JMP;
        expr.t = codelimit::NO_JMP;
        expr.kind = ExprKind::RELOC;

        let code = Code::new_abx(OpCode::CLOSURE, 0, self.fs.prop().nparams as u32 - 1)?;
        expr.info = self.fs.emit(code);

        self.to_nextreg(expr)
    }

    pub(super) fn single_aux(
        &mut self,
        name: &str,
        var_expr: &mut ExprDesc,
        base: u32,
    ) -> Result<(), ParseErr> {
        Ok(())
    }

    pub(super) fn single(&mut self, var_expr: &mut ExprDesc) -> Result<(), ParseErr> {
        let name = self.p.parse_lex().name()?;
        self.single_aux(&name, var_expr, 1)?;

        // TODO

        Ok(())
    }

    pub(super) fn indexed(
        &mut self,
        table_expr: &mut ExprDesc,
        key: &mut ExprDesc,
    ) -> Result<(), ParseErr> {
        // TODO
        Ok(())
    }
}
