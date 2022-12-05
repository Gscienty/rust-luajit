use std::ops::Deref;

use crate::{
    code::{codelimit, InterCode},
    object::{ConstantValue, Expr, ExprValue, LocVar, RefValue, Upval, Value, Var, VarKind},
};

use super::{func::FuncState, ParseErr, Parser};

pub(super) struct ParseVar<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseVar<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
    }

    fn search_upval(&self, fs: &FuncState, name: &str) -> Option<usize> {
        let nups = fs.prop().nups;
        for vidx in 0..nups {
            match fs.prop().proto.prop().upvars.get(vidx) {
                Some(upval) if upval.name.eq(name) => return Some(vidx),
                _ => {}
            }
        }

        None
    }

    fn search_var_toexp(&self, fs: &FuncState, vidx: usize, loc: Var) -> Expr {
        if matches!(loc.kind, VarKind::CTC) {
            Expr::econst(fs.prop().first_local + vidx)
        } else {
            Expr::local(vidx, loc.r_idx)
        }
    }

    fn search_var(&self, fs: &FuncState, name: &str) -> Option<Expr> {
        let nactvar = fs.prop().nactvar;
        for vidx in (0..nactvar).rev() {
            let loc = self.getloc_aux(fs, vidx);

            match loc {
                Some(loc) if loc.name.eq(name) => {
                    return Some(self.search_var_toexp(fs, vidx, loc))
                }
                _ => {}
            }
        }
        None
    }

    fn singlevar_loctoexp(&self, fs: &FuncState, exp: Expr, base: bool) -> Result<Expr, ParseErr> {
        match exp.value {
            ExprValue::Local(vidx, _) if !base => {
                let mut bl = fs.prop().block.clone();
                while bl.prop().nactvar > vidx {
                    let pbl = bl.prop().prev.clone();

                    if let Some(pbl) = pbl {
                        bl = pbl;
                    } else {
                        return Err(ParseErr::BadUsage);
                    }
                }

                bl.prop_mut().upval = true;
                bl.prop_mut().needclose = true;
            }
            _ => {}
        }

        Ok(exp)
    }

    fn singlevar_prevfs(&self, fs: &FuncState, name: &str) -> Result<Expr, ParseErr> {
        if let Some(fs) = &fs.prop().prev {
            let exp = self.singlevar_aux(fs, name, false)?;
            let nups = fs.prop().nups;
            let mut upvar = Upval::new(name);

            match exp.value {
                ExprValue::Local(vidx, ridx) => {
                    upvar.instack = true;
                    upvar.idx = ridx;
                    upvar.kind = match self.getloc_aux(fs, vidx) {
                        Some(var) => var.kind,
                        _ => return Err(ParseErr::BadUsage),
                    };
                }
                ExprValue::Upval(uidx) => {
                    upvar.instack = false;
                    upvar.idx = uidx;
                    upvar.kind = match fs.prop().proto.prop().upvars.get(uidx) {
                        Some(upvar) => upvar.kind,
                        _ => return Err(ParseErr::BadUsage),
                    };
                }
                _ => return Ok(exp),
            };

            fs.prop().proto.prop_mut().upvars.push(upvar);
            fs.prop_mut().nups = nups + 1;

            Ok(Expr::upval(nups))
        } else {
            Ok(Expr::void())
        }
    }

    fn singlevar_aux(&self, fs: &FuncState, name: &str, base: bool) -> Result<Expr, ParseErr> {
        let locexp = self.search_var(fs, name);
        if let Some(locexp) = locexp {
            return self.singlevar_loctoexp(fs, locexp, base);
        }

        let uidx = self.search_upval(fs, name);
        if let Some(uidx) = uidx {
            return Ok(Expr::upval(uidx));
        } else {
            return self.singlevar_prevfs(fs, name);
        }
    }

    pub(super) fn single_var(&mut self, name: &str) -> Result<Expr, ParseErr> {
        log::debug!("single var {}", name);

        let exp = self.singlevar_aux(&self.p.fs, name, true)?;
        if matches!(exp.value, ExprValue::Void) {
            let exp = self.singlevar_aux(&self.p.fs, Parser::ENV, true)?;
            let exp = self.exp_toanyregup(exp)?;

            return self.indexed(exp, Expr::from(name));
        }

        Ok(exp)
    }

    fn refvalue_tovar(&self, value: &RefValue) -> Result<Expr, ParseErr> {
        match value.get().deref() {
            Value::Nil => Ok(Expr::nil()),
            Value::Integer(v) => Ok(Expr::from(*v)),
            Value::Number(v) => Ok(Expr::from(*v)),
            Value::Boolean(v) => Ok(Expr::from(*v)),
            Value::String(v) => Ok(Expr::from(v.as_str())),
            _ => Err(ParseErr::BadUsage),
        }
    }

    pub(super) fn discharge_tovar(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        log::debug!("discharge tovar {}", exp.value);

        let tj = exp.true_jumpto;
        let fj = exp.false_jumpto;

        match exp.value {
            ExprValue::Const(idx) => match self.p.actvar.get(idx) {
                Some(var) => self.refvalue_tovar(&var.val),
                _ => Err(ParseErr::BadUsage),
            },
            ExprValue::Local(_, reg) => Ok(Expr::nonreloc(reg)),
            ExprValue::Upval(reg) => Ok(Expr::reloc(self.p.emiter().emit_getupval(reg))),
            ExprValue::IndexUp(t, idx) => Ok(Expr::reloc(self.p.emiter().emit_gettabup(t, idx))),
            ExprValue::IndexI(t, idx) => {
                self.free_reg(t)?;
                Ok(Expr::reloc(self.p.emiter().emit_geti(t, idx)))
            }
            ExprValue::IndexStr(t, idx) => {
                self.free_reg(t)?;
                Ok(Expr::reloc(self.p.emiter().emit_getfield(t, idx)))
            }
            ExprValue::Indexed(t, idx) => {
                if t < idx {
                    self.free_reg(idx)?;
                    self.free_reg(t)?;
                } else {
                    self.free_reg(t)?;
                    self.free_reg(idx)?;
                }

                Ok(Expr::reloc(self.p.emiter().emit_getfield(t, idx)))
            }
            ExprValue::VarArg(pc) => {
                self.p.emiter().set_rc(pc, 2);
                Ok(Expr::reloc(pc))
            }
            ExprValue::Call(pc) => {
                self.p.emiter().set_rc(pc, 2);

                self.p
                    .emiter()
                    .get_code(pc)
                    .ok_or(ParseErr::BadUsage)
                    .and_then(|c| match c {
                        InterCode::CALL(ra, ..) => Ok(Expr::nonreloc(ra as usize)),
                        _ => Err(ParseErr::BadUsage),
                    })
            }
            _ => Ok(exp),
        }
        .and_then(|exp| Ok(exp.tj(tj).fj(fj)))
    }

    pub(super) fn removevars(&mut self, tolevel: usize) {
        let pc = self.p.emiter().pc();

        while self.p.fs.prop().nactvar > tolevel {
            self.p.fs.prop_mut().nactvar -= 1;

            let pidx = match self.p.actvar.pop() {
                Some(v) if !matches!(v.kind, VarKind::CTC) => v.p_idx,
                _ => continue,
            };

            if let Some(var) = self.p.fs.prop().proto.prop_mut().locvars.get_mut(pidx) {
                var.end_pc = pc;
            }
        }
    }

    pub(super) fn store_var(&mut self, var: &Expr, exp: Expr) -> Result<(), ParseErr> {
        log::debug!("store var, var: {}, exp: {}", var.value, exp.value);
        let mut exp = exp;

        match var.value {
            ExprValue::Local(_, ridx) => {
                self.exp_free(&exp)?;
                self.exp_toreg(exp, ridx)?;
                return Ok(());
            }
            ExprValue::Upval(vreg) => {
                exp = self.exp_toanyreg(exp.clone())?;
                let exreg = self.locreg(&exp)?;

                self.p.emiter().emit_setupval(exreg, vreg);
            }
            ExprValue::IndexUp(t, idx) => {
                exp = self.exp_tokreg(exp)?;

                match exp.value {
                    ExprValue::Nonreloc(reg) => self.p.emiter().emit_settabup(t, idx, reg, false),
                    ExprValue::K(reg) => self.p.emiter().emit_settabup(t, idx, reg, true),
                    _ => unreachable!(),
                };
            }
            ExprValue::IndexI(t, idx) => {
                exp = self.exp_tokreg(exp)?;

                match exp.value {
                    ExprValue::Nonreloc(reg) => self.p.emiter().emit_seti(t, idx, reg, false),
                    ExprValue::K(reg) => self.p.emiter().emit_seti(t, idx, reg, true),
                    _ => unreachable!(),
                };
            }
            ExprValue::IndexStr(t, idx) => {
                exp = self.exp_tokreg(exp)?;

                match exp.value {
                    ExprValue::Nonreloc(reg) => self.p.emiter().emit_setfield(t, idx, reg, false),
                    ExprValue::K(reg) => self.p.emiter().emit_setfield(t, idx, reg, true),
                    _ => unreachable!(),
                };
            }
            ExprValue::Indexed(t, idx) => {
                exp = self.exp_tokreg(exp)?;

                match exp.value {
                    ExprValue::Nonreloc(reg) => self.p.emiter().emit_settable(t, idx, reg, false),
                    ExprValue::K(reg) => self.p.emiter().emit_settable(t, idx, reg, true),
                    _ => unreachable!(),
                };
            }
            _ => return Err(ParseErr::BadUsage),
        }

        self.exp_free(&exp)?;

        Ok(())
    }

    pub(super) fn reglevel(&self, nvar: usize) -> usize {
        let mut nvar = nvar;

        while nvar > 0 {
            nvar -= 1;

            match self.getloc(nvar) {
                Some(var) if !matches!(var.kind, VarKind::CTC) => return var.r_idx + 1,
                _ => {}
            }
        }
        0
    }

    pub(super) fn instack_nvars(&self) -> usize {
        let nactvar = self.p.fs.prop().nactvar;
        log::debug!("nvars_stack, fs nactvar: {}", nactvar);

        self.reglevel(nactvar)
    }

    pub(super) fn getloc_aux(&self, fs: &FuncState, idx: usize) -> Option<Var> {
        self.p
            .actvar
            .get(fs.prop().first_local + idx)
            .and_then(|v| Some(v.clone()))
    }

    pub(super) fn getloc(&self, idx: usize) -> Option<Var> {
        self.getloc_aux(&self.p.fs, idx)
    }

    pub(super) fn getloc_mut(&mut self, idx: usize) -> Option<&mut Var> {
        self.p.actvar.get_mut(self.p.fs.prop().first_local + idx)
    }

    pub(super) fn pushupval(&mut self, upval: Upval) -> usize {
        self.p.fs.prop().proto.prop_mut().upvars.push(upval);
        self.p.fs.prop_mut().nups += 1;

        self.p.fs.prop().nups - 1
    }

    pub(super) fn pushloc(&mut self, var: Var) -> usize {
        self.p.actvar.push(var);

        self.p.actvar.len() - 1 - self.p.fs.prop().first_local
    }

    pub(super) fn adjust_localvars(&mut self, nvars: usize) {
        log::debug!("adjust localvars: {}", nvars);

        let mut reglevel = self.instack_nvars();

        for _ in 0..nvars {
            let vidx = self.p.fs.prop().nactvar;
            self.p.fs.prop_mut().nactvar += 1;

            log::debug!(
                "fscope nactvar increment, value: {}",
                self.p.fs.prop().nactvar
            );

            if let Some(var) = self.getloc(vidx) {
                let pidx = self.register_localvar(&var.name);

                let mut var = self.getloc_mut(vidx).unwrap();
                var.r_idx = reglevel;
                var.p_idx = pidx;

                log::debug!(
                    "set varname: {} ridx: {}, pidx: {}",
                    var.name,
                    var.r_idx,
                    var.p_idx
                );

                reglevel += 1;
            }
        }
    }

    pub(super) fn register_localvar(&mut self, name: &str) -> usize {
        let pc = self.p.emiter().pc();
        self.p.fs.prop().proto.prop_mut().locvars.push(LocVar {
            name: name.to_string(),
            start_pc: pc,
            end_pc: 0,
        });

        self.p.fs.prop().proto.prop().locvars.len() - 1
    }

    pub(super) fn adjust_assign(
        &mut self,
        nvars: usize,
        nexps: usize,
        exp: Expr,
    ) -> Result<(), ParseErr> {
        log::debug!("adjust_assign, nvars: {}, nexps: {}", nvars, nexps);

        let needed = nvars as i32 - nexps as i32;

        if exp.hasmultret() {
            let extra = if needed + 1 >= 0 { needed + 1 } else { 0 } as usize;
            self.p.pexp().setreturns(&exp, extra)?;
        } else {
            if !matches!(exp.value, ExprValue::Void) {
                self.exp_toanyreg(exp)?;
            }

            if needed > 0 {
                let freereg = self.p.fs.prop().freereg;
                self.p.emiter().emit_loadnil(freereg, needed as usize);
            }
        }

        if needed > 0 {
            self.reserver_regs(needed as usize);
        } else {
            self.p.fs.prop_mut().freereg -= needed.abs() as usize;
        }

        Ok(())
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

    pub(super) fn discharge_toreg(&mut self, exp: Expr, reg: usize) -> Result<Expr, ParseErr> {
        let exp = self.discharge_tovar(exp)?;

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
                .pexp()
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

                self.p.pexp().jump_patchtohere(fj)?;
            }

            let final_pc = self.p.emiter().mark_pc();

            self.p
                .pexp()
                .jump_patchlistaux(exp.false_jumpto, Some(final_pc), Some(reg), pf)?;
            self.p
                .pexp()
                .jump_patchlistaux(exp.true_jumpto, Some(final_pc), Some(reg), pt)?;
        }

        Ok(Expr::nonreloc(reg))
    }

    pub(super) fn exp_tonextreg(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        log::debug!("exp tonextreg exp: {}", exp.value);

        let exp = self.discharge_tovar(exp)?;

        self.exp_free(&exp)?;
        self.reserver_regs(1);

        let reg = self.p.fs.prop().freereg - 1;
        self.exp_toreg(exp, reg)
    }

    pub(super) fn exp_toanyreg(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        log::debug!("exp toanyreg, exp: {}", exp.value);

        let exp = self.discharge_tovar(exp)?;

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
        if reg < self.instack_nvars() {
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

    pub(super) fn indexed(&mut self, tab: Expr, key: Expr) -> Result<Expr, ParseErr> {
        let key = if matches!(key.value, ExprValue::String(..)) {
            self.p.pvar().exp_tok(key)?
        } else {
            key
        };

        let tab = if matches!(tab.value, ExprValue::Upval(..))
            && matches!(key.value, ExprValue::K(kidx) if matches!(self.p.constant_pool.borrow().get(kidx), Some(ConstantValue::String(..))))
        {
            self.p.pvar().exp_toanyreg(tab)?
        } else {
            tab
        };

        let exp = if let ExprValue::Upval(reg) = tab.value {
            let ikey = if let ExprValue::K(kidx) = key.value {
                kidx
            } else {
                return Err(ParseErr::BadUsage);
            };

            Expr::indexup(reg, ikey)
        } else {
            let treg = match tab.value {
                ExprValue::Local(_, reg) => reg,
                ExprValue::Nonreloc(reg) => reg,
                _ => return Err(ParseErr::BadUsage),
            };

            match key.value {
                ExprValue::K(kidx)
                    if matches!(
                        self.p.constant_pool.borrow().get(kidx),
                        Some(ConstantValue::String(..))
                    ) =>
                {
                    Expr::indexstr(treg, kidx)
                }
                ExprValue::Integer(kval) => Expr::indexi(treg, kval),
                _ => {
                    let key = self.p.pvar().exp_toanyreg(key)?;
                    let kreg = self.p.pvar().locreg(&key)?;

                    Expr::indexed(treg, kreg)
                }
            }
        };

        Ok(exp)
    }
}
