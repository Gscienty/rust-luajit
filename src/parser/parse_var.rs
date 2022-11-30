use std::ops::Deref;

use crate::object::{Expr, ExprValue, LocVar, RefValue, Upval, Value, Var, VarKind};

use super::{FuncState, ParseErr, Parser};

pub(super) struct ParseVar<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseVar<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
    }

    fn searchupval(&mut self, fs: &FuncState, name: &str) -> Option<usize> {
        let nups = fs.prop().nups;
        for i in 0..nups {
            match self.p.fs.prop().proto.prop().upvars.get(i) {
                Some(upval) if upval.name.eq(name) => return Some(i),
                _ => {}
            }
        }

        None
    }

    fn searchvar(&mut self, fs: &FuncState, name: &str) -> Option<Expr> {
        log::debug!("search var: {}", name);

        let nactvar = self.p.fs.prop().nactvar;

        log::debug!("nactvar = {}", nactvar);

        for idx in (0..nactvar).rev() {
            match self.getlocaux(fs, idx) {
                Some(v) if v.name.eq(name) => {
                    return Some(match v.kind {
                        VarKind::CTC => {
                            let abs_idx = self.p.fs.prop().first_local + idx;
                            Expr::econst(abs_idx)
                        }
                        _ => Expr::local(idx, v.r_idx),
                    })
                }
                Some(v) => log::debug!("searched varname: {}", v.name),
                _ => {}
            }
        }

        None
    }

    fn single_varaux(&mut self, fs: &FuncState, name: &str, base: bool) -> Result<Expr, ParseErr> {
        log::debug!("single varaux {}", name);

        if let Some(exp) = self.searchvar(fs, name) {
            match exp.value {
                ExprValue::Local(vidx, _) if !base => {
                    let mut bl = fs.prop().block.clone();

                    while bl.prop().nactvar > vidx {
                        let pbl = bl.prop().prev.clone();
                        bl = match pbl {
                            Some(pbl) => pbl,
                            _ => break,
                        };
                    }
                    bl.prop_mut().upval = true;
                    bl.prop_mut().needclose = true;

                    Ok(exp)
                }
                _ => Ok(exp),
            }
        } else {
            let uidx = match self.searchupval(fs, name) {
                Some(uidx) => uidx,
                _ => match fs.prop().prev.clone() {
                    Some(pfs) => {
                        let exp = self.single_varaux(&pfs, name, false)?;

                        match exp.value {
                            ExprValue::Local(vidx, ridx) => {
                                let mut upval = Upval::new(name);
                                upval.name = name.to_string();
                                upval.instack = true;
                                upval.idx = ridx;
                                if let Some(var) = self.getlocaux(&pfs, vidx) {
                                    upval.kind = var.kind
                                } else {
                                    return Err(ParseErr::BadUsage);
                                }

                                self.pushupval(upval)
                            }
                            ExprValue::Upval(vidx) => {
                                let mut upval = Upval::new(name);
                                upval.name = name.to_string();
                                upval.idx = vidx;
                                if let Some(var) = pfs.prop().proto.prop().upvars.get(vidx) {
                                    upval.kind = var.kind
                                } else {
                                    return Err(ParseErr::BadUsage);
                                }

                                self.pushupval(upval)
                            }
                            _ => return Ok(exp),
                        }
                    }
                    _ => return Ok(Expr::void()),
                },
            };

            Ok(Expr::upval(uidx))
        }
    }

    pub(super) fn single_var(&mut self, name: &str) -> Result<Expr, ParseErr> {
        log::debug!("single var {}", name);

        let fs = self.p.fs.clone();

        let exp = self.single_varaux(&fs, name, true)?;

        if matches!(exp.value, ExprValue::Void) {
            log::debug!("global name: {}", name);

            let mut exp = self.single_varaux(&fs, Parser::ENV, true)?;
            exp = self.p.preg().exp_toanyregup(exp)?;

            self.p.ptab().indexed(exp, Expr::from(name))
        } else {
            Ok(exp)
        }
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
                self.p.preg().free_reg(t)?;
                Ok(Expr::reloc(self.p.emiter().emit_geti(t, idx)))
            }
            ExprValue::IndexStr(t, idx) => {
                self.p.preg().free_reg(t)?;
                Ok(Expr::reloc(self.p.emiter().emit_getfield(t, idx)))
            }
            ExprValue::Indexed(t, idx) => {
                if t < idx {
                    self.p.preg().free_reg(idx)?;
                    self.p.preg().free_reg(t)?;
                } else {
                    self.p.preg().free_reg(t)?;
                    self.p.preg().free_reg(idx)?;
                }

                Ok(Expr::reloc(self.p.emiter().emit_getfield(t, idx)))
            }
            ExprValue::VarArg(pc) => {
                self.p.emiter().set_rc(pc, 2);
                Ok(Expr::reloc(pc))
            }
            ExprValue::Call(_pc) => {
                // TODO
                Ok(Expr::todo())
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

            if let Some(var) = self.p.fs.prop_mut().proto.prop_mut().locvars.get_mut(pidx) {
                var.end_pc = pc;
            }
        }
    }

    pub(super) fn store_var(&mut self, var: Expr, exp: Expr) -> Result<(), ParseErr> {
        log::debug!("store var, var: {}, exp: {}", var.value, exp.value);
        let mut exp = exp;

        match var.value {
            ExprValue::Local(_, ridx) => {
                self.p.preg().exp_free(&exp)?;
                self.p.preg().exp_toreg(exp, ridx)?;
                return Ok(());
            }
            ExprValue::Upval(vreg) => {
                exp = self.p.preg().exp_toanyreg(exp.clone())?;
                let exreg = self.p.preg().locreg(&exp)?;

                self.p.emiter().emit_setupval(exreg, vreg);
            }
            ExprValue::IndexUp(t, idx) => {
                exp = self.p.preg().exp_tokreg(exp)?;

                match exp.value {
                    ExprValue::Nonreloc(reg) => self.p.emiter().emit_settabup(t, idx, reg, false),
                    ExprValue::K(reg) => self.p.emiter().emit_settabup(t, idx, reg, true),
                    _ => unreachable!(),
                };
            }
            ExprValue::IndexI(t, idx) => {
                exp = self.p.preg().exp_tokreg(exp)?;

                match exp.value {
                    ExprValue::Nonreloc(reg) => self.p.emiter().emit_seti(t, idx, reg, false),
                    ExprValue::K(reg) => self.p.emiter().emit_seti(t, idx, reg, true),
                    _ => unreachable!(),
                };
            }
            ExprValue::IndexStr(t, idx) => {
                exp = self.p.preg().exp_tokreg(exp)?;

                match exp.value {
                    ExprValue::Nonreloc(reg) => self.p.emiter().emit_setfield(t, idx, reg, false),
                    ExprValue::K(reg) => self.p.emiter().emit_setfield(t, idx, reg, true),
                    _ => unreachable!(),
                };
            }
            ExprValue::Indexed(t, idx) => {
                exp = self.p.preg().exp_tokreg(exp)?;

                match exp.value {
                    ExprValue::Nonreloc(reg) => self.p.emiter().emit_settable(t, idx, reg, false),
                    ExprValue::K(reg) => self.p.emiter().emit_settable(t, idx, reg, true),
                    _ => unreachable!(),
                };
            }
            _ => return Err(ParseErr::BadUsage),
        }

        self.p.preg().exp_free(&exp)?;
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

    pub(super) fn nvars_stack(&self) -> usize {
        let nactvar = self.p.fs.prop().nactvar;
        log::debug!("nvars_stack, fs nactvar: {}", nactvar);

        self.reglevel(nactvar)
    }

    pub(super) fn getloc_abs(&self, idx: usize) -> Option<&Var> {
        self.p.actvar.get(idx)
    }

    pub(super) fn getlocaux(&self, fs: &FuncState, idx: usize) -> Option<Var> {
        self.p
            .actvar
            .get(fs.prop().first_local + idx)
            .and_then(|v| Some(v.clone()))
    }

    pub(super) fn getloc(&self, idx: usize) -> Option<Var> {
        let fs = self.p.fs.clone();

        self.getlocaux(&fs, idx)
    }

    pub(super) fn getloc_mut(&mut self, idx: usize) -> Option<&mut Var> {
        self.p.actvar.get_mut(self.p.fs.prop().first_local + idx)
    }

    pub(super) fn pushupval(&mut self, upval: Upval) -> usize {
        self.p.fs.prop_mut().proto.prop_mut().upvars.push(upval);
        self.p.fs.prop_mut().nups += 1;

        self.p.fs.prop().nups - 1
    }

    pub(super) fn pushloc(&mut self, var: Var) -> usize {
        self.p.actvar.push(var);

        self.p.actvar.len() - 1 - self.p.fs.prop().first_local
    }

    pub(super) fn adjust_localvars(&mut self, nvars: usize) {
        log::debug!("adjust localvars: {}", nvars);

        let mut reglevel = self.nvars_stack();

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
        self.p.fs.prop_mut().proto.prop_mut().locvars.push(LocVar {
            name: name.to_string(),
            start_pc: pc,
            end_pc: 0,
        });

        self.p.fs.prop().proto.prop().locvars.len() - 1
    }
}
