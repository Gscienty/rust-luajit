use crate::object::{Expr, ExprValue, LocVar, Var, VarKind};

use super::{Block, ParseErr, Parser};

pub(super) struct ParseFunc<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseFunc<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
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

    pub(super) fn getloc(&self, idx: usize) -> Option<Var> {
        self.p
            .actvar
            .get(self.p.fs.prop().first_local + idx)
            .and_then(|v| Some(v.clone()))
    }

    pub(super) fn getloc_mut(&mut self, idx: usize) -> Option<&mut Var> {
        self.p.actvar.get_mut(self.p.fs.prop().first_local + idx)
    }

    pub(super) fn pushloc(&mut self, var: Var) -> usize {
        self.p.actvar.push(var);

        self.p.actvar.len() - 1 - self.p.fs.prop().first_local
    }

    pub(super) fn adjust_localvars(&mut self, nvars: usize) {
        log::debug!("adjust localvars: {}", nvars);

        let mut reglevel = self.p.pfscope().nvars_stack();

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

    pub(super) fn reserver_regs(&mut self, n: usize) {
        self.p.fs.prop_mut().freereg += n;
    }

    pub(super) fn free_reg(&mut self, reg: usize) -> Result<(), ParseErr> {
        if reg < self.nvars_stack() {
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

    pub(super) fn enterblock(&mut self, isloop: bool) {
        log::debug!("enter block, nactvar == {}", self.p.fs.prop().nactvar);

        let block = Block::new();

        block.prop_mut().is_loop = isloop;
        block.prop_mut().nactvar = self.p.fs.prop().nactvar;
        block.prop_mut().firstlabel = self.p.label.len();
        block.prop_mut().firstgoto = self.p.goto.len();
        block.prop_mut().inside_tobeclosed = self.p.fs.prop().block.prop().inside_tobeclosed;
        block.prop_mut().prev = Some(self.p.fs.prop().block.clone());

        self.p.fs.prop_mut().block = block;
    }

    pub(super) fn leaveblock(&mut self) -> Result<(), ParseErr> {
        log::debug!("leave block, nactvar == {}", self.p.fs.prop().nactvar);

        let block = self.p.fs.prop().block.clone();

        let level = self.reglevel(block.prop().nactvar);
        self.p.pvar().removevars(level);

        let hasclose = if block.prop().is_loop {
            self.p.plabel().createlabel("break", false)?
        } else {
            false
        };

        if !hasclose && block.prop().prev.is_some() && block.prop().upval {
            self.p.emiter().emit_close(level);
        }

        self.p.fs.prop_mut().freereg = level;
        while self.p.label.len() > block.prop().firstlabel {
            self.p.label.pop();
        }

        if let Some(prev_block) = &block.prop().prev {
            self.p.fs.prop_mut().block = prev_block.clone();
            self.p.plabel().movegotosout(&block)?;
        } else if block.prop().firstgoto < self.p.goto.len() {
            return Err(ParseErr::BadUsage);
        }

        Ok(())
    }
}
