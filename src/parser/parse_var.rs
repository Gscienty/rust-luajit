use std::ops::Deref;

use crate::object::{Expr, ExprValue, RefValue, Value, VarKind};

use super::{ParseErr, Parser};

pub(super) struct ParseVar<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseVar<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
    }

    fn searchvar(&mut self, name: &str) -> Result<Expr, ParseErr> {
        log::debug!("search var: {}", name);

        let nactvar = self.p.fs.prop().nactvar;

        for idx in (0..nactvar).rev() {
            match self.p.pfscope().getloc(idx) {
                Some(v) if v.name.eq(name) => {
                    return Ok(match v.kind {
                        VarKind::CTC => {
                            let abs_idx = self.p.fs.prop().first_local + idx;
                            Expr::econst(abs_idx)
                        }
                        _ => Expr::local(idx, v.r_idx),
                    })
                }
                _ => {}
            }
        }

        Err(ParseErr::BadUsage)
    }

    fn single_varaux(&mut self, name: &str, _base: bool) -> Result<Expr, ParseErr> {
        // TODO global should return void, single var process
        if let Ok(exp) = self.searchvar(name) {
            Ok(exp)
        } else {
            // TODO upval
            Ok(Expr::todo())
        }
    }

    pub(super) fn single_var(&mut self, name: &str) -> Result<Expr, ParseErr> {
        // TODO gfscope local var is ENV[name]
        self.single_varaux(name, true)
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
                self.p.pfscope().free_reg(t)?;
                Ok(Expr::reloc(self.p.emiter().emit_geti(t, idx)))
            }
            ExprValue::IndexStr(t, idx) => {
                self.p.pfscope().free_reg(t)?;
                Ok(Expr::reloc(self.p.emiter().emit_getfield(t, idx)))
            }
            ExprValue::Indexed(t, idx) => {
                if t < idx {
                    self.p.pfscope().free_reg(idx)?;
                    self.p.pfscope().free_reg(t)?;
                } else {
                    self.p.pfscope().free_reg(t)?;
                    self.p.pfscope().free_reg(idx)?;
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
}
