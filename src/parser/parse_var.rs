use crate::object::ConstantValue;

use super::{Expr, ExprValue, ParseErr, Parser};

pub(super) struct ParseVar<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseVar<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
    }

    pub(super) fn discharge_tovar(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        let tj = exp.true_jumpto;
        let fj = exp.false_jumpto;

        match exp.value {
            ExprValue::Const(idx) => match self.p.constant_pool.borrow().get(idx) {
                Some(ConstantValue::Float(v)) => Ok(Expr::from(*v)),
                Some(ConstantValue::Integer(v)) => Ok(Expr::from(*v)),
                Some(ConstantValue::String(v)) => Ok(Expr::from(v.as_str())),
                Some(ConstantValue::Bool(v)) => Ok(Expr::from(*v)),
                Some(ConstantValue::Nil) => Ok(Expr::nil()),
                _ => Err(ParseErr::BadUsage),
            },
            ExprValue::Local(_, reg) => Ok(Expr::nonreloc(reg)),
            ExprValue::Upval(reg) => Ok(Expr::reloc(self.p.parse_code().emit_getupval(reg))),
            ExprValue::IndexUp(t, idx) => {
                Ok(Expr::reloc(self.p.parse_code().emit_gettabup(t, idx)))
            }
            ExprValue::IndexI(t, idx) => {
                self.p.free_reg(t)?;
                Ok(Expr::reloc(self.p.parse_code().emit_geti(t, idx)))
            }
            ExprValue::IndexStr(t, idx) => {
                self.p.free_reg(t)?;
                Ok(Expr::reloc(self.p.parse_code().emit_getfield(t, idx)))
            }
            ExprValue::Indexed(t, idx) => {
                if t < idx {
                    self.p.free_reg(idx)?;
                    self.p.free_reg(t)?;
                } else {
                    self.p.free_reg(t)?;
                    self.p.free_reg(idx)?;
                }

                Ok(Expr::reloc(self.p.parse_code().emit_getfield(t, idx)))
            }
            ExprValue::VarArg(pc) => {
                self.p.parse_code().set_rc(pc, 2);
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
