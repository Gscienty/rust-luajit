use crate::object::{ConstantValue, Expr, ExprValue};

use super::{ParseErr, Parser};

pub(super) struct ParseTab<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseTab<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
    }

    pub(super) fn indexed(&mut self, tab: Expr, key: Expr) -> Result<Expr, ParseErr> {
        let key = if matches!(key.value, ExprValue::String(..)) {
            self.p.preg().exp_tok(key)?
        } else {
            key
        };

        let tab = if matches!(tab.value, ExprValue::Upval(..))
            && matches!(key.value, ExprValue::K(kidx) if matches!(self.p.constant_pool.borrow().get(kidx), Some(ConstantValue::String(..))))
        {
            self.p.preg().exp_toanyreg(tab)?
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
                    let key = self.p.preg().exp_toanyreg(key)?;
                    let kreg = self.p.preg().locreg(&key)?;

                    Expr::indexed(treg, kreg)
                }
            }
        };

        Ok(exp)
    }
}
