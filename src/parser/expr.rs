use std::fmt::Display;

#[derive(Clone)]
pub(super) enum ExprValue {
    Float(f64),
    Integer(i64),
    String(String),
    Bool(bool),
    Nil,

    Void,

    K(usize), // iK

    Nonreloc(usize), // R, expression has its value in a fixed register

    VarArg(usize), // PC
    Call(usize),   // PC
    Jump(usize),   // PC, expression is a test / comparsion
    Reloc(usize),  // PC, put result in any register

    Local(usize, usize), // idx, rR

    Upval(usize), // R

    Index(usize, usize),    // tR, rR
    IndexUp(usize, usize),  // tR, rR
    IndexI(usize, usize),   // tR, rR
    IndexStr(usize, usize), // tR, rR
    Indexed(usize, usize),  // tR, rR

    Var(usize, usize), // gidx, sidx
    Const(usize),      // gidx

    TODO,
}

#[derive(Clone)]
pub(super) struct Expr {
    pub(super) value: ExprValue,
    pub(super) true_jumpto: Option<usize>,
    pub(super) false_jumpto: Option<usize>,
}

impl From<i64> for Expr {
    fn from(value: i64) -> Self {
        Expr {
            value: ExprValue::Integer(value),
            true_jumpto: None,
            false_jumpto: None,
        }
    }
}

impl From<f64> for Expr {
    fn from(value: f64) -> Self {
        Expr {
            value: ExprValue::Float(value),
            true_jumpto: None,
            false_jumpto: None,
        }
    }
}

impl From<&str> for Expr {
    fn from(value: &str) -> Self {
        Expr {
            value: ExprValue::String(value.to_string()),
            true_jumpto: None,
            false_jumpto: None,
        }
    }
}

impl From<bool> for Expr {
    fn from(value: bool) -> Self {
        Expr {
            value: ExprValue::Bool(value),
            true_jumpto: None,
            false_jumpto: None,
        }
    }
}

impl Expr {
    pub(super) fn vararg(pc: usize) -> Self {
        Expr {
            value: ExprValue::VarArg(pc),
            true_jumpto: None,
            false_jumpto: None,
        }
    }

    pub(super) fn nil() -> Self {
        Expr {
            value: ExprValue::Nil,
            true_jumpto: None,
            false_jumpto: None,
        }
    }

    pub(super) fn todo() -> Self {
        Expr {
            value: ExprValue::TODO,
            true_jumpto: None,
            false_jumpto: None,
        }
    }

    pub(super) fn nonreloc(reg: usize) -> Self {
        Expr {
            value: ExprValue::Nonreloc(reg),
            true_jumpto: None,
            false_jumpto: None,
        }
    }

    pub(super) fn reloc(pc: usize) -> Self {
        Expr {
            value: ExprValue::Reloc(pc),
            true_jumpto: None,
            false_jumpto: None,
        }
    }

    pub(super) fn jmp(pc: usize) -> Self {
        Expr {
            value: ExprValue::Jump(pc),
            true_jumpto: None,
            false_jumpto: None,
        }
    }

    pub(super) fn k(idx: usize) -> Self {
        Expr {
            value: ExprValue::K(idx),
            true_jumpto: None,
            false_jumpto: None,
        }
    }

    pub(super) fn numeric(&self) -> bool {
        match self.value {
            ExprValue::Float(_) | ExprValue::Integer(_) => true,
            _ => false,
        }
    }

    pub(super) fn inreg(&self) -> bool {
        matches!(self.value, ExprValue::Nonreloc(_) | ExprValue::Reloc(_))
    }

    pub(super) fn tj(mut self, tj: Option<usize>) -> Self {
        self.true_jumpto = tj;

        self
    }

    pub(super) fn fj(mut self, tf: Option<usize>) -> Self {
        self.false_jumpto = tf;

        self
    }

    pub(super) fn hasjump(&self) -> bool {
        self.true_jumpto != self.false_jumpto
    }
}

impl Display for ExprValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprValue::Float(v) => write!(f, "Float({})", *v),
            ExprValue::Integer(v) => write!(f, "Integer({})", *v),
            ExprValue::String(v) => write!(f, "String({})", v.as_str()),
            ExprValue::Bool(v) => write!(f, "Bool({})", *v),
            ExprValue::Nil => write!(f, "Nil"),
            ExprValue::Void => write!(f, "Void"),
            ExprValue::TODO => write!(f, "Todo"),
            ExprValue::K(v) => write!(f, "K({})", *v),
            ExprValue::Nonreloc(v) => write!(f, "Nonreloc({})", *v),
            ExprValue::VarArg(v) => write!(f, "VarArg({})", *v),
            ExprValue::Call(v) => write!(f, "Call({})", *v),
            ExprValue::Jump(v) => write!(f, "Jump({})", *v),
            ExprValue::Reloc(v) => write!(f, "Reloc({})", *v),
            ExprValue::Upval(v) => write!(f, "Upval({})", *v),
            ExprValue::Local(v1, v2) => write!(f, "Local({}, {})", *v1, *v2),
            ExprValue::Index(v1, v2) => write!(f, "Index({}, {})", *v1, *v2),
            ExprValue::IndexUp(v1, v2) => write!(f, "IndexUp({}, {})", *v1, *v2),
            ExprValue::IndexI(v1, v2) => write!(f, "IndexI({}, {})", *v1, *v2),
            ExprValue::IndexStr(v1, v2) => write!(f, "IndexStr({}, {})", *v1, *v2),
            ExprValue::Indexed(v1, v2) => write!(f, "Indexed({}, {})", *v1, *v2),
            ExprValue::Var(v1, v2) => write!(f, "Var({}, {})", *v1, *v2),
            ExprValue::Const(v1) => write!(f, "Const({})", *v1),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::Parser, utils::Logger};

    #[test]
    fn test_code_concat() {
        log::set_logger(&Logger {}).unwrap();
        log::set_max_level(log::LevelFilter::Debug);

        let mut p = Parser::new(
            "local a = ('a' == 'b' or 1 < 2) + ('a'..'b') + 'c'..'d' + 'e' .. 'f' .. 'g'",
        );

        assert!(p.parse().is_ok());

        for ci in 0..p.get_codelen() {
            if let Some(c) = p.get_code(ci) {
                println!("{}", *c);
            }
        }
    }
}
