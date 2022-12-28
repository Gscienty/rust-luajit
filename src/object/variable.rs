use super::RefValue;

#[derive(Clone)]
pub(crate) struct Var {
    pub(crate) name: String,

    pub(crate) kind: VarKind,

    pub(crate) val: RefValue,

    pub(crate) r_idx: usize,
    pub(crate) p_idx: usize,
}

#[derive(Clone, Copy)]
pub(crate) enum VarKind {
    REG,     // regular
    CTC,     // compile time constant
    TOCLOSE, // to be closed
    CONST,   // constant
}

impl Var {
    pub(crate) fn new(name: String, kind: VarKind) -> Self {
        Self {
            name,
            kind,
            val: RefValue::new(),
            r_idx: 0,
            p_idx: 0,
        }
    }

    pub(crate) fn new_old(name: &str) -> Self {
        Self {
            name: name.to_string(),
            kind: VarKind::REG,
            val: RefValue::new(),
            r_idx: 0,
            p_idx: 0,
        }
    }
}
