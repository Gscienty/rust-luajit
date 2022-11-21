use super::RefValue;

#[derive(Clone, Copy)]
pub(crate) enum VarKind {
    REG,
    CONST,
    TOCLOSE,
    CTC,

    UNKNOW,
}

pub(crate) struct VarDesc {
    pub(crate) kind: VarKind,
    pub(crate) r_idx: usize,
    pub(crate) p_idx: usize,
    pub(crate) name: String,

    pub(crate) k: RefValue,
}

impl VarDesc {
    pub(crate) fn new(name: &str, kind: VarKind) -> Self {
        VarDesc {
            kind,
            r_idx: 0,
            p_idx: 0,
            name: String::from(name),
            k: RefValue::new(),
        }
    }
}
