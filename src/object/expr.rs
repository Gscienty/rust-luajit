use crate::{code::codelimit, object::RefValue};

pub(crate) enum ExprKind {
    VOID,
    NIL,
    TRUE,
    FALSE,
    K,
    KFLT,
    KINT,
    KSTR,
    NONRELOC,
    LOCAL,
    UPVAL,
    CONST,
    INDEXED,
    INDEXUP,
    INDEXI,
    INDEXSTR,
    JMP,
    RELOC,
    CALL,
    VARARG,
}

pub(crate) struct ExprDesc {
    pub(crate) kind: ExprKind,
    pub(crate) t: usize, // patch list of 'exit with true'
    pub(crate) f: usize, // patch list of 'exit with false'

    pub(crate) info: usize,
    pub(crate) val: Option<RefValue>,

    pub(crate) r_idx: usize, // register holding the variable
    pub(crate) v_idx: usize, // compiler index

    pub(crate) ind_tab: u32, // table register
    pub(crate) ind_idx: u32, // table key's index
}

impl ExprDesc {
    pub(crate) fn new() -> Self {
        Self {
            kind: ExprKind::VOID,
            t: codelimit::NO_JMP,
            f: codelimit::NO_JMP,

            info: 0,
            val: None,

            r_idx: 0,
            v_idx: 0,

            ind_tab: 0,
            ind_idx: 0,
        }
    }

    pub(crate) fn has_jmp(&self) -> bool {
        self.t != self.f
    }
}
