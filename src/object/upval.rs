use super::VarKind;

pub(crate) struct UpvalDesc {
    pub(crate) name: String,
    pub(crate) instack: bool,
    pub(crate) idx: usize,
    pub(crate) kind: VarKind,
}

impl UpvalDesc {
    pub(crate) fn new(name: &str) -> Self {
        Self {
            name: String::from(name),
            instack: false,
            idx: 0,
            kind: VarKind::UNKNOW,
        }
    }
}
