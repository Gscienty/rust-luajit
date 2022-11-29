#[derive(Clone)]
pub(crate) struct LabelDesc {
    pub(crate) name: String,
    pub(crate) pc: usize,
    pub(crate) nactvar: usize,
    pub(crate) close: bool,
}

impl LabelDesc {
    pub(crate) fn new(name: &str, pc: usize, nactvar: usize) -> Self {
        Self {
            name: name.to_string(),
            pc,
            nactvar,
            close: false,
        }
    }
}
