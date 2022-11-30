#[derive(Clone)]
pub(crate) struct LocVar {
    pub(crate) name: String,
    pub(crate) start_pc: usize,
    pub(crate) end_pc: usize,
}
