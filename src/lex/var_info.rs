pub(crate) mod var_info_flags {
    pub(crate) const VARRW: u8 = 0x01;
    pub(crate) const GOTO: u8 = 0x02;
    pub(crate) const LABEL: u8 = 0x04;
}

#[derive(Clone)]
pub(crate) struct VarInfo {
    pub(crate) name: String,
    pub(crate) start_pc: u32,
    pub(crate) end_pc: u32,
    pub(crate) slot: u8,
    pub(crate) info: u8,
}

impl VarInfo {
    pub(crate) fn is_goto(&self) -> bool {
        (self.info & var_info_flags::GOTO).ne(&0)
    }

    pub(crate) fn is_label(&self) -> bool {
        (self.info & var_info_flags::LABEL).ne(&0)
    }

    pub(crate) fn is_goto_label(&self) -> bool {
        (self.info & (var_info_flags::LABEL | var_info_flags::GOTO)).ne(&0)
    }
}