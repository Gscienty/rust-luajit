pub(crate) mod codelimit {
    pub(crate) const MAX_A: u32 = (1 << 8) - 1;
    pub(crate) const MAX_B: u32 = (1 << 8) - 1;
    pub(crate) const MAX_C: u32 = (1 << 8) - 1;
    pub(crate) const MAX_BX: u32 = (1 << 17) - 1;
    pub(crate) const MAX_SBX: u32 = (1 << 17) - 1;
    pub(crate) const MAX_AX: u32 = (1 << 25) - 1;
    pub(crate) const MAX_JX: u32 = (1 << 25) - 1;

    pub(crate) const MAX_OFFSET_SBX: u32 = MAX_SBX >> 1;

    pub(crate) const NO_JMP: usize = (1 << 25) - 1;
    pub(crate) const NO_REG: u32 = MAX_A;
}
