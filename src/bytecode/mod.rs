mod ops;
mod compose;

pub(crate) use ops::ByteOps;
pub(crate) use compose::*;

pub(crate) type ByteInstruction = u32;

pub(crate) mod bytecode_range {
    pub const J: u32 = 0x8000;
    pub const NOJMP: u32 = u32::MAX;
    pub const MAXA: u32 = 0xff;
    pub const MAXB: u32 = 0xff;
    pub const MAXC: u32 = 0xff;
    pub const MAXD: u32 = 0xffff;
    pub const NOREG: u32 = MAXA;
}