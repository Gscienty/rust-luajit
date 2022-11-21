use crate::parser::ParseErr;

use super::{OpCode, OpMode};

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

#[derive(Clone, Copy)]
pub(crate) struct Code {
    pub(crate) op: OpCode,
    pub(crate) k: bool,
    pub(crate) ra: u32,
    pub(crate) rb: u32,
    pub(crate) rc: u32,
    pub(crate) j: u32,
}

impl Code {
    pub(crate) fn new_abc(
        op: OpCode,
        ra: u32,
        rb: u32,
        rc: u32,
        k: bool,
    ) -> Result<Self, ParseErr> {
        if !matches!(op.mode(), OpMode::ABCMode) {
            return Err(ParseErr::BadUsage);
        }
        if ra > codelimit::MAX_A || rb > codelimit::MAX_B || rc > codelimit::MAX_C {
            return Err(ParseErr::BadUsage);
        }

        Ok(Self {
            op,
            k,
            ra,
            rb,
            rc,
            j: 0,
        })
    }

    pub(crate) fn new_abx(op: OpCode, ra: u32, rb: u32) -> Result<Self, ParseErr> {
        if !matches!(op.mode(), OpMode::ABxMode) {
            return Err(ParseErr::BadUsage);
        }
        if ra > codelimit::MAX_A || rb > codelimit::MAX_BX {
            return Err(ParseErr::BadUsage);
        }

        Ok(Self {
            op,
            k: false,
            ra,
            rb,
            rc: 0,
            j: 0,
        })
    }

    pub(crate) fn new_asbx(op: OpCode, ra: u32, rb: u32) -> Result<Self, ParseErr> {
        if !matches!(op.mode(), OpMode::AsBxMode) {
            return Err(ParseErr::BadUsage);
        }
        if ra > codelimit::MAX_A || rb > codelimit::MAX_SBX {
            return Err(ParseErr::BadUsage);
        }

        Ok(Self {
            op,
            k: false,
            ra,
            rb,
            rc: 0,
            j: 0,
        })
    }

    pub(crate) fn new_ax(op: OpCode, ra: u32) -> Result<Self, ParseErr> {
        if !matches!(op.mode(), OpMode::AxMode) {
            return Err(ParseErr::BadUsage);
        }
        if ra > codelimit::MAX_AX {
            return Err(ParseErr::BadUsage);
        }

        Ok(Self {
            op,
            k: false,
            ra,
            rb: 0,
            rc: 0,
            j: 0,
        })
    }

    pub(crate) fn new_j(op: OpCode, j: u32) -> Result<Self, ParseErr> {
        if !matches!(op.mode(), OpMode::JMode) {
            return Err(ParseErr::BadUsage);
        }
        if j > codelimit::MAX_JX {
            return Err(ParseErr::BadUsage);
        }

        Ok(Self {
            op,
            k: false,
            ra: 0,
            rb: 0,
            rc: 0,
            j,
        })
    }

    pub(crate) fn bits(&self) -> u32 {
        let op = self.op.bits();

        match self.op.mode() {
            OpMode::ABCMode => {
                let ra = self.ra << 7;
                let k = if self.k { 1 << 15 } else { 0 };
                let rb = self.rb << 16;
                let rc = self.rc << 24;

                op | ra | k | rb | rc
            }
            OpMode::ABxMode | OpMode::AsBxMode => {
                let ra = self.ra << 7;
                let rb = self.rb << 15;

                op | ra | rb
            }
            OpMode::AxMode => {
                let ra = self.ra << 7;

                op | ra
            }
            OpMode::JMode => {
                let j = self.j << 7;

                op | j
            }
        }
    }
}
