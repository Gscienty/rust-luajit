use super::ByteOps;

pub(crate) type Instruction = u32;

pub(crate) trait InstructionGetterSetter {
    fn get_ops(&self) -> Option<ByteOps>;
    fn get_a(&self) -> u32;
    fn get_b(&self) -> u32;
    fn get_c(&self) -> u32;
    fn get_d(&self) -> u32;
    fn get_j(&self) -> u32;

    fn set_ops(&mut self, ops: ByteOps);
    fn set_a(&mut self, a: u32);
    fn set_b(&mut self, b: u32);
    fn set_c(&mut self, c: u32);
    fn set_d(&mut self, d: u32);
    fn set_j(&mut self, j: u32);
}

pub(crate) mod ins_limit {
    pub const J: u32 = 0x8000;
    pub const NOJMP: usize = u32::MAX as usize;
    pub const MAXA: u32 = 0xff;
    pub const MAXB: u32 = 0xff;
    pub const MAXC: u32 = 0xff;
    pub const MAXD: u32 = 0xffff;
    pub const NOREG: u32 = MAXA;
}

pub(crate) fn ins_abc(ops: ByteOps, a: u32, b: u32, c: u32) -> Instruction {
    ops.get_code() | (a << 8) | (b << 24) | (c << 16)
}

pub(crate) fn ins_ad(ops: ByteOps, a: u32, d: u32) -> Instruction {
    ops.get_code() | (a << 8) | (d << 16)
}

pub(crate) fn ins_aj(ops: ByteOps, a: u32, j: u32) -> Instruction {
    ins_ad(ops, a, j + ins_limit::J)
}

pub(crate) fn code_to_ops(instruction: u32) -> Option<ByteOps> {
    Some(match instruction & 0xff {
        0 => ByteOps::NOP,
        1 => ByteOps::ISLT,
        2 => ByteOps::ISGE,
        3 => ByteOps::ISLE,
        4 => ByteOps::ISGT,
        5 => ByteOps::ISEQV,
        6 => ByteOps::ISNEV,
        7 => ByteOps::ISEQS,
        8 => ByteOps::ISNES,
        9 => ByteOps::ISEQN,
        10 => ByteOps::ISNEN,
        11 => ByteOps::ISEQP,
        12 => ByteOps::ISNEP,
        13 => ByteOps::ISTC,
        14 => ByteOps::ISFC,
        15 => ByteOps::IST,
        16 => ByteOps::ISF,
        17 => ByteOps::ISTYPE,
        18 => ByteOps::ISNUM,
        19 => ByteOps::MOV,
        20 => ByteOps::NOT,
        21 => ByteOps::UNM,
        22 => ByteOps::LEN,
        23 => ByteOps::ADDVN,
        24 => ByteOps::SUBVN,
        25 => ByteOps::MULVN,
        26 => ByteOps::DIVVN,
        27 => ByteOps::MODVN,
        28 => ByteOps::ADDNV,
        29 => ByteOps::SUBNV,
        30 => ByteOps::MULNV,
        31 => ByteOps::DIVNV,
        32 => ByteOps::MODNV,
        33 => ByteOps::ADDVV,
        34 => ByteOps::SUBVV,
        35 => ByteOps::MULVV,
        36 => ByteOps::DIVVV,
        37 => ByteOps::MODVV,
        38 => ByteOps::POW,
        39 => ByteOps::CAT,
        40 => ByteOps::KSTR,
        41 => ByteOps::KCDATA,
        42 => ByteOps::KSHORT,
        43 => ByteOps::KNUM,
        44 => ByteOps::KPRI,
        45 => ByteOps::KNIL,
        46 => ByteOps::UGET,
        47 => ByteOps::USETV,
        48 => ByteOps::USETS,
        49 => ByteOps::USETN,
        50 => ByteOps::USETP,
        51 => ByteOps::UCLO,
        52 => ByteOps::FNEW,
        53 => ByteOps::TNEW,
        54 => ByteOps::TDUP,
        55 => ByteOps::GGET,
        56 => ByteOps::GSET,
        57 => ByteOps::TGETV,
        58 => ByteOps::TGETS,
        59 => ByteOps::TGETB,
        60 => ByteOps::TGETR,
        61 => ByteOps::TSETV,
        62 => ByteOps::TSETS,
        63 => ByteOps::TSETB,
        64 => ByteOps::TSETM,
        65 => ByteOps::TSETR,
        66 => ByteOps::CALLM,
        67 => ByteOps::CALL,
        68 => ByteOps::CALLMT,
        69 => ByteOps::CALLT,
        70 => ByteOps::ITERC,
        71 => ByteOps::ITERN,
        72 => ByteOps::VARG,
        73 => ByteOps::ISNEXT,
        74 => ByteOps::RETM,
        75 => ByteOps::RET,
        76 => ByteOps::RET0,
        77 => ByteOps::RET1,
        78 => ByteOps::FORI,
        79 => ByteOps::JFORI,
        80 => ByteOps::FORL,
        81 => ByteOps::IFORL,
        82 => ByteOps::JFORL,
        83 => ByteOps::ITERL,
        84 => ByteOps::IITERL,
        85 => ByteOps::JITERL,
        86 => ByteOps::LOOP,
        87 => ByteOps::ILOOP,
        88 => ByteOps::JLOOP,
        89 => ByteOps::JMP,
        90 => ByteOps::FUNCF,
        91 => ByteOps::IFUNCF,
        92 => ByteOps::JFUNCF,
        93 => ByteOps::FUNCV,
        94 => ByteOps::IFUNCV,
        95 => ByteOps::JFUNCV,
        96 => ByteOps::FUNCC,
        97 => ByteOps::FUNCCW,
        _ => {
            return None;
        }
    })
}

impl InstructionGetterSetter for Instruction {
    fn get_ops(&self) -> Option<ByteOps> {
        code_to_ops(*self)
    }

    fn get_a(&self) -> u32 {
        ((*self) >> 8) & 0xff
    }

    fn get_b(&self) -> u32 {
        (*self) >> 24
    }

    fn get_c(&self) -> u32 {
        ((*self) >> 16) & 0xff
    }

    fn get_d(&self) -> u32 {
        (*self) >> 16
    }

    fn get_j(&self) -> u32 {
        self.get_d() - ins_limit::J
    }

    fn set_ops(&mut self, ops: ByteOps) {
        *self = ((*self) & 0xffffff00) | (ops.get_code() & 0xff);
    }

    fn set_a(&mut self, a: u32) {
        *self = ((*self) & 0xffff00ff) | ((a & 0xff) << 8)
    }

    fn set_b(&mut self, b: u32) {
        *self = ((*self) & 0x00ffffff) | ((b & 0xff) << 24)
    }

    fn set_c(&mut self, c: u32) {
        *self = ((*self) & 0xff00ffff) | ((c & 0xff) << 16)
    }

    fn set_d(&mut self, d: u32) {
        *self = ((*self) & 0x0000ffff) | ((d & 0xffff) << 16)
    }

    fn set_j(&mut self, j: u32) {
        self.set_d(j + ins_limit::J)
    }
}
