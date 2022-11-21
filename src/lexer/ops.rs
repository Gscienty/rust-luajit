#[derive(Clone, Copy)]
pub(crate) enum ByteOps {
    // comparison ops
    ISLT,
    ISGE,
    ISLE,
    ISGT,

    ISEQV,
    ISNEV,
    ISEQS,
    ISNES,
    ISEQN,
    ISNEN,
    ISEQP,
    ISNEP,

    // unary test and copy ops
    ISTC,
    ISFC,
    IST,
    ISF,
    ISTYPE,
    ISNUM,

    // unary ops
    MOV,
    NOT,
    UNM,
    LEN,

    // binary ops
    ADDVN,
    SUBVN,
    MULVN,
    DIVVN,
    MODVN,

    ADDNV,
    SUBNV,
    MULNV,
    DIVNV,
    MODNV,

    ADDVV,
    SUBVV,
    MULVV,
    DIVVV,
    MODVV,

    POW,
    CAT,

    // constant ops
    KSTR,
    KCDATA,
    KSHORT,
    KNUM,
    KPRI,
    KNIL,

    // upvalue and function ops
    UGET,
    USETV,
    USETS,
    USETN,
    USETP,
    UCLO,
    FNEW,

    // table ops
    TNEW,
    TDUP,
    GGET,
    GSET,
    TGETV,
    TGETS,
    TGETB,
    TGETR,
    TSETV,
    TSETS,
    TSETB,
    TSETM,
    TSETR,

    // call and vararg handling
    CALLM,
    CALL,
    CALLMT,
    CALLT,
    ITERC,
    ITERN,
    VARG,
    ISNEXT,

    // return
    RETM,
    RET,
    RET0,
    RET1,

    // loops and branches
    FORI,
    JFORI,

    FORL,
    IFORL,
    JFORL,

    ITERL,
    IITERL,
    JITERL,

    LOOP,
    ILOOP,
    JLOOP,

    JMP,

    // function headers
    FUNCF,
    IFUNCF,
    JFUNCF,
    FUNCV,
    IFUNCV,
    JFUNCV,
    FUNCC,
    FUNCCW,

    NOP,
}

impl ByteOps {
    pub(crate) fn get_code(&self) -> u32 {
        match self {
            Self::NOP => 0,
            Self::ISLT => 1,
            Self::ISGE => 2,
            Self::ISLE => 3,
            Self::ISGT => 4,
            Self::ISEQV => 5,
            Self::ISNEV => 6,
            Self::ISEQS => 7,
            Self::ISNES => 8,
            Self::ISEQN => 9,
            Self::ISNEN => 10,
            Self::ISEQP => 11,
            Self::ISNEP => 12,
            Self::ISTC => 13,
            Self::ISFC => 14,
            Self::IST => 15,
            Self::ISF => 16,
            Self::ISTYPE => 17,
            Self::ISNUM => 18,
            Self::MOV => 19,
            Self::NOT => 20,
            Self::UNM => 21,
            Self::LEN => 22,
            Self::ADDVN => 23,
            Self::SUBVN => 24,
            Self::MULVN => 25,
            Self::DIVVN => 26,
            Self::MODVN => 27,
            Self::ADDNV => 28,
            Self::SUBNV => 29,
            Self::MULNV => 30,
            Self::DIVNV => 31,
            Self::MODNV => 32,
            Self::ADDVV => 33,
            Self::SUBVV => 34,
            Self::MULVV => 35,
            Self::DIVVV => 36,
            Self::MODVV => 37,
            Self::POW => 38,
            Self::CAT => 39,
            Self::KSTR => 40,
            Self::KCDATA => 41,
            Self::KSHORT => 42,
            Self::KNUM => 43,
            Self::KPRI => 44,
            Self::KNIL => 45,
            Self::UGET => 46,
            Self::USETV => 47,
            Self::USETS => 48,
            Self::USETN => 49,
            Self::USETP => 50,
            Self::UCLO => 51,
            Self::FNEW => 52,
            Self::TNEW => 53,
            Self::TDUP => 54,
            Self::GGET => 55,
            Self::GSET => 56,
            Self::TGETV => 57,
            Self::TGETS => 58,
            Self::TGETB => 59,
            Self::TGETR => 60,
            Self::TSETV => 61,
            Self::TSETS => 62,
            Self::TSETB => 63,
            Self::TSETM => 64,
            Self::TSETR => 65,
            Self::CALLM => 66,
            Self::CALL => 67,
            Self::CALLMT => 68,
            Self::CALLT => 69,
            Self::ITERC => 70,
            Self::ITERN => 71,
            Self::VARG => 72,
            Self::ISNEXT => 73,
            Self::RETM => 74,
            Self::RET => 75,
            Self::RET0 => 76,
            Self::RET1 => 77,
            Self::FORI => 78,
            Self::JFORI => 79,
            Self::FORL => 80,
            Self::IFORL => 81,
            Self::JFORL => 82,
            Self::ITERL => 83,
            Self::IITERL => 84,
            Self::JITERL => 85,
            Self::LOOP => 86,
            Self::ILOOP => 87,
            Self::JLOOP => 88,
            Self::JMP => 89,
            Self::FUNCF => 90,
            Self::IFUNCF => 91,
            Self::JFUNCF => 92,
            Self::FUNCV => 93,
            Self::IFUNCV => 94,
            Self::JFUNCV => 95,
            Self::FUNCC => 96,
            Self::FUNCCW => 97,
        }
    }

    pub(crate) fn invert_conparison(&self) -> ByteOps {
        match self {
            Self::ISLT => Self::ISGE,
            Self::ISGE => Self::ISLT,

            Self::ISLE => Self::ISGT,
            Self::ISGT => Self::ISLE,

            Self::ISEQV => Self::ISNEV,
            Self::ISNEV => Self::ISEQV,

            Self::ISEQS => Self::ISNES,
            Self::ISNES => Self::ISEQS,

            Self::ISEQN => Self::ISNEN,
            Self::ISNEN => Self::ISEQN,

            Self::ISEQP => Self::ISNEP,
            Self::ISNEP => Self::ISEQP,

            _ => Self::NOP,
        }
    }

    pub(crate) fn no_patch(&self) -> ByteOps {
        match self {
            Self::ISTC => Self::IST,
            Self::ISFC => Self::ISF,
            _ => Self::NOP,
        }
    }
}
