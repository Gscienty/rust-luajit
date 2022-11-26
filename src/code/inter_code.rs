use std::fmt::Display;

#[derive(Clone, Copy)]
pub(crate) enum InterCode {
    LOADNIL(u8, u8),    // rA, rB; R[rA], R[rA+1], ... , R[rA + rB] := nil
    LOADTRUE(u8),       // rA; R[rA] := true
    LOADFALSE(u8),      // rA; R[rA] := false
    LOADK(u8, u32),     // rA, rB; R[rA] := K[rB]; notes: maybe use LOADKX
    LOADINT(u8, u32),   // rA, rB; R[rA] := rB
    LOADFLOAT(u8, u32), // rA, rB; R[rA] := rB
    VARARG(u8, u8),     // rA, rC; R[rA], R[rA+1], ... , R[rA+rC-2] := vararg
    JMP(u32),           // sJ; pc += sJ
    MOVE(u8, u8),       // rA, rB; R[rA] := R[rB]
    CONCAT(u8, u8),     // rA, rB; R[rA] := R[rA] .. ... .. R[rA + rB - 1]
    ADDI(u8, u8, u8),   // rA, rB, rC: R[rA] := R[rB] + rC
    ADDK(u8, u8, u8),   // rA, rB, rC; R[rA] := R[rB] + K[rC]
    ADD(u8, u8, u8),    // rA, rB, rC; R[rA] := R[rB] + R[rC]
    SUBK(u8, u8, u8),   // rA, rB, rC; R[rA] := R[rB] - K[rC]
    SUB(u8, u8, u8),    // rA, rB, rC: R[rA] := R[rB] - R[rC]
    MULK(u8, u8, u8),   // rA, rB, rC; R[rA] := R[rB] * K[rC]
    MUL(u8, u8, u8),    // rA, rB, rC; R[rA] := R[rB] * R[rC]
    DIVK(u8, u8, u8),   // rA, rB, rC; R[rA] := R[rB] / K[rC]
    DIV(u8, u8, u8),    // rA, rB, rC; R[rA] := R[rB] / R[rC]
    IDIVK(u8, u8, u8),  // rA, rB, rC; R[rA] := R[rB] // K[rC]
    IDIV(u8, u8, u8),   // rA, rB, rC; R[rA] := R[rB] // R[rC]
    MODK(u8, u8, u8),   // rA, rB, rC; R[rA] := R[rB] % K[rC]
    MOD(u8, u8, u8),    // rA, rB, rC; R[rA] := R[rB] % R[rC]
    POWK(u8, u8, u8),   // rA, rB, rC; R[rA] := R[rB] ^ K[rC]
    POW(u8, u8, u8),    // rA, rB, rC; R[rA] := R[rB] ^ R[rC]
    SHLI(u8, u8, u8),   // rA, rB, rC: R[rA] := R[rB] << rC
    SHL(u8, u8, u8),    // rA, rB, rC: R[rA] := R[rB] << R[rC]
    SHRI(u8, u8, u8),   // rA, rB, rC: R[rA] := R[rB] >> rC
    SHR(u8, u8, u8),    // rA, rB, rC: R[rA] := R[rB] >> R[rC]
}

impl Display for InterCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LOADNIL(ra, rb) => write!(f, "LOADNIL rA({}), rB({})", *ra, *rb),
            Self::LOADTRUE(ra) => write!(f, "LOADTRUE rA({})", *ra),
            Self::LOADFALSE(ra) => write!(f, "LOADFALSE rA({})", *ra),
            Self::LOADK(ra, rb) => write!(f, "LOADK rA({}), rB({})", *ra, *rb),
            Self::LOADINT(ra, rb) => write!(f, "LOADINT rA({}), rB({})", *ra, *rb),
            Self::LOADFLOAT(ra, rb) => write!(f, "LOADFLOAT rA({}), rB({})", *ra, *rb),
            Self::VARARG(ra, rc) => write!(f, "VARARG rA({}), rC({})", *ra, *rc),
            Self::JMP(sj) => write!(f, "JMP sJ({})", *sj),
            Self::MOVE(ra, rb) => write!(f, "MOVE rA({}), rB({})", *ra, *rb),
            Self::CONCAT(ra, rb) => write!(f, "CONCAT rA({}), rB({})", *ra, *rb),
            Self::ADDI(ra, rb, rc) => write!(f, "ADDI rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::ADDK(ra, rb, rc) => write!(f, "ADDK rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::ADD(ra, rb, rc) => write!(f, "ADD rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::SUBK(ra, rb, rc) => write!(f, "SUBK rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::SUB(ra, rb, rc) => write!(f, "SUB rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::MULK(ra, rb, rc) => write!(f, "MULK rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::MUL(ra, rb, rc) => write!(f, "MUL rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::DIVK(ra, rb, rc) => write!(f, "DIVK rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::DIV(ra, rb, rc) => write!(f, "DIV rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::IDIVK(ra, rb, rc) => write!(f, "IDIVK rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::IDIV(ra, rb, rc) => write!(f, "IDIV rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::MODK(ra, rb, rc) => write!(f, "MODK rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::MOD(ra, rb, rc) => write!(f, "MOD rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::POWK(ra, rb, rc) => write!(f, "POWK rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::POW(ra, rb, rc) => write!(f, "POW rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::SHLI(ra, rb, rc) => write!(f, "SHLI rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::SHRI(ra, rb, rc) => write!(f, "SHRI rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::SHL(ra, rb, rc) => write!(f, "SHL rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::SHR(ra, rb, rc) => write!(f, "SHR rA({}), rB({}), rC({})", *ra, *rb, *rc),
        }
    }
}
