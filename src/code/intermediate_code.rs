use std::fmt::Display;

#[derive(Clone, Copy)]
pub(crate) enum IntermediateCode {
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
    MULK(u8, u8, u8),   // rA, rB, rC; R[rA] := R[rB] * K[rC]
    MUL(u8, u8, u8),    // rA, rB, rC; R[rA] := R[rB] * R[rC]
}

impl Display for IntermediateCode {
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
            Self::MULK(ra, rb, rc) => write!(f, "MULK rA({}), rB({}), rC({})", *ra, *rb, *rc),
            Self::MUL(ra, rb, rc) => write!(f, "MUL rA({}), rB({}), rC({})", *ra, *rb, *rc),
        }
    }
}

impl IntermediateCode {
    pub(crate) fn set_ra(&mut self, ra: u8) {
        *self = match *self {
            Self::CONCAT(_, rb) => Self::CONCAT(ra, rb),
            Self::ADDI(_, rb, rc) => Self::ADDI(ra, rb, rc),
            Self::ADDK(_, rb, rc) => Self::ADDK(ra, rb, rc),
            Self::ADD(_, rb, rc) => Self::ADD(ra, rb, rc),
            Self::MULK(_, rb, rc) => Self::MULK(ra, rb, rc),
            Self::MUL(_, rb, rc) => Self::MUL(ra, rb, rc),
            _ => *self,
        };
    }
}
