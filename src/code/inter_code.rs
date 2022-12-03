use std::fmt::Display;

#[derive(Clone, Copy)]
pub(crate) enum InterCode {
    LOADNIL(u8, u8),            // rA, rB; R[rA], R[rA+1], ... , R[rA + rB] := nil
    LOADTRUE(u8),               // rA; R[rA] := true
    LOADFALSE(u8),              // rA; R[rA] := false
    LFALSESKIP(u8),             // rA; R[rA] := false; pc++
    LOADK(u8, u32),             // rA, rB; R[rA] := K[rB]; notes: maybe use LOADKX
    LOADINT(u8, u32),           // rA, rB; R[rA] := rB
    LOADFLOAT(u8, u32),         // rA, rB; R[rA] := rB
    VARARG(u8, u8),             // rA, rC; R[rA], R[rA+1], ... , R[rA+rC-2] := vararg
    JMP(Option<i32>),           // sJ; pc += sJ
    MOVE(u8, u8),               // rA, rB; R[rA] := R[rB]
    CONCAT(u8, u8),             // rA, rB; R[rA] := R[rA] .. ... .. R[rA + rB - 1]
    ADDI(u8, u8, u8),           // rA, rB, rC: R[rA] := R[rB] + rC
    ADDK(u8, u8, u8),           // rA, rB, rC; R[rA] := R[rB] + K[rC]
    ADD(u8, u8, u8),            // rA, rB, rC; R[rA] := R[rB] + R[rC]
    SUBK(u8, u8, u8),           // rA, rB, rC; R[rA] := R[rB] - K[rC]
    SUB(u8, u8, u8),            // rA, rB, rC: R[rA] := R[rB] - R[rC]
    MULK(u8, u8, u8),           // rA, rB, rC; R[rA] := R[rB] * K[rC]
    MUL(u8, u8, u8),            // rA, rB, rC; R[rA] := R[rB] * R[rC]
    DIVK(u8, u8, u8),           // rA, rB, rC; R[rA] := R[rB] / K[rC]
    DIV(u8, u8, u8),            // rA, rB, rC; R[rA] := R[rB] / R[rC]
    IDIVK(u8, u8, u8),          // rA, rB, rC; R[rA] := R[rB] // K[rC]
    IDIV(u8, u8, u8),           // rA, rB, rC; R[rA] := R[rB] // R[rC]
    MODK(u8, u8, u8),           // rA, rB, rC; R[rA] := R[rB] % K[rC]
    MOD(u8, u8, u8),            // rA, rB, rC; R[rA] := R[rB] % R[rC]
    POWK(u8, u8, u8),           // rA, rB, rC; R[rA] := R[rB] ^ K[rC]
    POW(u8, u8, u8),            // rA, rB, rC; R[rA] := R[rB] ^ R[rC]
    SHLI(u8, u8, u8),           // rA, rB, rC: R[rA] := R[rB] << rC
    SHL(u8, u8, u8),            // rA, rB, rC: R[rA] := R[rB] << R[rC]
    SHRI(u8, u8, u8),           // rA, rB, rC: R[rA] := R[rB] >> rC
    SHR(u8, u8, u8),            // rA, rB, rC: R[rA] := R[rB] >> R[rC]
    BANDK(u8, u8, u8),          // rA, rB, rC: R[rA] := R[rB] >> R[rC]
    BAND(u8, u8, u8),           // rA, rB, rC: R[rA] := R[rB] >> R[rC]
    BORK(u8, u8, u8),           // rA, rB, rC: R[rA] := R[rB] >> R[rC]
    BOR(u8, u8, u8),            // rA, rB, rC: R[rA] := R[rB] >> R[rC]
    BXORK(u8, u8, u8),          // rA, rB, rC: R[rA] := R[rB] >> R[rC]
    BXOR(u8, u8, u8),           // rA, rB, rC: R[rA] := R[rB] >> R[rC]
    EQI(u8, u32, bool),         // rA, sB, k: if (R[rA] == sB) ~= k then pc++
    EQ(u8, u8, bool),           // rA, rB, k: if (R[rA] == R[rB]) ~= k then pc++
    EQK(u8, u8, bool),          // rA, rB, k: if (R[rA] == K[rB]) ~= k then pc++
    LT(u8, u8, bool),           // rA, rB, k: if (R[rA] < R[rB]) ~= k then pc++
    LE(u8, u8, bool),           // rA, rB, k: if (R[rA] <= R[rB]) ~= k then pc++
    LTI(u8, u32, bool),         // rA, sB, k: if (R[rA] < sB) ~= k then pc++
    LEI(u8, u32, bool),         // rA, sB, k: if (R[rA] <= sB) ~= k then pc++
    GTI(u8, u32, bool),         // rA, sB, k: if (R[rA] > sB) ~= k then pc++
    GEI(u8, u32, bool),         // rA, sB, k: if (R[rA] >= sB) ~= k then pc++
    TEST(u8, bool),             // rA, k; if (not R[rA] == k) then pc++
    TESTSET(u8, u8, bool),      // rA, rB, k; if (not R[rA] == k) then pc++ else R[rA] := R[rB]
    NOT(u8, u8),                // rA, rB; R[rA] := not R[rB]
    UNM(u8, u8),                // rA, rB; R[rA] := -R[rB]
    BNOT(u8, u8),               // rA, rB; R[rA] := ~R[rB]
    LEN(u8, u8),                // rA, rB; R[rA] := #R[rB]
    GETUPVAL(u8, u8),           // rA, rB; R[rA] := U[rB]
    SETUPVAL(u8, u8),           // rA, rB; U[rB] := R[rA]
    GETTABUP(u8, u8, u8),       // rA, rB, rC; R[rA] := U[rB][K[rC]]
    GETI(u8, u8, u8),           // rA, rB, rC; R[rA] := U[rB][rC]
    GETFIELD(u8, u8, u8),       // rA, rB, rC; R[rA] := R[rB][K[rC]]
    GETTABLE(u8, u8, u8),       // rA, rB, rC; R[rA] := R[rB][R[rC]]
    TBC(u8),                    // mark vA to be close
    CLOSE(u8),                  // close all upvalues >= R[rA]
    FORPREP(u8, i32),           // rA, Bx; pc += Bx + 1
    TFORPREP(u8, i32),          // rA, Bx; R[rA + 3]; pc += Bx
    TFORCALL(u8, u8),           // rA, rC; R[rA+4], ... , R[rA+3+rC] := R[rA](R[rA + 1], R[rA + 2])
    FORLOOP(u8, i32),           // rA, Bx; pc -= Bx
    TFORLOOP(u8, i32),          // rA, Bx; if R[rA+2] ~= nil then { R[rA] = R[rA+2]; pc -= Bx }
    SETTABUP(u8, u8, u8, bool), // rA, rB, rC, k; U[rA][K[rB]] := RK[rC]
    SETI(u8, u8, u8, bool),     // rA, rB, rC, k; R[rA][rB] := RK[rC]
    SETFIELD(u8, u8, u8, bool), // rA, rB, rC, k; R[rA][K[rB]] := RK[rC]
    SETTABLE(u8, u8, u8, bool), // rA, rB, rC, k; R[rA][R[rB]] := RK[rC]
    RETURN(u8, u8),             // rA, rB; return R[rA], ... , R[rA+rB-2]
    RETURN0,                    // ; return
    RETURN1(u8),                // rA; return R[rA]
    CLOSURE(u8, u32),           // rA, Bx; R[rA] = closure(KPROTO[Bx])
    VARARGPREP(u8),             // adjust vararg parameters
    CALL(u8, u8, u8), // rA, rB, rC; R[rA], ... , R[rA+rC-2] := R[rA](R[rA+1], ... , R[rA+rB-1])
}

impl Display for InterCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LOADNIL(ra, rb) => write!(f, "LOADNIL\t${} #{}", ra, rb),
            Self::LOADTRUE(ra) => write!(f, "LOADTRUE\t${}", ra),
            Self::LOADFALSE(ra) => write!(f, "LOADFALSE\t${}", ra),
            Self::LFALSESKIP(ra) => write!(f, "LFLASESKIP\t{}", ra),
            Self::LOADK(ra, rb) => write!(f, "LOADK\t${} &{}", ra, rb),
            Self::LOADINT(ra, rb) => write!(f, "LOADINT\t${} #{}", ra, rb),
            Self::LOADFLOAT(ra, rb) => write!(f, "LOADFLOAT\t${} #{}", ra, rb),
            Self::VARARG(ra, rc) => write!(f, "VARARG\t${} ${}", ra, rc),
            Self::JMP(sj) => match sj {
                Some(sj) => write!(f, "JMP\t#{}", sj),
                _ => write!(f, "JMP\t#0"),
            },
            Self::MOVE(ra, rb) => write!(f, "MOVE\t${} ${}", ra, rb),
            Self::CONCAT(ra, rb) => write!(f, "CONCAT\t${} ${}", ra, rb),
            Self::ADDI(ra, rb, rc) => write!(f, "ADDI\t${} ${} #{}", ra, rb, rc),
            Self::ADDK(ra, rb, rc) => write!(f, "ADDK\t${} ${} &{}", ra, rb, rc),
            Self::ADD(ra, rb, rc) => write!(f, "ADD\t${} ${} ${}", ra, rb, rc),
            Self::SUBK(ra, rb, rc) => write!(f, "SUBK\t${} ${} &{}", ra, rb, rc),
            Self::SUB(ra, rb, rc) => write!(f, "SUB\t${} ${} ${}", ra, rb, rc),
            Self::MULK(ra, rb, rc) => write!(f, "MULK\t${} ${} &{}", ra, rb, rc),
            Self::MUL(ra, rb, rc) => write!(f, "MUL\t${} ${} ${}", ra, rb, rc),
            Self::DIVK(ra, rb, rc) => write!(f, "DIVK\t${} ${} &{}", ra, rb, rc),
            Self::DIV(ra, rb, rc) => write!(f, "DIV\t${} ${} ${}", ra, rb, rc),
            Self::IDIVK(ra, rb, rc) => write!(f, "IDIVK\t${} ${} &{}", ra, rb, rc),
            Self::IDIV(ra, rb, rc) => write!(f, "IDIV\t${} ${} ${}", ra, rb, rc),
            Self::MODK(ra, rb, rc) => write!(f, "MODK\t${} ${} &{}", ra, rb, rc),
            Self::MOD(ra, rb, rc) => write!(f, "MOD\t${} ${} ${}", ra, rb, rc),
            Self::POWK(ra, rb, rc) => write!(f, "POWK\t${} ${} &{}", ra, rb, rc),
            Self::POW(ra, rb, rc) => write!(f, "POW\t${} ${} ${}", ra, rb, rc),
            Self::SHLI(ra, rb, rc) => write!(f, "SHLI\t${} ${} #{}", ra, rb, rc),
            Self::SHRI(ra, rb, rc) => write!(f, "SHRI\t${} ${} #{}", ra, rb, rc),
            Self::SHL(ra, rb, rc) => write!(f, "SHL\t${} ${} ${}", ra, rb, rc),
            Self::SHR(ra, rb, rc) => write!(f, "SHR\t${} ${} ${}", ra, rb, rc),
            Self::BANDK(ra, rb, rc) => write!(f, "BANDK\t${} ${} ${}", ra, rb, rc),
            Self::BAND(ra, rb, rc) => write!(f, "BAND\t${} ${} ${}", ra, rb, rc),
            Self::BORK(ra, rb, rc) => write!(f, "BORK\t${} ${} ${}", ra, rb, rc),
            Self::BOR(ra, rb, rc) => write!(f, "BOR\t${} ${} ${}", ra, rb, rc),
            Self::BXORK(ra, rb, rc) => write!(f, "BXORK\t${} ${} ${}", ra, rb, rc),
            Self::BXOR(ra, rb, rc) => write!(f, "BXOR\t${} ${} ${}", ra, rb, rc),
            Self::EQI(ra, rb, k) => write!(f, "EQI\t${} #{} ${}", ra, rb, k),
            Self::EQ(ra, rb, k) => write!(f, "EQ\t${} ${} {}", ra, rb, k),
            Self::EQK(ra, rb, k) => write!(f, "EQK\t${} &{} {}", ra, rb, k),
            Self::LT(ra, rb, k) => write!(f, "LT\t${} ${} {}", ra, rb, k),
            Self::LE(ra, rb, k) => write!(f, "LE\t${} ${} {}", ra, rb, k),
            Self::LTI(ra, rb, k) => write!(f, "LTI\t${} #{} {}", ra, rb, k),
            Self::LEI(ra, rb, k) => write!(f, "LEI\t${} #{} {}", ra, rb, k),
            Self::GTI(ra, rb, k) => write!(f, "GTI\t${} #{} {}", ra, rb, k),
            Self::GEI(ra, rb, k) => write!(f, "GEI\t${} #{} {}", ra, rb, k),
            Self::TEST(ra, k) => write!(f, "TEST\t${} {}", ra, k),
            Self::TESTSET(ra, rb, k) => write!(f, "TESTS\t${} ${} {}", ra, rb, k),
            Self::NOT(ra, rb) => write!(f, "NOT\t${} ${}", ra, rb),
            Self::UNM(ra, rb) => write!(f, "UNM\t${} ${}", ra, rb),
            Self::BNOT(ra, rb) => write!(f, "BNOT\t${} ${}", ra, rb),
            Self::LEN(ra, rb) => write!(f, "LEN\t${} ${}", ra, rb),
            Self::GETUPVAL(ra, rb) => write!(f, "GETUPV\t${} &{}", ra, rb),
            Self::SETUPVAL(ra, rb) => write!(f, "SETUPV\t${} ${}", ra, rb),
            Self::GETTABUP(ra, rb, rc) => write!(f, "GETTU\t${} &{} &{}", ra, rb, rc),
            Self::GETI(ra, rb, rc) => write!(f, "GETI\t${} &{} #{}", ra, rb, rc),
            Self::GETFIELD(ra, rb, rc) => write!(f, "GETF\t${} ${} &{}", ra, rb, rc),
            Self::GETTABLE(ra, rb, rc) => write!(f, "GETT\t${} ${} ${}", ra, rb, rc),
            Self::TBC(ra) => write!(f, "TBC\t${}", ra),
            Self::CLOSE(ra) => write!(f, "CLOSE\t${}", ra),
            Self::FORPREP(ra, rb) => write!(f, "FORP\t${} #{}", ra, rb),
            Self::TFORPREP(ra, rb) => write!(f, "TFORP\t${} #{}", ra, rb),
            Self::TFORCALL(ra, rc) => write!(f, "TFORC\t${} #{}", ra, rc),
            Self::FORLOOP(ra, rb) => write!(f, "FORL\t${} #{}", ra, rb),
            Self::TFORLOOP(ra, rb) => write!(f, "TFORL\t${} #{}", ra, rb),
            Self::SETTABUP(ra, rb, rc, k) => write!(f, "SETTU\t&{} &{} ${} {}", ra, rb, rc, k),
            Self::SETI(ra, rb, rc, k) => write!(f, "SETI\t${} #{} &${} {}", ra, rb, rc, k),
            Self::SETFIELD(ra, rb, rc, k) => write!(f, "SETF\t${} &{} &${} {}", ra, rb, rc, k),
            Self::SETTABLE(ra, rb, rc, k) => write!(f, "SETT\t${} ${} &${} {}", ra, rb, rc, k),
            Self::RETURN(ra, rb) => write!(f, "RET\t${} #{}", ra, rb),
            Self::RETURN1(ra) => write!(f, "RET1\t${}", ra),
            Self::RETURN0 => write!(f, "RET0"),
            Self::CLOSURE(ra, rb) => write!(f, "CLOSURE\t${} &{}", ra, rb),
            Self::VARARGPREP(ra) => write!(f, "VARARGP\t${}", ra),
            Self::CALL(ra, rb, rc) => write!(f, "CALL\t${} #{} #{}", ra, rb, rc),
        }
    }
}

impl InterCode {
    pub(crate) fn test_mode(&self) -> bool {
        match self {
            Self::EQI(_, _, _)
            | Self::EQ(_, _, _)
            | Self::EQK(_, _, _)
            | Self::LT(_, _, _)
            | Self::LE(_, _, _)
            | Self::LTI(_, _, _)
            | Self::LEI(_, _, _)
            | Self::GTI(_, _, _)
            | Self::GEI(_, _, _)
            | Self::TEST(_, _)
            | Self::TESTSET(_, _, _) => true,
            _ => false,
        }
    }
}
