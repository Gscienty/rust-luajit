#[derive(Clone, Copy)]
pub(crate) enum OpCode {
    MOVE,       // A B      | R[A] := R[B]
    LOADI,      // A sBx    | R[A] := sBx (int)
    LOADF,      // A sBx    | R[A] := sBx (number)
    LOADK,      // A Bx     | R[A] := K[Bx]
    LOADKX,     // A Bx     | R[A] := K[extra arg]
    LOADFALSE,  // A        | R[A] := false
    LFALSESKIP, // A        | R[A] := false; pc++
    LOADTRUE,   // A        | R[A] := true
    LOADNIL,    // A B      | R[A], R[A+1], ..., R[A+B] := nil
    GETUPVAL,   // A B      | R[A] := Upval[B]
    SETUPVAL,   // A B      | Upval[B] := R[A]

    GETTABUP, // A B C      | R[A] := Upval[B][K[C]:string]
    GETTABLE, // A B C      | R[A] := R[B][R[C]]
    GETI,     // A B C      | R[A] := R[B][C]
    GETFIELD, // A B C      | R[A] := R[B][K[C]:string]

    SETTABUP, // A B C      | Upval[A][K[B]:string] := RK(C)
    SETTABLE, // A B C      | R[A][R[B]] := RK(C)
    SETI,     // A B C      | R[A][B] := RK(C)
    SETFIELD, // A B C      | R[A][K[B]:string] = RK(C)

    NEWTABLE, // A B C      | R[A] := {}

    SELF, // A B C          | R[A+1] := R[B]; R[A] := R[B][RK(C):string]

    ADDI, // A B sC         | R[A] := R[B] + sC

    ADDK,  // A B C         | R[A] := R[B] + K[C]:number
    SUBK,  // A B C         | R[A] := R[B] - K[C]:number
    MULK,  // A B C         | R[A] := R[B] * K[C]:number
    MODK,  // A B C         | R[A] := R[B] % K[C]:number
    POWK,  // A B C         | R[A] := R[B] ^ K[C]:number
    DIVK,  // A B C         | R[A] := R[B] / K[C]:number
    IDIVK, // A B C         | R[A] := R[B] // K[C]:number

    BANDK, // A B C         | R[A] := R[B] & K[C]:int
    BORK,  // A B C         | R[A] := R[B] | K[C]:int
    BXORK, // A B C         | R[A] := R[B] ~ K[C]:int

    SHRI, // A B sC         | R[A] := R[B] >> sC
    SHLI, // A B sC         | R[A] := sC << R[B]

    ADD,  // A B C          | R[A] := R[B] + R[C]
    SUB,  // A B C          | R[A] := R[B] - R[C]
    MUL,  // A B C          | R[A] := R[B] * R[C]
    MOD,  // A B C          | R[A] := R[B] % R[C]
    POW,  // A B C          | R[A] := R[B] ^ R[C]
    DIV,  // A B C          | R[A] := R[B] / R[C]
    IDIV, // A B C          | R[A] := R[B] // R[C]

    BAND, // A B C          | R[A] := R[B] & R[C]
    BOR,  // A B C          | R[A] := R[B] | R[C]
    BXOR, // A B C          | R[A] := R[B] ~ R[C]
    SHL,  // A B C          | R[A] := R[B] << R[C]
    SHR,  // A B C          | R[A] := R[B] >> R[C]

    MMBIN,  // A B C
    MMBINI, // A sB
    MMBINK, // A B C

    UNM,  // A B            | R[A] := -R[B]
    BNOT, // A B            | R[A] := ~R[B]
    NOT,  // A B            | R[A] := not R[B]
    LEN,  // A B            | R[A] := #R[B]

    CONCAT, // A B          | R[A] := R[A].. ... ..R[A+B-1]

    CLOSE, // A
    TBC,   // A
    JMP,   // sJ            | pc := sJ
    EQ,    // A B           | if ((R[A] == R[B]) ~= k) then pc++
    LT,    // A B           | if ((R[A] < R[B]) ~= k) then pc++
    LE,    // A B           | if ((R[A] <= R[B]) ~= k) then pc++

    EQK, // A B             | if ((R[A] <= R[B]) ~= k) then pc++
    EQI, // A sB            | if ((R[A] == sB) ~= k) then pc++
    LTI, // A sB            | if ((R[A] < sB) ~= k) then pc++
    LEI, // A sB            | if ((R[A] <= sB) ~= k) then pc++
    GTI, // A sB            | if ((R[A] > sB) ~= k) then pc++
    GEI, // A sB            | if ((R[A] >= sB) ~= k) then pc++

    TEST,    // A           | if (not R[A] == k) then pc++
    TESTSET, // A B         | if (not R[B] == k) then pc++ else R[A] := R[B]

    CALL,     // A B C      | R[A], ..., R[A+C-2] := R[A](R[A+1], ..., R[A+B-1])
    TAILCALL, // A B C      | return R[A](R[A+1], ..., R[A+B-1])

    RETURN,  // A B C       | return R[A], ..., R[A+B-2]
    RETURN0, //
    RETURN1, // A           | return R[A]

    FORLOOP, // A Bx        | update counters; if loop continues then pc -= Bx
    FORPREP, // A Bx        | if not to run then pc += Bx+1

    TFORPREP, // A Bx       | create upval for R[A+3]; pc += Bx
    TFORCALL, // A C        | R[A+4], ..., R[A+3+C] := R[A](R[A+1], R[A+2])
    TFORLOOP, // A Bx       | if R[A+2] ~= nil then { R[A]=R[A+2]; pc -= Bx }

    SETLIST, // A B C       | R[A][C+i] := R[A+i], 1 <= i <= B

    CLOSURE, // A Bx        | R[A] := closure(KPROTO[Bx])

    VARARG, // A C          | R[A], R[A+1], ..., R[A+C-2] := vararg

    VARARGPREP, // A

    EXTRARG, // Ax
}

pub(crate) enum OpMode {
    ABCMode,
    ABxMode,
    AsBxMode,
    AxMode,
    JMode,
}

impl OpCode {
    pub(crate) fn mode(&self) -> OpMode {
        match self {
            Self::MOVE => OpMode::ABCMode,
            Self::LOADI => OpMode::AsBxMode,
            Self::LOADF => OpMode::AsBxMode,
            Self::LOADK => OpMode::ABxMode,
            Self::LOADKX => OpMode::ABxMode,
            Self::LOADFALSE => OpMode::ABCMode,
            Self::LFALSESKIP => OpMode::ABCMode,
            Self::LOADTRUE => OpMode::ABCMode,
            Self::LOADNIL => OpMode::ABCMode,
            Self::GETUPVAL => OpMode::ABCMode,
            Self::SETUPVAL => OpMode::ABCMode,
            Self::GETTABUP => OpMode::ABCMode,

            Self::GETTABLE => OpMode::ABCMode,
            Self::GETI => OpMode::ABCMode,
            Self::GETFIELD => OpMode::ABCMode,

            Self::SETTABUP => OpMode::ABCMode,
            Self::SETTABLE => OpMode::ABCMode,
            Self::SETI => OpMode::ABCMode,
            Self::SETFIELD => OpMode::ABCMode,

            Self::NEWTABLE => OpMode::ABCMode,

            Self::SELF => OpMode::ABCMode,

            Self::ADDI => OpMode::ABCMode,

            Self::ADDK => OpMode::ABCMode,
            Self::SUBK => OpMode::ABCMode,
            Self::MULK => OpMode::ABCMode,
            Self::MODK => OpMode::ABCMode,
            Self::POWK => OpMode::ABCMode,
            Self::DIVK => OpMode::ABCMode,
            Self::IDIVK => OpMode::ABCMode,

            Self::BANDK => OpMode::ABCMode,
            Self::BORK => OpMode::ABCMode,
            Self::BXORK => OpMode::ABCMode,

            Self::SHRI => OpMode::ABCMode,
            Self::SHLI => OpMode::ABCMode,

            Self::ADD => OpMode::ABCMode,
            Self::SUB => OpMode::ABCMode,
            Self::MUL => OpMode::ABCMode,
            Self::MOD => OpMode::ABCMode,
            Self::POW => OpMode::ABCMode,
            Self::DIV => OpMode::ABCMode,
            Self::IDIV => OpMode::ABCMode,

            Self::BAND => OpMode::ABCMode,
            Self::BOR => OpMode::ABCMode,
            Self::BXOR => OpMode::ABCMode,
            Self::SHL => OpMode::ABCMode,
            Self::SHR => OpMode::ABCMode,

            Self::MMBIN => OpMode::ABCMode,
            Self::MMBINI => OpMode::ABCMode,
            Self::MMBINK => OpMode::ABCMode,

            Self::UNM => OpMode::ABCMode,
            Self::BNOT => OpMode::ABCMode,
            Self::NOT => OpMode::ABCMode,
            Self::LEN => OpMode::ABCMode,

            Self::CONCAT => OpMode::ABCMode,

            Self::CLOSE => OpMode::ABCMode,
            Self::TBC => OpMode::ABCMode,
            Self::JMP => OpMode::JMode,
            Self::EQ => OpMode::ABCMode,
            Self::LT => OpMode::ABCMode,
            Self::LE => OpMode::ABCMode,

            Self::EQK => OpMode::ABCMode,
            Self::EQI => OpMode::ABCMode,
            Self::LTI => OpMode::ABCMode,
            Self::LEI => OpMode::ABCMode,
            Self::GTI => OpMode::ABCMode,
            Self::GEI => OpMode::ABCMode,

            Self::TEST => OpMode::ABCMode,
            Self::TESTSET => OpMode::ABCMode,

            Self::CALL => OpMode::ABCMode,
            Self::TAILCALL => OpMode::ABCMode,

            Self::RETURN => OpMode::ABCMode,
            Self::RETURN0 => OpMode::ABCMode,
            Self::RETURN1 => OpMode::ABCMode,

            Self::FORLOOP => OpMode::ABxMode,
            Self::FORPREP => OpMode::ABxMode,

            Self::TFORPREP => OpMode::ABxMode,
            Self::TFORCALL => OpMode::ABCMode,
            Self::TFORLOOP => OpMode::ABxMode,

            Self::SETLIST => OpMode::ABCMode,

            Self::CLOSURE => OpMode::ABxMode,

            Self::VARARG => OpMode::ABCMode,

            Self::VARARGPREP => OpMode::ABCMode,

            Self::EXTRARG => OpMode::AxMode,
        }
    }

    pub(crate) fn bits(&self) -> u32 {
        match self {
            Self::MOVE => 0,
            Self::LOADI => 1,
            Self::LOADF => 2,
            Self::LOADK => 3,
            Self::LOADKX => 4,
            Self::LOADFALSE => 5,
            Self::LFALSESKIP => 6,
            Self::LOADTRUE => 7,
            Self::LOADNIL => 8,
            Self::GETUPVAL => 9,
            Self::SETUPVAL => 10,
            Self::GETTABUP => 11,

            Self::GETTABLE => 13,
            Self::GETI => 14,
            Self::GETFIELD => 15,

            Self::SETTABUP => 17,
            Self::SETTABLE => 18,
            Self::SETI => 19,
            Self::SETFIELD => 20,

            Self::NEWTABLE => 22,

            Self::SELF => 24,

            Self::ADDI => 26,

            Self::ADDK => 28,
            Self::SUBK => 29,
            Self::MULK => 30,
            Self::MODK => 31,
            Self::POWK => 32,
            Self::DIVK => 33,
            Self::IDIVK => 34,

            Self::BANDK => 36,
            Self::BORK => 37,
            Self::BXORK => 38,

            Self::SHRI => 40,
            Self::SHLI => 41,

            Self::ADD => 43,
            Self::SUB => 44,
            Self::MUL => 45,
            Self::MOD => 46,
            Self::POW => 47,
            Self::DIV => 48,
            Self::IDIV => 49,

            Self::BAND => 51,
            Self::BOR => 52,
            Self::BXOR => 53,
            Self::SHL => 54,
            Self::SHR => 55,

            Self::MMBIN => 57,
            Self::MMBINI => 58,
            Self::MMBINK => 59,

            Self::UNM => 61,
            Self::BNOT => 62,
            Self::NOT => 63,
            Self::LEN => 64,

            Self::CONCAT => 66,

            Self::CLOSE => 68,
            Self::TBC => 69,
            Self::JMP => 70,
            Self::EQ => 71,
            Self::LT => 72,
            Self::LE => 73,

            Self::EQK => 75,
            Self::EQI => 76,
            Self::LTI => 77,
            Self::LEI => 78,
            Self::GTI => 79,
            Self::GEI => 80,

            Self::TEST => 82,
            Self::TESTSET => 83,

            Self::CALL => 85,
            Self::TAILCALL => 86,

            Self::RETURN => 88,
            Self::RETURN0 => 89,
            Self::RETURN1 => 90,

            Self::FORLOOP => 92,
            Self::FORPREP => 93,

            Self::TFORPREP => 95,
            Self::TFORCALL => 96,
            Self::TFORLOOP => 97,

            Self::SETLIST => 99,

            Self::CLOSURE => 101,

            Self::VARARG => 103,

            Self::VARARGPREP => 105,

            Self::EXTRARG => 107,
        }
    }

    pub(crate) fn mm_mode(&self) -> bool {
        match self {
            Self::MMBIN | Self::MMBINI | Self::MMBINK => true,
            _ => false,
        }
    }

    pub(crate) fn ot_mode(&self) -> bool {
        match self {
            Self::CALL | Self::TAILCALL | Self::VARARG => true,
            _ => false,
        }
    }

    pub(crate) fn it_mode(&self) -> bool {
        match self {
            Self::CALL | Self::TAILCALL | Self::RETURN | Self::SETLIST | Self::VARARGPREP => true,
            _ => false,
        }
    }

    pub(crate) fn t_mode(&self) -> bool {
        match self {
            Self::EQ
            | Self::LT
            | Self::LE
            | Self::EQK
            | Self::EQI
            | Self::LTI
            | Self::LEI
            | Self::GTI
            | Self::GEI
            | Self::TEST
            | Self::TESTSET => true,
            _ => false,
        }
    }

    pub(crate) fn a_mode(&self) -> bool {
        match self {
            Self::SETUPVAL
            | Self::SETTABUP
            | Self::SETTABLE
            | Self::SETI
            | Self::SETFIELD
            | Self::MMBIN
            | Self::MMBINI
            | Self::MMBINK
            | Self::CLOSE
            | Self::TBC
            | Self::JMP
            | Self::EQ
            | Self::LT
            | Self::LE
            | Self::EQK
            | Self::EQI
            | Self::LTI
            | Self::LEI
            | Self::GTI
            | Self::GEI
            | Self::TEST
            | Self::RETURN
            | Self::RETURN0
            | Self::RETURN1
            | Self::TFORPREP
            | Self::TFORCALL
            | Self::SETLIST
            | Self::EXTRARG => false,
            _ => true,
        }
    }
}
