use crate::{BytecodeInstruction, BytecodeLine};

pub struct BytecodeInstructionLine {
    instruction: BytecodeInstruction,
    line: BytecodeLine,
}