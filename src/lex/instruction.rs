use crate::bytecode::ByteInstruction;

pub(crate) struct ByteCodeInstructionLine {
    pub(crate) instruction: ByteInstruction,
    pub(crate) line: u32,
}

impl ByteCodeInstructionLine {
    pub(crate) fn new(instruction: ByteInstruction, line: u32) -> Self {
        ByteCodeInstructionLine {
            instruction,
            line,
        }
    }
}