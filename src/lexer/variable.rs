use crate::{GCReference, BytecodePosition};

pub struct VariableInfo {
    name: GCReference,
    start_pc: BytecodePosition,
    end_pc: BytecodePosition,
    slot: u8,
    info: u8,
}