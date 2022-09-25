use crate::{GCTable, LexState, LuaState, BytecodePosition, BytecodeRegister, BytecodeLine, BytecodeInstructionLine};

pub struct FuncScope {
    prev: &'static FuncScope,
    variable_start: u32,
    actvar_count: u8,
    flags: u8,
}

pub struct FuncState {
    constants_table: &'static GCTable,
    lexer_state: &'static LexState,
    lua_state: &'static LuaState,
    block: &'static FuncScope,
    prev: &'static FuncState,
    pc: BytecodePosition,
    last_target: BytecodePosition,
    jump_pc: BytecodePosition,
    free_register: BytecodeRegister,
    actvar_count: BytecodeRegister,
    number_count: BytecodeRegister,
    gc_count: BytecodeRegister,
    line_defined: BytecodeLine,
    stack_base: &'static BytecodeInstructionLine,
    stack_limit: BytecodePosition,
    vbase: u32,
    flags: u8,
    params_count: u8,
    frame_size: u8,
    upvalues_count: u8,
    var_map: [u16; 200],
    upvalue_map: [u16; 60],
    temporary_map: [u16; 60],
}
