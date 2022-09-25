use super::{GCHeader, GCReference, MemoryReference, LuaRustFunc, TaggedValue};

pub enum FIDType {
    Lua,
    Rust,
}

pub struct GCFuncHeader {
    header: GCHeader,
    ffid: FIDType,
    upvalues_count: u8,
    env: GCReference,
    gclist: GCReference,
    pc: MemoryReference,
}

pub struct  GCFuncRust {
    header: GCFuncHeader,
    func: LuaRustFunc,
    upvalue: [TaggedValue],
}

pub struct GCFuncLua {
    header: GCFuncHeader,
    upvalue_ptr: [GCReference],
}