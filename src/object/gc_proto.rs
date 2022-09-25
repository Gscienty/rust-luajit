use super::{GCHeader, GCReference, MemoryReference, BytecodeLine};

pub mod prototype {
    pub const CHILD: u8 = 0x01;
    pub const VARARG: u8 = 0x02;
    pub const FFI: u8 = 0x04;
    pub const NOJIT: u8 = 0x08;
    pub const ILOOP: u8 = 0x10;
    pub const HASRETURN: u8 = 0x20;
    pub const FIXUPRETURN: u8 = 0x40;
    pub const CLCOUNT: u8 = 0x20;
    pub const CLCBITS: u8 = 3;
    pub const CLCPOLY: u8 = 3 * CLCOUNT;
    pub const UVLOCAL: u16 = 0x8000;
    pub const UVIMMUTABLE: u16 = 0x4000;
}

pub struct GCProto {
    header: GCHeader,
    params_count: u8,
    frame_size: u8,
    bytecode_count: u32,
    gc_list: GCReference,
    constant_array: MemoryReference,
    upvalue_list: MemoryReference,
    gc_count: u32,
    number_count: u32,
    colocated_array_count: u32,
    upvalue_count: u8,
    flags: u8,
    trace: u16,

    chunkname: GCReference,
    firstline: BytecodeLine,
    numline: BytecodeLine,
    line_info: MemoryReference,
    upvalue_info: MemoryReference,
    var_info: MemoryReference,
}

