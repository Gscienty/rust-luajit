use super::GCHeader;

#[repr(C)]
pub struct GCRustData {
    header: GCHeader,
    rust_typeid: u16,
}

#[repr(C)]
pub struct GCRustDataVariable {
    offset: u16,
    extra: u16,
    len: u32,
}