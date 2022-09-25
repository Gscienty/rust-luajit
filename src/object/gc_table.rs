use crate::{TaggedValue, MemoryReference, GCHeader, GCReference};

pub struct Node {
    key: TaggedValue,
    value: TaggedValue,
    next: MemoryReference,
    freetop: MemoryReference,
}

pub struct GCTable {
    header: GCHeader,
    nomm: u8,
    colocation: i8,
    array: MemoryReference,
    gc_list: GCReference,
    metatable: GCReference,
    node: MemoryReference,
    array_size: u32,
    hash_mask: u32,
    freetop: MemoryReference,
}