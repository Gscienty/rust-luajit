use super::{GCHeader, TaggedValue, GCReference, MemoryReference};

pub struct GCUpvalue {
    header: GCHeader,
    closed: u8,
    immutable: u8,
    tagged_value: TaggedValue,

    prev: GCReference,
    next: GCReference,

    v: MemoryReference,

    dhash: u32,
}

impl GCUpvalue {
    pub fn get_prev(&self) -> &Self {
        self.prev.get_ref::<GCUpvalue>()
    }

    pub fn get_next(&self) -> &Self {
        self.next.get_ref::<GCUpvalue>()
    }

    pub fn get_value(&self) -> &TaggedValue {
        self.v.get_ref::<TaggedValue>()
    }
}