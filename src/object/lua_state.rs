use std::{ptr::null, os::raw::c_void};

use crate::{GCHeader, GlobalState, MemoryReference};

pub struct LuaState {
    header: GCHeader,
    mref: MemoryReference,
}

impl LuaState {
    pub fn new(global_state: &GlobalState) -> Self {
        let mut mref = MemoryReference::new();
        mref.set_ref(global_state);
        
        LuaState {
            header: GCHeader::new(),
            mref,
        }
    }

    pub fn realloc_memory(&self, ptr: *const c_void, old_size: usize, new_size: usize) -> *const c_void {
        let global_state = self.mref.get_ref_mut::<GlobalState>();
        let ptr = global_state.alloc(ptr, old_size, new_size);

        if ptr.eq(&null()) && new_size > 0 {
            null()
        } else {
            let gc = global_state.get_gc_state_mut();
            gc.set_total(gc.get_total() - old_size + new_size);

            ptr
        }
    }
}