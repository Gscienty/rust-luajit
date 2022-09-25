use std::{os::raw::c_void, ptr::null};

use crate::{LuaAlloc, GCState};

pub struct GlobalState {
    alloc_func: LuaAlloc,
    allocd_ptr: *const c_void,
    gc_state: GCState,
}

impl GlobalState {
    pub fn new(alloc_func: LuaAlloc) -> Self {
        GlobalState {
            alloc_func,
            allocd_ptr: null(),
            gc_state: GCState::new(),
        }
    }

    /// 申请内存空间
    pub fn alloc(&self, ptr: *const c_void, old_size: usize, new_size: usize) -> *const c_void {
        (self.alloc_func)(self.allocd_ptr, ptr, old_size, new_size)
    }

    // 获取 GC 状态
    pub fn get_gc_state_mut(&mut self) -> &mut GCState {
        &mut self.gc_state
    }
    pub fn get_gc_state(&self) -> &GCState {
        &self.gc_state
    }
}