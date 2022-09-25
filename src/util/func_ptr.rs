use std::{os::raw::{c_uchar, c_void}};

use crate::{LuaState, MemoryReference};
pub type LuaAlloc = fn(allocd: *const c_void, ptr: *const c_void, old_size: usize, new_size: usize) -> *const c_void;