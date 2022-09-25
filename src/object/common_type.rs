use std::{os::raw::{c_void, c_uchar}, ptr::{null, copy}};

use crate::LuaState;

pub type BytecodeInstruction = u32;
pub type BytecodePosition = u32;
pub type BytecodeRegister = u32;
pub type BytecodeLine = i32;

pub mod string_buffer_bitmarks {
    pub const EXT: u8 = 0x01;
    pub const COW: u8 = 0x02;
    pub const BORROW: u8 = 0x04;
}

mod string_buffer_constant {
    pub const MIN_BUFFER_LEN: usize = 64;
}

pub struct StringBuffer {
    flags: u8,

    read_ptr: *const c_void, // unsupport
    write_ptr: *const c_void,
    begin_ptr: *const c_void,
    end_ptr: *const c_void,

    lua_state: &'static LuaState,
}

impl StringBuffer {
    pub fn new(lua_state: &'static LuaState) -> Self {
        StringBuffer {
            flags: 0,

            read_ptr: null(),
            write_ptr: null(),
            begin_ptr: null(),
            end_ptr: null(),

            lua_state,
        }
    }

    pub fn get_read_ptr(&self) -> *const c_void {
        self.begin_ptr
    }

    pub fn get_read_eptr(&self) -> *const c_void {
        self.write_ptr
    }

    pub fn size(&self) -> usize {
        self.end_ptr as usize - self.begin_ptr as usize 
    }

    pub fn len(&self) -> usize {
        self.write_ptr as usize - self.begin_ptr as usize
    }

    pub fn left(&self) -> usize {
        self.end_ptr as usize - self.write_ptr as usize
    }

    pub fn xlen(&self) -> usize {
        self.write_ptr as usize - self.read_ptr as usize
    }

    pub fn xslack(&self) -> usize {
        self.read_ptr as usize - self.begin_ptr as usize
    }

    pub fn set_flag(&mut self, flag: u8) {
        self.flags |= flag;
    }

    pub fn clear_flag(&mut self, flag: u8) {
        self.flags |= self.flags & !flag
    }

    pub fn is_flag(&self, flag: u8) -> bool {
        (self.flags & flag) != 0
    }

    pub fn put(&mut self, chr: char) {
        let write_ptr = self.more(1);

        unsafe {
            *(write_ptr as *const c_uchar as *mut c_uchar) = chr as u8;
            self.write_ptr = write_ptr.add(1);
        }

    }

    fn reset(&mut self) {
        self.write_ptr = self.begin_ptr
    }

    fn more(&mut self, size: usize) -> *const c_void {
        if self.left() < size {
            let old_size = self.size();
            let len = self.len();
            let mut new_size = usize::max(string_buffer_constant::MIN_BUFFER_LEN, old_size);

            while new_size < size {
                new_size += new_size;
            }

            self.begin_ptr = if self.is_flag(string_buffer_bitmarks::COW) {
                self.clear_flag(string_buffer_bitmarks::COW);

                let begin_ptr = self.lua_state.realloc_memory(null(), old_size, new_size);

                unsafe {
                    copy(self.begin_ptr, begin_ptr as *mut c_void, old_size);
                }

                begin_ptr
            } else {
                self.lua_state.realloc_memory(self.begin_ptr, old_size, new_size)
            };

            unsafe {
                self.write_ptr = self.begin_ptr.add(len);
                self.end_ptr = self.begin_ptr.add(new_size);
            }
        }

        self.write_ptr
    }

}