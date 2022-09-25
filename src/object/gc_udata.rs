use std::{alloc::{Layout, alloc_zeroed}, mem::size_of};

use super::{GCHeader, GCReference};

pub enum UserdataType {
    USERDATA,
    IOFile,
    FFICLIB,
    BUFFER,
}

#[repr(C)]
pub struct GCUserdata {
    header: GCHeader,
    ud_type: UserdataType,
    env: GCReference,
    len: u32,
    metatable: GCReference,
}

impl GCUserdata {
    pub fn alloc_empty(ud_type: UserdataType, len: u32) -> &'static Self {
        let layout = Layout::from_size_align(size_of::<GCUserdata>() + len as usize, 1).unwrap();
        let ptr = unsafe {
            alloc_zeroed(layout)
        };

        let gcu = unsafe {
            &mut *(ptr as *mut GCUserdata) 
        };

        *gcu = GCUserdata {
            header: GCHeader::new(),
            ud_type,
            env: GCReference::new(),
            len,
            metatable: GCReference::new(),
        };

        gcu
    }
}