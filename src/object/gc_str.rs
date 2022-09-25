use std::{mem::size_of, alloc::{Layout, alloc_zeroed, dealloc}, slice};

use super::GCHeader;

// String object header, String payload follow
#[repr(C)]
pub struct GCStr {
    header: GCHeader,
    reversed: u8,
    hashalg: u8,
    sid: u32,
    hash: u32,
    len: u32,
}

impl GCStr {
    pub fn alloc_empty(len: u32) -> &'static Self {
        let layout = Layout::from_size_align(size_of::<GCStr>() + len as usize, 1).unwrap();
        let ptr = unsafe {
            alloc_zeroed(layout)
        };

        let gcs = unsafe { &mut *(ptr as *mut GCStr) };
        *gcs = GCStr {
            header: GCHeader::new(),
            reversed: 0,
            hashalg: 0,
            sid: 0,
            hash: 0,
            len,
        };

        gcs
    }

    pub fn get_data(&self) -> &[u8] {
        let ptr = ((self as *const GCStr as usize) + size_of::<GCStr>()) as *const u8;

        unsafe { slice::from_raw_parts(ptr, self.len as usize) }
    }

    pub fn get_data_mut(&self) -> &mut [u8] {
        let ptr = ((self as *const GCStr as usize) + size_of::<GCStr>()) as *mut u8;

        unsafe { slice::from_raw_parts_mut(ptr, self.len as usize) }
    }
}

impl Drop for GCStr {
   fn drop(&mut self) {
        let layout = Layout::from_size_align(size_of::<GCHeader>() + self.len as usize, 1).unwrap();
        let ptr = self as *const GCStr as usize;

        unsafe { dealloc(ptr as *mut u8, layout) }
   } 
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gcstr_set() {
        let gcs = GCStr::alloc_empty(3);
        let buffer = gcs.get_data_mut();

        buffer[0] = 'h' as u8;
        buffer[1] = 'i' as u8;
        buffer[2] = '\0' as u8;

        let actual = String::from_utf8(gcs.get_data().to_vec()).unwrap();

        assert_eq!(actual, String::from("hi\0"));

        drop(gcs);
    }
}