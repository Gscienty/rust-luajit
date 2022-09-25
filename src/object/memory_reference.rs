#[repr(C)]
pub struct MemoryReference {
    ptr: isize,
}

impl MemoryReference{
    pub fn new() -> Self {
        MemoryReference { ptr: 0 }
    }

    pub fn set_ref<T>(&mut self, r: &T) {
        let ptr = r as *const T as isize;

        self.ptr = ptr;
    }

    pub fn set_mref(&mut self, r: &MemoryReference) {
        self.ptr = r.get_ptr();
    }

    pub fn get_ref<T>(&self) -> &T {
        unsafe {&*(self.ptr as *const T)}
    }

    pub fn get_ref_mut<T>(&self) -> &mut T {
        unsafe{&mut *(self.ptr as *mut T)}
    }

    pub fn get_ptr(&self) -> isize {
        self.ptr
    }

    pub fn set_ptr(&mut self, ptr: isize) {
        self.ptr = ptr
    }

    pub fn set_nil(&mut self) {
        self.ptr = 0
    }

    pub fn is_nil(&self) -> bool {
        self.ptr == 0
    }
}

#[cfg(test)]
mod tests {

    use super::MemoryReference;

    #[test]
    fn mr_i32() {
        let value: i32 = 32;
        let mut mref = MemoryReference::new();

        mref.set_ref(&value);

        assert_eq!(mref.get_ref::<i32>(), &32);
    }

    #[test]
    fn mr_i64() {
        let value: i64 = 64;
        let mut mref = MemoryReference::new();

        mref.set_ref(&value);

        assert_eq!(mref.get_ref::<i64>(), &64_i64);
    }

    #[test]
    fn mr_mut_i32() {
        let value: i32 = 32;

        let mut mref = MemoryReference::new();

        mref.set_ref(&value);

        *mref.get_ref_mut::<i32>() = 33;

        assert_eq!(value, 33);
    }
}