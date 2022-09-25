use super::{MemoryReference, IType, ToIType};

pub mod gc_bitmarks {
    pub const WHITE0: u8 = 0x01;
    pub const WHITE1: u8 = 0x02;
    pub const BLACK: u8 = 0x04;
    pub const FINALIZED: u8 = 0x08;
    pub const WEAKKEY: u8 = 0x08;
    pub const WEAKVAL: u8 = 0x10;
    pub const CDATA_FIN: u8 = 0x10;
    pub const FIXED: u8 = 0x20;
    pub const SFIXED: u8 = 0x40;

    pub const WHITES: u8 = WHITE0 | WHITE1;
    pub const COLORS: u8 = WHITES | BLACK;
    pub const WEAK: u8 = WEAKKEY | WEAKVAL;
}

#[repr(C)]
pub struct GCHeader {
    nextgc: GCReference,
    flags: u8,
    gct: IType,
}

impl GCHeader {
    pub fn new() -> Self {
        GCHeader {
            nextgc: GCReference::new(),
            flags: 0,
            gct: 0,
        }
    }

    pub fn set_nextgc(&mut self, r: &GCReference) {
        self.nextgc.set_gcref(r)
    }

    pub fn set_flag(&mut self, flag: u8) {
        self.flags |= flag
    }

    pub fn is_flag(&self, flag: u8) -> bool {
        (self.flags & flag) != 0
    } 

    pub fn set_gct(&mut self, gct: IType) {
        self.gct = !gct & 0x0f
    }

    pub fn get_nextgc(&self) -> &GCReference {
        &self.nextgc
    }

    pub fn get_nextgc_mut(&mut self) -> &mut GCReference {
        &mut self.nextgc
    }
}

impl ToIType for GCHeader {
    fn to_itype(&self) -> IType {
        !self.gct & 0x0f
    }
}


#[repr(C)]
pub struct GCReference {
    mref: MemoryReference
}

impl GCReference {
    pub fn new() -> Self {
        GCReference { mref: MemoryReference::new() }
    }

    pub fn set_ptr(&mut self, ptr: isize) {
        self.mref.set_ptr(ptr)
    }

    pub fn get_ptr(&self) -> isize {
        self.mref.get_ptr()
    }

    pub fn get_ref<T>(&self) -> &T {
        self.mref.get_ref::<T>()
    }

    pub fn get_ref_mut<T>(&self) -> &mut T {
        self.mref.get_ref_mut::<T>()
    }

    pub fn set_ref<T>(&mut self, r: &T) {
        self.mref.set_ref(r)
    }

    pub fn set_gcref(&mut self, r: &GCReference) {
        self.mref.set_mref(&r.mref)
    }

    pub fn set_nil(&mut self) {
        self.mref.set_nil()
    }

    pub fn is_nil(&self) -> bool {
        self.mref.is_nil()
    }

    pub fn get_gcheader(&self) -> &GCHeader {
        self.mref.get_ref::<GCHeader>()
    }

    pub fn get_gcheader_mut(&self) -> &mut GCHeader {
        self.mref.get_ref_mut::<GCHeader>()
    }

    pub fn get_next(&self) -> &GCReference {
        self.get_gcheader().get_nextgc()
    }

    pub fn get_next_mut(&mut self) -> &mut GCReference {
        self.get_gcheader_mut().get_nextgc_mut()
    }
}

impl PartialEq for GCReference {
    fn eq(&self, other: &Self) -> bool {
        self.get_ptr() == other.get_ptr()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct GCTestObject {
        gcheader: GCHeader,
        value: i32, 
    }

    #[test]
    fn gc_header() {
        let mut value = GCTestObject{
            gcheader: GCHeader::new(),
            value: 32,
        };

        value.gcheader.set_gct(2);

        let mut gcref = GCReference::new();
        gcref.set_ref(&value);

        assert_eq!(gcref.get_gcheader().to_itype(), 2);

        assert_eq!(gcref.get_next().is_nil(), true);
    }
}