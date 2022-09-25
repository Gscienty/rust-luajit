/// nan-boxing TValue
use super::{GCReference, Number};

/// TaggedValue contains:
/// primitive types
/// GC objects
/// lightuserdata
/// int
/// number
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct TaggedValue {
    value: u64,
}

pub type IType = u8;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Tagged {
    NIL,
    FALSE,
    TRUE,
    LIGHTUD,
    STR,
    UPVAL,
    THREAD,
    PROTO,
    FUNC,
    TRACE,
    CDATA,
    TAB,
    UDATA,
    NUMX,
    UNKNOW,
}

pub trait ToIType {
    fn to_itype(&self) -> IType;

    fn to_tagged(&self) -> Tagged {
        Tagged::from(self.to_itype())
    }
}

impl ToIType for Tagged {
    fn to_itype(&self) -> IType {
        match self {
           &Self::NIL => 0b1111, 
           &Self::FALSE => 0b1110,
           &Self::TRUE => 0b1101,
           &Self::LIGHTUD => 0b1100,
           &Self::STR => 0b1011,
           &Self::UPVAL => 0b1010,
           &Self::THREAD => 0b1001,
           &Self::PROTO => 0b1000,
           &Self::FUNC => 0b0111,
           &Self::TRACE => 0b0110,
           &Self::CDATA => 0b0101,
           &Self::TAB => 0b0100,
           &Self::UDATA => 0b0011,
           &Self::NUMX => 0b0010,
           _ => 0b0000,
        }
    }
}

impl From<IType> for Tagged {
    fn from(itype: IType) -> Self {
        match itype {
            0b1111 => Self::NIL,
            0b1110 => Self::FALSE,
            0b1101 => Self::TRUE,
            0b1100 => Self::LIGHTUD,
            0b1011 => Self::STR,
            0b1010 => Self::UPVAL,
            0b1001 => Self::THREAD,
            0b1000 => Self::PROTO,
            0b0111 => Self::FUNC,
            0b0110 => Self::TRACE,
            0b0101 => Self::CDATA,
            0b0100 => Self::TAB,
            0b0011 => Self::UDATA,
            0b0010 => Self::NUMX,
            _ => Self::UNKNOW,
        }
    }
}

impl ToIType for TaggedValue {
    fn to_itype(&self) -> IType {
        (self.value >> 47 & 0b00001111) as IType
    }
}

impl From<Tagged> for TaggedValue {
    fn from(value: Tagged) -> Self {
        TaggedValue {
            value: (0xfff8 << 48) | ((value.to_itype() as u64) << 47) | 0x00007fffffffffff
        }
    }
}

impl From<TaggedValue> for Tagged {
    fn from(value: TaggedValue) -> Self {
        Tagged::from(value.to_itype())
    }
}

impl From<GCReference> for TaggedValue {
    fn from(value: GCReference) -> Self {
        TaggedValue {
            value: ((0xfff8 as u64) << 48) | ((value.get_gcheader().to_itype() as u64) << 47) | (value.get_ptr() as u64)
        }
    }
}

impl From<TaggedValue> for GCReference {
    fn from(value: TaggedValue) -> Self {
        let mut gcr = GCReference::new();
        gcr.set_ptr((value.value & 0x00007fff_ffffffff) as isize);

        gcr
    }
}

impl From<Number> for TaggedValue {
    fn from(value: Number) -> Self {
        TaggedValue {
            value: unsafe { *(&value as *const f64 as *const u64) }
        }
    }
}

impl From<TaggedValue> for Number {
    fn from(value: TaggedValue) -> Self {
        unsafe { *(&value.value as *const u64 as *const f64) }
    }
}

impl From<u32> for TaggedValue {
    fn from(value: u32) -> Self {
        TaggedValue {
            value: ((0xfff8 as u64) << 48) | (value as u64)
        }
    }
}

impl From<TaggedValue> for u32 {
    fn from(value: TaggedValue) -> Self {
        (value.value & 0xffffffffu64) as u32
    }
}

impl From<TaggedValue> for i32 {
    fn from(value: TaggedValue) -> Self {
        unsafe { *(&value.value as *const u64 as *const i32) }
    }
}

impl From<i32> for TaggedValue {
    fn from(value: i32) -> Self {
        TaggedValue {
            value: ((0xfff8 as u64) << 48) | (unsafe { *(&value as *const i32 as *const u32) } as u64)
        }
    }
}

impl From<u64> for TaggedValue {
    fn from(value: u64) -> Self {
        TaggedValue {
            value
        }
    }
}

impl From<TaggedValue> for u64 {
    fn from(value: TaggedValue) -> Self {
        value.value
    }
}

impl TaggedValue {
    pub fn is_positive_infinity(&self) -> bool {
        self.value.eq(&0x7ff00000_00000000)
    }

    pub fn is_negative_infinity(&self) -> bool {
        self.value.eq(&0xfff00000_00000000)
    }

    pub fn is_nan(&self) -> bool {
        self.value.eq(&0xfff80000_00000000)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tv_primitive_eq(tag: Tagged) {
        assert_eq!(TaggedValue::from(tag).to_tagged(), tag);
    }

    #[test]
    fn tv_primitive() {
        tv_primitive_eq(Tagged::NIL);
        tv_primitive_eq(Tagged::FALSE);
        tv_primitive_eq(Tagged::TRUE);
    }
}