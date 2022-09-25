use std::{os::raw::c_uchar, ops::{MulAssign, AddAssign}};

use crate::TaggedValue;

#[derive(PartialEq, Eq)]
enum NumberScanBase {
    Oct,
    Hex,
    Bin,
    Dec,
}

impl NumberScanBase {
    fn get_radix(&self) -> u32 {
        match *self {
            Self::Oct => 8,
            Self::Hex => 16,
            Self::Bin => 2,
            Self::Dec => 10,
        }
    }
}

#[derive(PartialEq, Eq)]
enum NumberScanType {
    Int,
    UInt,
    Number,
}

/// 将 string_buffer 中的存储的 Number Token 解析为 tagged_value
pub fn number_parse(ptr: *const c_uchar, eptr: *const c_uchar) -> Result<TaggedValue, ()> {
    let mut is_neg = false;
    let mut read_ptr = ptr; 
    let read_eptr = eptr; 

    // Token 开始位置不为数字时的处理
    if !unsafe { *read_ptr as char }.is_digit(10) {
        while unsafe { *read_ptr as char }.is_whitespace() {
            read_ptr = unsafe { read_ptr.add(1) };
        }

        // 正负号处理
        if unsafe { *read_ptr as char }.eq(&'+') || unsafe { *read_ptr as char }.eq(&'-') {
            is_neg = unsafe { *read_ptr as char }.eq(&'-');
            read_ptr = unsafe { read_ptr.add(1) };
        }
        
        // 特殊数字处理, inf || infinity || nan
        if unsafe { *read_ptr as char }.ge(&'A') {
            let mut tagged_value = TaggedValue::from(0xfff80000_00000000u64);

            read_ptr = if unsafe { *read_ptr as char }.eq_ignore_ascii_case(&'i')
                && unsafe { *read_ptr.add(1) as char }.eq_ignore_ascii_case(&'n')
                && unsafe { *read_ptr.add(2) as char }.eq_ignore_ascii_case(&'f') {
                
                tagged_value = if is_neg {
                    TaggedValue::from(0xfff00000_00000000u64)
                } else {
                    TaggedValue::from(0x7ff00000_00000000u64)
                };

                if unsafe { *read_ptr as char }.eq_ignore_ascii_case(&'i')
                    && unsafe { *read_ptr.add(1) as char }.eq_ignore_ascii_case(&'n')
                    && unsafe { *read_ptr.add(2) as char }.eq_ignore_ascii_case(&'i')
                    && unsafe { *read_ptr.add(3) as char }.eq_ignore_ascii_case(&'t')
                    && unsafe { *read_ptr.add(4) as char }.eq_ignore_ascii_case(&'y') {
                    unsafe { read_ptr.add(8) }
                } else {
                    unsafe { read_ptr.add(3) }
                }
            } else if unsafe { *read_ptr as char }.eq_ignore_ascii_case(&'n') 
                && unsafe { *read_ptr.add(1) as char }.eq_ignore_ascii_case(&'a')
                && unsafe { *read_ptr.add(2) as char }.eq_ignore_ascii_case(&'n') {
                unsafe { read_ptr.add(3) }
            } else {
                read_ptr
            };

            if read_ptr.ge(&read_eptr) {
                return Err(()) // TODO 补全错误类型
            }  else {
                return Ok(tagged_value)
            }
        }
    }

    // 获取数字进制
    let base = if unsafe { *read_ptr as char }.ne(&'0') {
        NumberScanBase::Dec
    } else if unsafe { *read_ptr.add(1) as char }.eq_ignore_ascii_case(&'x') {
        read_ptr = unsafe { read_ptr.add(2) };
        NumberScanBase::Hex
    } else if unsafe { *read_ptr.add(1) as char }.eq_ignore_ascii_case(&'b') {
        read_ptr = unsafe { read_ptr.add(2) };
        NumberScanBase::Bin
    } else {
        read_ptr = unsafe { read_ptr.add(1) };
        NumberScanBase::Oct
    };

    // number token 数字部分的位置
    let mut n_ptr = read_ptr;
    let mut n_eptr = read_ptr;
    let mut n_type = NumberScanType::Int;

    // 寻找suffix
    while n_eptr.ne(&read_eptr) {
        unsafe {
            if (*n_eptr as char).eq(&'.') && n_type.ne(&NumberScanType::Number) && base.eq(&NumberScanBase::Dec) {
                n_type = NumberScanType::Number;
            } else if !(*n_eptr as char).is_digit(base.get_radix()) {
                break;
            }
            n_eptr = n_eptr.add(1);
        }
    }
    if n_eptr.ne(&read_eptr) {
        unsafe {
            if (*n_eptr as char).eq(&'u') && n_type.ne(&NumberScanType::Number) && !is_neg {
                n_type = NumberScanType::UInt;
            } else if (*n_eptr as char).ne(&'\0') {
                println!("HERE {}", *n_eptr as char);
                return Err(()) // TODO unexpected suffix
            }
        }
    }

    match base {
        NumberScanBase::Bin => {
            match n_type {
                NumberScanType::Int => {
                    let mut value = 0i32;
                    while n_ptr.ne(&n_eptr) {
                        unsafe {
                            value.mul_assign(base.get_radix() as i32);
                            value.add_assign((*n_ptr as i32) - ('0' as i32));

                            n_ptr = n_ptr.add(1);
                        }
                    }

                    if is_neg {
                        value = -value;
                    }
                    Ok(TaggedValue::from(value))
                },
                NumberScanType::UInt => {
                    let mut value = 0u32;
                    while n_ptr.ne(&n_eptr) {
                        unsafe {
                            value.mul_assign(base.get_radix());
                            value.add_assign((*n_ptr as u32) - ('0' as u32));
                            n_ptr = n_ptr.add(1);
                        }
                    }

                    Ok(TaggedValue::from(value))
                },
                _ => Err(())
            }
        },
        NumberScanBase::Dec => {
            match n_type {
                NumberScanType::Int => {
                    let mut value = 0i32;
                    while n_ptr.ne(&n_eptr) {
                        unsafe {
                            value.mul_assign(base.get_radix() as i32);
                            value.add_assign((*n_ptr as i32) - ('0' as i32));
                            n_ptr = n_ptr.add(1);
                        }
                    }

                    Ok(TaggedValue::from(value))
                },
                NumberScanType::UInt => {
                    let mut value = 0u32;
                    while n_ptr.ne(&n_eptr) {
                        unsafe {
                            value.mul_assign(base.get_radix());
                            value.add_assign((*n_ptr as u32) - ('0' as u32));
                            n_ptr = n_ptr.add(1);
                        }
                    }

                    Ok(TaggedValue::from(value))
                },
                NumberScanType::Number => {
                    let mut value = 0f64;

                    while n_ptr.ne(&n_eptr) {
                        unsafe {
                            if (*n_ptr as char).eq(&'.') {
                                n_ptr = n_ptr.add(1);
                                break
                            }

                            value.mul_assign(base.get_radix() as f64);
                            value.add_assign(((*n_ptr as u32) - ('0' as u32)) as f64);

                            n_ptr = n_ptr.add(1);
                        }
                    }

                    let mut decimal_part = 0.1;
                    while n_ptr.ne(&n_eptr) {
                        unsafe {
                            value.add_assign(((*n_ptr as u32) - ('0' as u32)) as f64 * decimal_part);
                            decimal_part.mul_assign(0.1);

                            n_ptr = n_ptr.add(1);
                        }
                    }

                    Ok(TaggedValue::from(value))
                },
            }
        },
        NumberScanBase::Hex => {
            match n_type {
                NumberScanType::Int => {
                    let mut value = 0i32;
                    while n_ptr.ne(&n_eptr) {
                        unsafe {
                            value.mul_assign(base.get_radix() as i32);
                            value.add_assign(match *n_ptr as char {
                                '0' ..= '9' => (*n_ptr as i32) - ('0' as i32),
                                'a' ..= 'f' => (*n_ptr as i32) - ('a' as i32) + 10,
                                'A' ..= 'F' => (*n_ptr as i32) - ('A' as i32) + 10,
                                _ => {
                                    return Err(()); // TODO error
                                }
                            });
                            n_ptr = n_ptr.add(1);
                        }
                    }

                    if is_neg {
                        value = -value;
                    }
                    Ok(TaggedValue::from(value))
                },
                NumberScanType::UInt => {
                    let mut value = 0u32;
                    while n_ptr.ne(&n_eptr) {
                        unsafe {
                            value.mul_assign(base.get_radix());
                            value.add_assign(match *n_ptr as char {
                                '0' ..= '9' => (*n_ptr as u32) - ('0' as u32),
                                'a' ..= 'f' => (*n_ptr as u32) - ('a' as u32) + 10,
                                'A' ..= 'F' => (*n_ptr as u32) - ('A' as u32) + 10,
                                _ => {
                                    return Err(()); // TODO error
                                }
                            });
                            n_ptr = n_ptr.add(1);
                        }
                    }

                    Ok(TaggedValue::from(value))
                },
                _ => Err(())
            }
        },
        NumberScanBase::Oct => {
            match n_type {
                NumberScanType::Int => {
                    let mut value = 0i32;
                    while n_ptr.ne(&n_eptr) {
                        unsafe {
                            value.mul_assign(base.get_radix() as i32);
                            value.add_assign((*n_ptr as i32) - ('0' as i32));

                            n_ptr = n_ptr.add(1);
                        }
                    }

                    if is_neg {
                        value = -value;
                    }
                    Ok(TaggedValue::from(value))
                },
                NumberScanType::UInt => {
                    let mut value = 0u32;
                    while n_ptr.ne(&n_eptr) {
                        unsafe {
                            value.mul_assign(base.get_radix() as u32);
                            value.add_assign((*n_ptr as u32) - ('0' as u32));

                            n_ptr = n_ptr.add(1);
                        }
                    }

                    Ok(TaggedValue::from(value))
                },
                _ => Err(())
            }
        }
    }
}