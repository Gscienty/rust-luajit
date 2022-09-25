
use std::{os::raw::c_uchar, ptr::null, ops::{AddAssign, MulAssign, Mul}, rt::panic_count::count_is_zero};

use crate::{FuncState, LuaState, TaggedValue, StringBuffer, BytecodeLine, Tagged};

pub trait LuaSource {
    fn read(&mut self, lua_state: *const LuaState) -> Result<(*const c_uchar, usize), ()>;
}

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

pub struct LexState {
    func_state: *const FuncState, // current func state
    lua_state: *const LuaState,
    token_value: TaggedValue,
    lookahead_value: TaggedValue,
    input_ptr: *const c_uchar,
    input_eptr: *const c_uchar,
    chr: char,
    token: u32,
    lookahead: u32,
    string_buffer: StringBuffer,
    line_count: BytecodeLine,
    last_line: BytecodeLine,
}

impl LexState {
    pub fn new(lua_state: *const LuaState)  -> Self {
        LexState {
            func_state: null(),
            lua_state,
            token_value: TaggedValue::from(Tagged::NIL),
            lookahead_value: TaggedValue::from(Tagged::NIL),
            input_ptr: null(),
            input_eptr: null(),
            chr: '\0',
            token: 0,
            lookahead: 0,
            string_buffer: StringBuffer::new(unsafe { &*lua_state as &'static LuaState }),
            line_count: 1,
            last_line: 1,
        }
    }

    fn next(&mut self, source: &mut impl LuaSource) -> char {
        if self.input_ptr.lt(&self.input_eptr) {
            unsafe {
                self.chr = *self.input_ptr as char;
                self.input_ptr = self.input_ptr.add(1);
            }
            self.chr
        } else {
            self.chr = self.more(source);
            self.chr
        }
    }

    fn more(&mut self, source: &mut impl LuaSource) -> char {
        let mut size: usize = 0;
        let (input_ptr , size) = source.read(self.lua_state).unwrap();

        if input_ptr == null() || size == 0 {
            char::MAX
        } else {
            unsafe {
                self.input_ptr = input_ptr.add(1);
                self.input_eptr = input_ptr.add(size);

                *input_ptr as char
            }
        }
    }

    fn save(&mut self, chr: char) {
        self.string_buffer.put(chr);
    }

    fn save_next(&mut self, source: &mut impl LuaSource) -> char {
        self.save(self.chr);

        self.next(source)
    }

    fn is_eol(&self) -> bool {
        self.chr.eq(&'\n') || self.chr.eq(&'\r')
    }

    fn newline(&mut self, source: &mut impl LuaSource) {
        let old_chr = self.chr;
        self.next(source);
        if self.is_eol() && self.chr.ne(&old_chr) {
            self.next(source);
        }

        self.line_count.add_assign(1);
    }

    /// 解析数字类型，将解析结果
    fn number(&mut self, source: &mut impl LuaSource) -> Result<TaggedValue, ()> {
        if self.input_ptr.eq(&null()) {
            return Err(())
        }

        let mut c = char::MAX;
        while self.chr.is_digit(36) || self.chr.eq(&'.') || ((self.chr.eq(&'-') || self.chr.eq(&'+')) && c.eq(&char::MAX)) {
            c = self.chr;
            self.save_next(source);
        }
        self.save('\0');

        self.number_parse()
    }

    // 跳过 [=...=[ 和 ]=...=] 并返回长度
    fn skip_eq(&mut self, source: &mut impl LuaSource) -> i32 {
        let mut count = 0i32;

        let s = self.chr;
        while self.save_next(source).eq(&'=') {
            count.add_assign(1);
        }

        if self.chr.eq(&s) {
            count
        } else {
            (-count) - 1
        }
    }

    /// 将 string_buffer 中的存储的 Number Token 解析为 tagged_value
    fn number_parse(&self) -> Result<TaggedValue, ()> {
        let mut is_neg = false;
        let mut read_ptr = self.string_buffer.get_read_ptr() as *const c_uchar; 
        let read_eptr = self.string_buffer.get_read_eptr() as *const c_uchar; 

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

}

#[cfg(test)]
mod tests {
    use std::{os::raw::c_void, alloc::{Layout, alloc}};

    use crate::GlobalState;

    use super::*;

    // notes: alloc once
    fn mock_alloc(_: *const c_void, _: *const c_void, _: usize, new_size: usize) -> *const c_void {
        let layout = Layout::from_size_align(new_size, 1).unwrap();

        unsafe { alloc(layout) as *const c_void }
    } 

    struct MockLuaSource {
        once: bool,
        source: &'static str,
    }

    impl LuaSource for MockLuaSource {
        fn read(&mut self, lua_state: *const LuaState) -> Result<(*const c_uchar, usize), ()> {
            if self.once {
                Ok((null(), 0))
            } else {
                self.once = true;
                Ok((self.source.as_ptr(), str::len(self.source)))
            }
        }
    }

    #[test]
    fn lex_parse_number_position_infinity() {
        let global_state = GlobalState::new(mock_alloc);
        let lua_state = LuaState::new(&global_state);
        let mut lex_state = LexState::new(&lua_state);

        let mut src = MockLuaSource{once: false, source: "+INF"};
        lex_state.next(&mut src);

        let value = lex_state.number(&mut src).unwrap();

        assert_eq!(value.is_positive_infinity(), true);
    }

    #[test]
    fn lex_parse_number_negative_infinity() {
        let global_state = GlobalState::new(mock_alloc);
        let lua_state = LuaState::new(&global_state);
        let mut lex_state = LexState::new(&lua_state);

        let mut src = MockLuaSource{once: false, source: "-infinity"};
        lex_state.next(&mut src);

        let value = lex_state.number(&mut src).unwrap();
        assert_eq!(value.is_negative_infinity(), true);
    }

    #[test]
    fn lex_parse_number_nan() {
        let global_state = GlobalState::new(mock_alloc);
        let lua_state = LuaState::new(&global_state);
        let mut lex_state = LexState::new(&lua_state);

        let mut src = MockLuaSource{once: false, source: "NaN "};
        lex_state.next(&mut src);

        let value = lex_state.number(&mut src).unwrap();
        assert_eq!(value.is_nan(), true);
    }

    #[test]
    fn lex_parse_bin() {
        let global_state = GlobalState::new(mock_alloc);
        let lua_state = LuaState::new(&global_state);
        let mut lex_state = LexState::new(&lua_state);

        let mut src = MockLuaSource{once: false, source: "-0b00001101"};
        lex_state.next(&mut src);

        let value = lex_state.number(&mut src).unwrap();

        assert_eq!(i32::from(value), -0b00001101);
    }

    #[test]
    fn lex_parse_ubin() {
        let global_state = GlobalState::new(mock_alloc);
        let lua_state = LuaState::new(&global_state);
        let mut lex_state = LexState::new(&lua_state);

        let mut src = MockLuaSource{once: false, source: "0b00001101u"};
        lex_state.next(&mut src);

        let value = lex_state.number(&mut src).unwrap();

        assert_eq!(u32::from(value), 0b00001101);
    }

    #[test]
    fn lex_parse_hex() {
        let global_state = GlobalState::new(mock_alloc);
        let lua_state = LuaState::new(&global_state);
        let mut lex_state = LexState::new(&lua_state);

        let mut src = MockLuaSource{once: false, source: "0xffac"};
        lex_state.next(&mut src);

        let value = lex_state.number(&mut src).unwrap();

        assert_eq!(i32::from(value), 0xffac);
    }

    #[test]
    fn lex_parse_oct() {
        let global_state = GlobalState::new(mock_alloc);
        let lua_state = LuaState::new(&global_state);
        let mut lex_state = LexState::new(&lua_state);

        let mut src = MockLuaSource{once: false, source: "0667"};
        lex_state.next(&mut src);

        let value = lex_state.number(&mut src).unwrap();

        assert_eq!(i32::from(value), 0o667);
    }

    #[test]
    fn lex_parse_dec() {
        let global_state = GlobalState::new(mock_alloc);
        let lua_state = LuaState::new(&global_state);
        let mut lex_state = LexState::new(&lua_state);

        let mut src = MockLuaSource{once: false, source: "123.45+67"};
        lex_state.next(&mut src);

        let value = lex_state.number(&mut src).unwrap();

        assert_eq!(f64::from(value), 123.45);
    }
}