
use std::{os::raw::c_uchar, ptr::null, ops::{AddAssign}};

use crate::{FuncState, LuaState, TaggedValue, StringBuffer, BytecodeLine, Tagged};

use super::number_scan;

pub trait LuaSource {
    fn read(&mut self, lua_state: *const LuaState) -> Result<(*const c_uchar, usize), ()>;
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

    /// parse number token
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

        number_scan::number_parse(
            self.string_buffer.get_read_ptr() as *const c_uchar,
            self.string_buffer.get_read_eptr() as *const c_uchar,
        )
    }

    // skip [=...=[ and ]=...=]
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

    // parse long string or long comment
    fn longstring(&mut self, source: &mut impl LuaSource, sep: i32, is_comment: bool) -> Result<TaggedValue, ()> {
        _ = self.save_next(source); // skip second [

        if self.is_eol() { // skip initial newline
            self.newline(source);
        }

        loop {
            match self.chr {
                char::MAX => return Err(()),
                ']' => {
                    if self.skip_eq(source).eq(&sep) {
                        self.save_next(source); // skip second ]
                        break;
                    }
                },
                '\n' | '\r' => {
                    self.save('\n');
                    _ = self.newline(source);

                    // TODO reset string buffer if is_comment
                }
                _ => _ = self.save_next(source),
            }
        }

        if is_comment {
            Ok(TaggedValue::from(Tagged::NIL))
        } else {
            Err(())
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