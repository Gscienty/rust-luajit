use std::ops::{AddAssign, ShlAssign, MulAssign};

use crate::{types::Value, bytecode::ByteInstruction};

use super::{source::LuaSource, token::Token, VarInfo, ByteCodeInstructionLine};

pub(crate) struct LexState<'s> {
    source: LuaSource<'s>,
    token_builder: Vec<char>,
    pub(crate) line_number: u32,
    pub(crate) last_line: u32,

    pub(crate) token: Token,
    pub(crate) lookahead_token: Token,

    pub(crate) var_stack: Vec<VarInfo>,
    pub(crate) instructions: Vec<ByteCodeInstructionLine>,
}

mod format {
    use std::{collections::VecDeque, ops::{MulAssign, AddAssign, DivAssign}};

    use crate::{types::Value, lex::Token};

    pub(super) fn parse_number(token_builder: &Vec<char>, default_radix: u32) -> Result<Value, &str> {
        let mut reader = VecDeque::new();
        reader.extend(token_builder);

        let negative = matches!(reader.front(), Some('-'));
        if matches!(reader.front(), Some('-') | Some('+')) {
            reader.pop_front();
        }

        // inf | infinity | nan
        if matches!(reader.front(), Some(chr) if chr.is_alphabetic()) {
            if matches!(reader.front(), Some(chr) if chr.eq_ignore_ascii_case(&'i')) {
                reader.pop_front();
                // inf | infinity
                if !matches!(reader.pop_front(), Some(chr) if chr.eq_ignore_ascii_case(&'n')) {
                    return Err("unexception word")
                }
                if !matches!(reader.pop_front(), Some(chr) if chr.eq_ignore_ascii_case(&'f')) {
                    return Err("unexception word")
                }

                if reader.front().is_some() {
                    if !matches!(reader.pop_front(), Some(chr) if chr.eq_ignore_ascii_case(&'i')) {
                        return Err("unexception word")
                    }
                    if !matches!(reader.pop_front(), Some(chr) if chr.eq_ignore_ascii_case(&'n')) {
                        return Err("unexception word")
                    }
                    if !matches!(reader.pop_front(), Some(chr) if chr.eq_ignore_ascii_case(&'i')) {
                        return Err("unexception word")
                    }
                    if !matches!(reader.pop_front(), Some(chr) if chr.eq_ignore_ascii_case(&'t')) {
                        return Err("unexception word")
                    }
                    if !matches!(reader.pop_front(), Some(chr) if chr.eq_ignore_ascii_case(&'y')) {
                        return Err("unexception word")
                    }
                }

                return Ok(Value::Number(if negative {
                    f64::NEG_INFINITY
                } else {
                    f64::INFINITY
                }));
            } else if matches!(reader.front(), Some(chr) if chr.eq_ignore_ascii_case(&'n')) {
                reader.pop_front();
                // nan
                if !matches!(reader.pop_front(), Some(chr) if chr.eq_ignore_ascii_case(&'a')) {
                    return Err("unexception word")
                }
                if !matches!(reader.pop_front(), Some(chr) if chr.eq_ignore_ascii_case(&'n')) {
                    return Err("unexception word")
                }

                return Ok(Value::Number(f64::NAN))
            } else {
                return Err("unexception word")
            }
        }

        let radix = if matches!(reader.front(), Some('0')) {
            reader.pop_front();

            match reader.front() {
                Some('B') | Some('b') => {
                    reader.pop_front();
                    2
                },
                Some('X') | Some('x') => {
                    reader.pop_front();
                    16
                },
                _ => 8,
            }
        } else {
            default_radix 
        };

        // integer
        let must_integer = if matches!(reader.back(), Some('u') | Some('i')) {
            if matches!(reader.back(), Some('u')) && negative {
                return Err("unexpected suffix 'u'")
            }
            reader.pop_back();
            true
        } else {
            false
        };

        let mut is_float = false;
        let mut integer_value = 0i64;
        let mut float_value = 0f64;

        while let Some(chr) = reader.pop_front() {
            match chr {
                '.' => {
                    if must_integer {
                        return Err("unexpected '.'")
                    }
                    is_float = true;
                    float_value = integer_value as f64;

                    let mut base = f64::from(1);
                    while let Some(chr) = reader.front() {
                        if let Some(digit) = chr.to_digit(radix) {
                            base.div_assign(f64::from(radix));
                            float_value.add_assign(base * f64::from(digit));

                            reader.pop_front();
                        } else {
                            break;
                        }
                    }
                },
                'p' | 'P' => {
                    if must_integer {
                        return Err("unexpected 'p'")
                    }
                    if radix.ne(&16) {
                        return Err("unexcepted radix")
                    }
                    if !is_float {
                        float_value = integer_value as f64;
                    }
                    is_float = true;

                    float_value = if let Ok(pointer) = parse_number(&Vec::from(reader), 16) {
                        match pointer {
                            Value::Integer(value) => float_value.powi(value as i32),
                            Value::Number(value) => float_value.powf(value),
                            _ => return Err("unexpected point")
                        }
                    } else {
                        return Err("unexpected point")
                    };

                    break;
                },
                'e' | 'E' => {
                    if must_integer {
                        return Err("unexpected 'e'")
                    }
                    if radix.ne(&10) {
                        return Err("unexcepted radix")
                    }
                    if !is_float {
                        float_value = integer_value as f64;
                    }
                    is_float = true;

                    float_value = if let Ok(pointer) = parse_number(&Vec::from(reader), 10) {
                        match pointer {
                            Value::Integer(value) => float_value.powi(value as i32),
                            Value::Number(value) => float_value.powf(value),
                            _ => return Err("unexpected point")
                        }
                    } else {
                        return Err("unexpected point")
                    };
                    break;
                },
                _ => {
                    if let Some(digit) = chr.to_digit(radix) {
                        integer_value.mul_assign(radix as i64);
                        integer_value.add_assign(digit as i64);
                    }
                }
            }
        }

        return Ok(if is_float {
            if negative {
                Value::Number(-float_value)
            } else {
                Value::Number(float_value)
            }
        } else {
            if negative {
                Value::Integer(-integer_value)
            } else {
                Value::Integer(integer_value)
            }
        })
    }

    pub(super) fn parse_name(token_builder: &Vec<char>) -> Token {
        let word = token_builder.iter().collect::<String>();

        match word.as_str() {
            "else" => Token::Else,
            "elseif" => Token::ElseIf,
            "end" => Token::End,
            "until" => Token::Until,
            "if" => Token::If,
            "while" => Token::While,
            "do" => Token::Do,
            "for" => Token::For,
            "repeat" => Token::Repeat,
            "function" => Token::Function,
            "local" => Token::Local,
            "return" => Token::Return,
            "break" => Token::Break,
            "goto" => Token::Goto,
            _ => Token::Name(word),
        }

    }
}

impl<'s> LexState<'s> {
    pub(crate) fn new(source: LuaSource<'s>) -> Self {
        LexState {
            source,
            token_builder: Vec::new(),
            line_number: 1,
            last_line: 1,

            token: Token::EOF,
            lookahead_token: Token::EOF,

            var_stack: Vec::new(),
            instructions: Vec::new(),
        }
    }

    fn save(&mut self, chr: char) {
        self.token_builder.push(chr)
    }

    fn savenext(&mut self) -> Option<char> {
        if let Some(chr) = self.source.get() {
            self.save(chr);
            self.source.next()
        } else {
            None
        }
    }

    fn newline(&mut self) {
        if self.source.is_eol() {
            let old = self.source.get();
            self.source.next();

            // skip '\n\r' or '\r\n'
            if self.source.is_eol() && self.source.get().ne(&old) {
                self.source.next();
            }

            self.line_number.add_assign(1);
        }
    }

    fn clear(&mut self) {
        self.token_builder.clear();
    }

    fn number(&mut self) -> Result<Value, &str> {
        if self.source.is_eof() {
            return Err("source is eof")
        }

        let mut p = true;
        let mut e: bool = true;
        let mut symbol = true;
        let mut dot = true;

        loop {
            match self.source.get() {
                Some('+') | Some('-') => if symbol {
                    self.savenext();
                    symbol = false;
                } else {
                    return Err("unexpected symbol");
                }
                Some('.') => if dot {
                    self.savenext();
                    dot = false;
                } else {
                    return Err("unexpected dot");
                }
                Some('p') => if p {
                    self.savenext();
                    symbol = true;
                    p = false;
                    e = false;
                    dot = true;
                } else {
                    return Err("unexpected p");
                }
                Some('e') => if e {
                    self.savenext();
                    symbol = true;
                    p = false;
                    e = false;
                    dot = true;
                } else {
                    return Err("unexpected e");
                } 
                _ => if self.source.is_digit(36) {
                    self.savenext();
                    symbol = false;
                } else {
                    break;
                }
            }
        }

        format::parse_number(&self.token_builder, 10)
    }

    fn skip_eq(&mut self) -> i32 {
        let mut count = 0;
        let s = self.source.get();

        while matches!(self.savenext(), Some('=')) {
            count.add_assign(1);
        };

        if self.source.get().eq(&s) {
            count
        } else {
            (-count) - 1
        }
    }

    fn long_string(&mut self, sep: i32, is_comment: bool) -> Result<String, &str> {
        // skip [
        self.savenext();
        // skip newline
        if self.source.is_eol() {
            self.newline();
        }

        loop {
            match self.source.get() {
                None => return Err("unexpected eof"),
                Some(']') => {
                    if self.skip_eq().eq(&sep) {
                        self.savenext(); // skip ]
                        break;
                    }
                },
                Some('\n') | Some('\r') => {
                    self.save('\n');
                    self.newline();
                    if is_comment {
                        self.clear();
                    }
                }
                _ => {
                    self.savenext();
                }
            }
        };

        if is_comment {
            Ok(String::new())
        } else {
            Ok(self.token_builder
                .iter()
                .skip((2 + sep) as usize)
                .take(self.token_builder.len() - (2 * (2 + sep)) as usize)
                .collect::<String>())
        }

    }

    fn string(&mut self) -> Result<String, &str> {
        let delim = self.source.get();
        self.savenext();

        while self.source.get().ne(&delim) {
            match self.source.get() {
                None => {
                    return Err("unexception eof");
                }
                Some('\r') | Some('\n') => {
                    return Err("unexception eol");
                }
                Some('\\') => {
                    if let Some(chr) = self.source.next() {
                        let c = match chr {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            'x' => {
                                if let Some(c) = self.source.next() {
                                    let mut ret = c
                                        .to_digit(16)
                                        .ok_or("unexception hex")? << 4;
                                    
                                    if let Some(c) = self.source.next() {
                                        ret.add_assign(c
                                            .to_digit(16)
                                            .ok_or("unexception hex")?)
                                    } else {
                                        return Err("unexception eof")
                                    }
                                    
                                    ret as u8 as char
                                } else {
                                    return Err("unexception eof")
                                }
                            },
                            'u' => {
                                if !matches!(self.source.next(), Some('{')) {
                                    return Err("unexception {");
                                }
                                let mut chr = 0u32;
                                loop {
                                    chr.shl_assign(4);
                                    chr.add_assign(
                                        self.source.get()
                                            .and_then(|c| c.to_digit(16))
                                            .ok_or("unexception hex")?
                                    );

                                    if chr.ge(&0x110000) {
                                        return Err("unexception unicode")
                                    }
                                    if matches!(self.source.next(), Some('}')) {
                                        break;
                                    }
                                }

                                if chr.lt(&0x800) {
                                    if chr.lt(&0x80) {
                                        chr as u8 as char
                                    } else {
                                        self.save((0xc0 | (chr >> 6)) as u8 as char);

                                        (0x80 | (chr & 0x3f)) as u8 as char
                                    }
                                } else {
                                    if chr.ge(&0x10000) {
                                        self.save((0xf0 | (chr >> 18)) as u8 as char);
                                        self.save((0x80 | ((chr >> 12) & 0x3f)) as u8 as char);
                                    } else {
                                        if chr.ge(&0xd800) && chr.lt(&0xe000) {
                                            return Err("unexception unicode");
                                        }
                                        self.save((0xe0 | (chr >> 12)) as u8 as char);
                                    }
                                    self.save((0x80 | ((chr >> 6) & 0x3f)) as u8 as char);

                                    (0x80 | (chr & 0x3f)) as u8 as char
                                }
                            }
                            'z' => {
                                self.source.next();
                                loop {
                                    if let Some(chr) = self.source.get() {
                                        if chr.is_whitespace() {
                                            if self.source.is_eol() {
                                                self.newline();
                                            } else {
                                                self.source.next();
                                            }
                                        } else {
                                            break;
                                        }
                                    } else {
                                        break;
                                    }
                                };
                                continue;
                            }
                            '\n' | '\r' => {
                                self.save('\n');
                                self.newline();
                                continue;
                            }
                            '\\' | '\"' | '\'' => chr,
                            _ => {
                                let mut num = chr.to_digit(10).ok_or("unexception number")?;
                                if let Some(n) = self.source.next().and_then(|chr| chr.to_digit(10)) {
                                    num.mul_assign(10);
                                    num.add_assign(n);
                                }
                                if let Some(n) = self.source.next().and_then(|chr| chr.to_digit(10)) {
                                    num.mul_assign(10);
                                    num.add_assign(n);
                                }

                                if num.gt(&255) {
                                    return Err("unexception num");
                                }
                                self.save(num as u8 as char);

                                continue;
                            }
                        };

                        self.save(c);
                        self.source.next();
                    } else {
                        return Err("unexception esp")
                    }
                }
                _ => {
                    self.savenext();
                }
            }
        }
        self.savenext();

        Ok(self.token_builder
            .iter()
            .skip(1)
            .take((self.token_builder.len() - 2) as usize)
            .collect::<String>())
    }

    fn scan(&mut self) -> Result<Token, &str> {
        self.clear();

        loop {
            if matches!(self.source.get(), Some(chr) if chr.is_alphanumeric()) {
                if matches!(self.source.get(), Some(chr) if chr.is_numeric()) {
                    return self.number().and_then(|number| Ok(Token::Number(number)))
                }

                // identifier or reserved word
                while matches!(self.source.get(), Some(chr) if chr.is_alphanumeric()) {
                    self.savenext();
                }
                return Ok(format::parse_name(&self.token_builder))
            }

            match self.source.get() {
                Some('\r') | Some('\n') => {
                    self.newline();
                    continue;
                }
                Some(' ') | Some('\t') => {
                    self.source.next();
                    continue;
                }
                None => {
                    return Ok(Token::EOF);
                }
                Some('-') => {
                    self.source.next();
                    if !matches!(self.source.get(), Some('-')) {
                        return Ok(Token::SingleChar('-'))
                    }
                    self.source.next();

                    if matches!(self.source.get(), Some('[')) {
                        // comment
                        let sep = self.skip_eq();
                        self.clear();

                        if sep.ge(&0) {
                            _ = self.long_string(sep, true);
                            self.clear();

                            continue;
                        }
                    }
                }
                Some('[') => {
                    let sep = self.skip_eq();
                    if sep.ge(&0) {
                        return Ok(Token::String(self.long_string(sep, false)?))
                    } else if sep.eq(&-1) {
                        return Ok(Token::SingleChar('['))
                    } else {
                        return Err("unexception [")
                    }
                }
                Some('=') => {
                    self.source.next();
                    return if matches!(self.source.get(), Some('=')) {
                        self.source.next();
                        Ok(Token::EQ)
                    } else {
                        Ok(Token::SingleChar('='))
                    };
                }
                Some('<') => {
                    self.source.next();
                    return if matches!(self.source.get(), Some('=')) {
                        self.source.next();
                        Ok(Token::LE)
                    } else {
                        Ok(Token::SingleChar('<'))
                    };
                }
                Some('>') => {
                    self.source.next();
                    return if matches!(self.source.get(), Some('=')) {
                        self.source.next();
                        Ok(Token::GE)
                    } else {
                        Ok(Token::SingleChar('>'))
                    };
                }
                Some('~') => {
                    self.source.next();
                    return if matches!(self.source.get(), Some('=')) {
                        self.source.next();
                        Ok(Token::NE)
                    } else {
                        Ok(Token::SingleChar('~'))
                    };
                }
                Some(':') => {
                    self.source.next();
                    return if matches!(self.source.get(), Some(':')) {
                        self.source.next();
                        Ok(Token::Label)
                    } else {
                        Ok(Token::SingleChar(':'))
                    };
                }
                Some('"') | Some('\'') => {
                    return Ok(Token::String(self.string()?))
                }
                Some('.') => {
                    if matches!(self.savenext(), Some('.')) {
                        self.source.next();
                        return if matches!(self.source.get(), Some('.')) {
                            self.source.next();
                            Ok(Token::Dots)
                        } else {
                            Ok(Token::Concat)
                        }

                    } else if !matches!(self.source.get(), Some(chr) if chr.is_digit(10)) {
                        return Ok(Token::SingleChar('.'))
                    } else {
                        return self.number().and_then(|value| Ok(Token::Number(value)))
                    }
                }
                _ => {
                    // single-char token
                    if let Some(chr) = self.source.get() {
                        self.source.next();

                        return Ok(match chr {
                            ';' => Token::Semicolon, 
                            _ => Token::SingleChar(chr),
                        });
                    } else {
                        return Err("internal error");
                    }
                }
            };
        }
    }

    pub(crate) fn next(&mut self) {
        self.last_line = self.line_number;
        if matches!(self.lookahead_token, Token::EOF) {
            self.token = if let Ok(token) = self.scan() {
                token
            } else {
                Token::EOF
            }
        } else {
            self.token = self.lookahead_token.clone();
            self.lookahead_token = Token::EOF;
        }
    }

    pub(crate) fn lookahead(&mut self) -> Token {
        self.lookahead_token = if let Ok(token) = self.scan() {
            token
        } else {
            Token::EOF
        };

        self.lookahead_token.clone()
    }

    pub(crate) fn is_end_block(&self) -> bool {
        matches!(self.token, Token::Else | Token::ElseIf | Token::End | Token::Until | Token::EOF)
    }

    pub(crate) fn check_and_consume(&mut self, token: Token) -> bool {
        if self.token.eq(&token) {
            self.next();

            true
        } else {
            false
        }
    }

    pub(crate) fn get_ins(&self, index: usize) -> Option<ByteInstruction> {
        self.instructions
            .get(index)
            .and_then(|ins| Some(ins.instruction))
    }

    pub(crate) fn get_ins_mut(&mut self, index: usize) -> Option<&mut ByteInstruction> {
        self.instructions
            .get_mut(index)
            .and_then(|ins| Some(&mut ins.instruction))
    }

    pub(crate) fn get_insline(&self, index: usize) -> Option<u32> {
        self.instructions
            .get(index)
            .and_then(|ins| Some(ins.line))
    }

    pub(crate) fn get_insline_mut(&mut self, index: usize) -> Option<&mut u32> {
        self.instructions
            .get_mut(index)
            .and_then(|ins| Some(&mut ins.line))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn number(source: &str) -> Result<Value, &str> {
        let mut s = LuaSource::source(source);
        s.setup();
        let mut state = LexState::new(s);
        if let Ok(value) = state.number() {
            Ok(value)
        } else {
            Err("failed")
        }
    }

    fn scan(source: &str) -> Result<Token, &str> {
        let mut s = LuaSource::source(source);
        s.setup();
        let mut state = LexState::new(s);
        if let Ok(value) = state.scan() {
            Ok(value)
        } else {
            Err("failed")
        }
    }

    #[test]
    fn lex_state_number() {
        let infinity = number("+infinity");
        assert_eq!(infinity.is_ok(), true);
        assert_eq!(infinity.unwrap(), Value::Number(f64::INFINITY));

        let neg_infinity = number("-INF");
        assert_eq!(neg_infinity.is_ok(), true);
        assert_eq!(neg_infinity.unwrap(), Value::Number(f64::NEG_INFINITY));

        let nan = number("NaN");
        assert_eq!(nan.is_ok(), true);
        assert_ne!(nan.unwrap(), Value::Number(f64::NAN));

        let integer = number("18134");
        assert_eq!(integer.is_ok(), true);
        assert_eq!(integer.unwrap(), Value::Integer(18134));

        let neg_integer = number("-65335");
        assert_eq!(neg_integer.is_ok(), true);
        assert_eq!(neg_integer.unwrap(), Value::Integer(-65335));

        let binary_dot = number("0b1.001");
        assert_eq!(binary_dot.is_ok(), true);
        assert_eq!(binary_dot.unwrap(), Value::Number(1.125));

        let hex_p = number("0xffp0");
        assert_eq!(hex_p.is_ok(), true);
        assert_eq!(hex_p.unwrap(), Value::Number(1.0));

    }

    fn assert_string(lua_string: &str, expect_string: &str) {
        if let Ok(Token::String(value)) = scan(lua_string) {
            assert_eq!(value, String::from(expect_string));
        } else {
            assert!(false)
        }
    }

    #[test]
    fn lex_state_string() {
        assert_string("\"hello world\"", "hello world");
        assert_string("\'\'", "");
        assert_string("\'\\48\'", "0");
        assert_string("\'\\r\'", "\r");
    }
}