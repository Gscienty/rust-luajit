use crate::{lexer::Token, parser::ParseErr};

pub(crate) struct Lexer {
    source: String,
    read_offset: usize,

    pub(crate) line_number: usize,
    pub(crate) last_line: usize,
    current: Option<char>,

    token_buffer: String,

    pub(crate) token: Token,
    lookahead_token: Token,
}

impl Lexer {
    pub(crate) fn new(source: &str) -> Self {
        Lexer {
            source: String::from(source),
            read_offset: 1,

            line_number: 1,
            last_line: 1,
            current: source.chars().next(),

            token_buffer: String::new(),

            token: Token::EOF,
            lookahead_token: Token::EOF,
        }
    }

    pub(crate) fn token_next(&mut self) {
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

        log::debug!("token next: {}", self.token)
    }

    pub(crate) fn token_lookahead(&mut self) -> Result<Token, ParseErr> {
        if !matches!(self.lookahead_token, Token::EOF) {
            Err(ParseErr::BadUsage)
        } else {
            self.lookahead_token = if let Ok(token) = self.scan() {
                token
            } else {
                Token::EOF
            };

            Ok(self.lookahead_token.clone())
        }
    }

    #[inline]
    fn next(&mut self) -> Option<char> {
        self.current = self.source[self.read_offset..].chars().next();
        self.read_offset += 1;

        self.current
    }

    #[inline]
    fn save(&mut self, chr: char) {
        self.token_buffer.push(chr);
    }

    #[inline]
    fn save_next(&mut self) -> Option<char> {
        self.save(self.current?);
        self.next()
    }

    #[inline]
    fn clear(&mut self) {
        self.token_buffer.clear();
    }

    #[inline]
    fn new_line(&mut self) {
        if self.is_eol() {
            let old = self.current;
            self.next();

            if self.is_eol() && self.current != old {
                self.next();
            }

            self.line_number += 1;
        }
    }

    #[inline]
    fn is_eol(&self) -> bool {
        matches!(self.current, Some('\n' | '\r'))
    }

    #[inline]
    fn is_eof(&self) -> bool {
        self.current.is_none()
    }

    fn skip_eq(&mut self) -> i32 {
        let mut count = 0i32;
        let s = self.current;

        while matches!(self.save_next(), Some('=')) {
            count += 1;
        }

        if self.current == s {
            count
        } else {
            (-count) - 1
        }
    }

    fn parse_string(&mut self) -> Result<Token, &str> {
        let delim = self.current;
        self.save_next();

        while self.current != delim {
            match self.current {
                None | Some('\r' | '\n') => return Err("bad usage"),
                Some('\\') => {
                    let chr = match self.next() {
                        Some('n') => '\n',
                        Some('r') => '\r',
                        Some('t') => '\t',
                        Some('\\') => '\\',
                        Some('\'') => '\'',
                        Some('\"') => '\"',
                        Some('\n' | '\r') => {
                            self.save('\n');
                            self.new_line();
                            continue;
                        }
                        Some('z') => {
                            self.next();
                            loop {
                                let chr = if let Some(chr) = self.current {
                                    chr
                                } else {
                                    break;
                                };
                                if !chr.is_whitespace() {
                                    break;
                                }

                                if self.is_eol() {
                                    self.new_line();
                                } else {
                                    self.next();
                                }
                            }
                            continue;
                        }
                        Some('x') => {
                            ((self
                                .next()
                                .and_then(|x| x.to_digit(16))
                                .ok_or("bad usage")?
                                << 4)
                                | (self
                                    .next()
                                    .and_then(|x| x.to_digit(16))
                                    .ok_or("bad usage")?)) as u8 as char
                        }
                        Some('u') => {
                            if !matches!(self.next(), Some('{')) {
                                return Err("bad usage");
                            }
                            let mut chr = 0u32;
                            loop {
                                chr <<= 4;
                                chr += self
                                    .current
                                    .and_then(|c| c.to_digit(16))
                                    .ok_or("unexception hex")?;

                                if chr >= 0x110000 {
                                    return Err("unexception unicode");
                                }
                                if matches!(self.next(), Some('}')) {
                                    break;
                                }
                            }

                            if chr < 0x800 {
                                if chr < 0x80 {
                                    chr as u8 as char
                                } else {
                                    self.save((0xc0 | (chr >> 6)) as u8 as char);

                                    (0x80 | (chr & 0x3f)) as u8 as char
                                }
                            } else {
                                if chr >= 0x10000 {
                                    self.save((0xf0 | (chr >> 18)) as u8 as char);
                                    self.save((0x80 | ((chr >> 12) & 0x3f)) as u8 as char);
                                } else {
                                    if chr >= 0xd800 && chr < 0xe000 {
                                        return Err("unexception unicode");
                                    }
                                    self.save((0xe0 | (chr >> 12)) as u8 as char);
                                }
                                self.save((0x80 | ((chr >> 6) & 0x3f)) as u8 as char);

                                (0x80 | (chr & 0x3f)) as u8 as char
                            }
                        }
                        None => return Err("bad usage"),
                        _ => return Err("bad usage"),
                    };

                    self.save(chr);
                    self.next();
                }
                _ => {
                    self.save_next();
                }
            }
        }

        self.save_next();
        let value = self.token_buffer[1..self.token_buffer.len() - 1].to_string();
        self.token_buffer.clear();

        Ok(Token::String(value))
    }

    fn parse_longstring(&mut self, sep: i32, is_comment: bool) -> Result<Token, &str> {
        self.save_next();
        if self.is_eol() {
            self.new_line();
        }

        loop {
            match self.current {
                Some(']') => {
                    if self.skip_eq() == sep {
                        self.save_next();
                        break;
                    }
                }
                Some('\n' | '\r') => {
                    self.save('\n');
                    self.new_line();
                    if is_comment {
                        self.clear();
                    }
                }
                None => return Err("bad usage"),
                _ => {
                    self.save_next();
                }
            }
        }

        if is_comment {
            Ok(Token::Nil)
        } else {
            let value = self.token_buffer[1..self.token_buffer.len() - 1].to_string();
            self.token_buffer.clear();

            Ok(Token::String(value))
        }
    }

    fn scan(&mut self) -> Result<Token, &str> {
        self.token_buffer.clear();
        loop {
            if matches!(self.current, Some(chr) if chr.is_alphanumeric()) {
                if matches!(self.current, Some(chr) if chr.is_numeric()) {
                    return self.number();
                }

                // identifier or reversed word
                while matches!(self.current, Some(chr) if chr.is_alphanumeric() || chr == '_') {
                    self.save_next();
                }
                return self.identifier();
            }

            match self.current {
                None => return Ok(Token::EOF),
                Some('\r' | '\n') => {
                    self.new_line();
                    continue;
                }
                Some(' ' | '\t') => {
                    self.next();
                    continue;
                }
                Some('-') => {
                    self.next();
                    if !matches!(self.current, Some('-')) {
                        break Ok(Token::Operator('-'));
                    }
                    self.next();

                    if matches!(self.current, Some('[')) {
                        let sep = self.skip_eq();
                        self.clear();

                        if sep > 0 {
                            _ = self.parse_longstring(sep, true);
                            self.clear();
                            continue;
                        }
                    }
                }
                Some('[') => {
                    let sep = self.skip_eq();
                    if sep > 0 {
                        break self.parse_longstring(sep, false);
                    } else if sep == -1 {
                        break Ok(Token::Operator('['));
                    } else {
                        break Err("bad usage");
                    }
                }
                Some('\"' | '\'') => break self.parse_string(),
                Some('=') => {
                    self.next();

                    if matches!(self.current, Some('=')) {
                        self.next();
                        break Ok(Token::EQ);
                    }
                    break Ok(Token::Operator('='));
                }
                Some('<') => {
                    self.next();

                    match self.current {
                        Some('=') => {
                            self.next();
                            break Ok(Token::LE);
                        }
                        Some('<') => {
                            self.next();
                            break Ok(Token::SHL);
                        }
                        _ => break Ok(Token::Operator('<')),
                    }
                }
                Some('>') => {
                    self.next();

                    match self.current {
                        Some('=') => {
                            self.next();
                            break Ok(Token::GE);
                        }
                        Some('>') => {
                            self.next();
                            break Ok(Token::SHR);
                        }
                        _ => break Ok(Token::Operator('>')),
                    }
                }
                Some('/') => {
                    self.next();

                    match self.current {
                        Some('/') => {
                            self.next();
                            break Ok(Token::IDIV);
                        }
                        _ => break Ok(Token::Operator('/')),
                    }
                }
                Some('~') => {
                    self.next();

                    if matches!(self.current, Some('=')) {
                        self.next();
                        break Ok(Token::NE);
                    }
                    break Ok(Token::Operator('~'));
                }
                Some(':') => {
                    self.next();

                    if matches!(self.current, Some(':')) {
                        self.next();
                        break Ok(Token::Label);
                    }
                    break Ok(Token::Operator(':'));
                }
                Some('.') => {
                    if matches!(self.save_next(), Some('.')) {
                        self.next();
                        if matches!(self.current, Some('.')) {
                            self.next();
                            break Ok(Token::Dots);
                        }
                        break Ok(Token::Concat);
                    } else if !matches!(self.current, Some(chr) if chr.is_digit(10)) {
                        self.token_buffer.clear();
                        break Ok(Token::Operator('.'));
                    } else {
                        break self.number();
                    }
                }
                Some(chr) => {
                    self.next();

                    break Ok(Token::Operator(chr));
                }
            }
        }
    }

    fn number(&mut self) -> Result<Token, &str> {
        let mut allowable_symbol = true;
        let mut allowable_dot = true;
        let mut allowable_pflag = true;
        let mut allowable_eflag = true;

        loop {
            match self.current {
                Some('+' | '-') => {
                    if !allowable_symbol {
                        return Err("bad usage");
                    }

                    self.save_next();
                    allowable_symbol = false;
                }
                Some('.') => {
                    if !allowable_dot {
                        return Err("bad usage");
                    }
                    self.save_next();
                    allowable_dot = false;
                }
                Some('p') => {
                    if !allowable_pflag {
                        return Err("bad usage");
                    }
                    self.save_next();
                    allowable_symbol = true;
                    allowable_pflag = false;
                    allowable_eflag = false;
                    allowable_dot = true;
                }
                Some('e') => {
                    if !allowable_eflag {
                        return Err("bad usage");
                    }
                    self.save_next();
                    allowable_symbol = true;
                    allowable_pflag = false;
                    allowable_eflag = false;
                    allowable_dot = true;
                }
                _ => {
                    if self.current.is_none()
                        || matches!(self.current, Some(chr) if !chr.is_alphanumeric())
                    {
                        break self.parse_number(0, 10);
                    }
                    self.save_next();
                    allowable_symbol = false;
                }
            }
        }
    }

    fn parse_number(&mut self, start_off: usize, default_radix: u32) -> Result<Token, &str> {
        if self.token_buffer.eq_ignore_ascii_case("nan") {
            return Ok(Token::Number(f64::NAN));
        }

        let negative = self.token_buffer.starts_with("-");
        let mut off = if matches!(self.token_buffer.chars().next(), Some('+' | '-')) {
            start_off + 1
        } else {
            start_off
        };

        if self.token_buffer[off..].eq_ignore_ascii_case("inf")
            || self.token_buffer[off..].eq_ignore_ascii_case("infinity")
        {
            return Ok(Token::Number(if negative {
                f64::NEG_INFINITY
            } else {
                f64::INFINITY
            }));
        }

        let radix = if self.token_buffer[off..].starts_with("0") {
            off += 1;

            match self.token_buffer[off..].chars().next() {
                Some('B' | 'b') => {
                    off += 1;
                    2
                }
                Some('X' | 'x') => {
                    off += 1;
                    16
                }
                Some('O' | 'o') => {
                    off += 1;
                    8
                }
                _ => default_radix,
            }
        } else {
            default_radix
        };

        let must_integer = self.token_buffer.ends_with('u') || self.token_buffer.ends_with('i');

        let mut is_float = false;
        let mut int_value = 0i64;
        let mut float_value = 0f64;

        while let Some(chr) = self.token_buffer[off..].chars().next() {
            off += 1;

            match chr {
                'u' | 'i' => {
                    if !must_integer {
                        return Err("bad usage");
                    }
                }
                '.' => {
                    if must_integer {
                        return Err("bad usage");
                    }
                    is_float = true;
                    float_value = int_value as f64;

                    let mut base = 1f64;

                    while let Some(chr) = self.token_buffer[off..].chars().next() {
                        let digit = if let Some(digit) = chr.to_digit(radix) {
                            digit as f64
                        } else {
                            break;
                        };

                        base /= radix as f64;
                        float_value += base * digit;

                        off += 1;
                    }
                }
                'p' | 'P' => {
                    if must_integer {
                        return Err("bad usage");
                    }
                    if radix != 16 {
                        return Err("bad radix");
                    }
                    if !is_float {
                        float_value = int_value as f64;
                    }
                    is_float = true;

                    float_value = match self.parse_number(off, 16) {
                        Ok(Token::Number(value)) => float_value.powf(value),
                        Ok(Token::Integer(value)) => float_value.powi(value as i32),
                        _ => return Err("bad p"),
                    };

                    break;
                }
                'e' | 'E' => {
                    if must_integer {
                        return Err("bad usage");
                    }
                    if radix != 10 {
                        return Err("bad radix");
                    }
                    if !is_float {
                        float_value = int_value as f64;
                    }
                    is_float = true;

                    float_value = match self.parse_number(off, 10) {
                        Ok(Token::Number(value)) => float_value.powf(value),
                        Ok(Token::Integer(value)) => float_value.powi(value as i32),
                        _ => return Err("bad p"),
                    };

                    break;
                }
                _ => {
                    if let Some(digit) = chr.to_digit(radix) {
                        int_value = int_value * radix as i64 + digit as i64;
                    }
                }
            }
        }

        self.token_buffer.clear();

        Ok(if is_float {
            Token::Number(if negative { -float_value } else { float_value })
        } else {
            Token::Integer(if negative { -int_value } else { int_value })
        })
    }

    fn identifier(&mut self) -> Result<Token, &str> {
        let identifier = match self.token_buffer.as_str() {
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
            "not" => Token::Not,
            "and" => Token::And,
            "or" => Token::Or,
            "nil" => Token::Nil,
            "true" => Token::True,
            "false" => Token::False,
            "in" => Token::In,
            "then" => Token::Then,
            _ => Token::Name(self.token_buffer.clone()),
        };

        self.token_buffer.clear();

        Ok(identifier)
    }

    pub(crate) fn get_lastline(&self) -> usize {
        self.last_line
    }
}
