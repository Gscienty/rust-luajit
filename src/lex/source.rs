use std::str::Chars;

pub(crate) struct LuaSource<'s> {
    current: Option<char>,
    read_ptr: Chars<'s>,
}

impl<'s> LuaSource<'s> {
    pub(crate) fn source(src: &'s str) -> Self {
        LuaSource {
            current: None,
            read_ptr: src.chars(),
        }
    }

    pub(crate) fn setup(&mut self) {
        self.next();
    }

    pub(crate) fn next(&mut self) -> Option<char> {
        self.current = self.read_ptr.next();

        self.get()
    }

    pub(crate) fn get(&self) -> Option<char> {
        self.current
    }

    pub(crate) fn is_eof(&self) -> bool {
        self.current.is_none()
    }

    pub(crate) fn is_eol(&self) -> bool {
        matches!(self.get(), Some('\n') | Some('\r'))
    }
    
    pub(crate) fn is_digit(&self, radix: u32) -> bool {
        if let Some(chr) = self.get() {
            chr.is_digit(radix)
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_source_readnext() {
        let mut source = LuaSource::source("hello world");

        assert_eq!(source.next(), Some('h'));
        assert_eq!(source.next(), Some('e'));
        assert_eq!(source.next(), Some('l'));
        assert_eq!(source.next(), Some('l'));
        assert_eq!(source.next(), Some('o'));
        assert_eq!(source.next(), Some(' '));
        assert_eq!(source.next(), Some('w'));
        assert_eq!(source.next(), Some('o'));
        assert_eq!(source.next(), Some('r'));
        assert_eq!(source.next(), Some('l'));
        assert_eq!(source.next(), Some('d'));
        assert_eq!(source.next(), None);
    }
}