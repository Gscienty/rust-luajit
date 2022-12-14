#![allow(dead_code)]

mod utils;

mod code;
mod errors;
mod lexer;
mod object;
mod parser;
mod state;
mod vm;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
