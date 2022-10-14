use crate::types::Value;

#[derive(Clone, PartialEq)]
pub(crate) enum Token {
    Name(String),
    String(String),
    Number(Value),
    SingleChar(char),
    EQ,
    LE,
    GE,
    NE,
    Label,
    Comment,
    Dots,
    Concat,
    EOF,

    Else,
    ElseIf,
    End,
    Until,

    Semicolon,

    If,
    While,
    Do,
    For,
    Repeat,
    Function,
    Local,
    Return,
    Break,
    Goto,
}