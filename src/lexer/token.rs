use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Token {
    EOF,
    Number(f64),
    Integer(i64),
    Operator(char),
    String(String),

    Label,

    Dots,
    Concat,

    EQ,
    NE,
    LE,
    GE,

    IDIV,
    SHL,
    SHR,

    Else,
    ElseIf,
    End,
    Until,
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
    Not,
    And,
    Or,
    Nil,
    True,
    Then,
    False,
    Name(String),

    In,
}

impl Token {
    pub(crate) fn block_follow(&self, withuntil: bool) -> bool {
        match self {
            Token::Else | Token::ElseIf | Token::End | Token::EOF => true,
            Token::Until => withuntil,
            _ => false,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::EOF => write!(f, "Token_EOF"),
            Token::Number(value) => write!(f, "Token_Number({})", *value),
            Token::Integer(value) => write!(f, "Token_Integer({})", *value),
            Token::Operator(value) => write!(f, "Token_Operator({})", *value),
            Token::String(value) => write!(f, "Token_String({})", *value),
            Token::Name(value) => write!(f, "Token_Name({})", *value),
            Token::Label => write!(f, "Token_Label"),
            Token::Dots => write!(f, "Token_Dots"),
            Token::Concat => write!(f, "Token_Concat"),
            Token::EQ => write!(f, "Token_EQ"),
            Token::NE => write!(f, "Token_NE"),
            Token::LE => write!(f, "Token_LE"),
            Token::GE => write!(f, "Token_GE"),
            Token::IDIV => write!(f, "Token_IDIV"),
            Token::SHL => write!(f, "Token_SHL"),
            Token::Else => write!(f, "Token_Else"),
            Token::ElseIf => write!(f, "Token_ElseIf"),
            Token::End => write!(f, "Token_End"),
            Token::Until => write!(f, "Token_Until"),
            Token::If => write!(f, "Token_If"),
            Token::While => write!(f, "Token_While"),
            Token::Do => write!(f, "Token_Do"),
            Token::For => write!(f, "Token_For"),
            Token::Repeat => write!(f, "Token_Repeat"),
            Token::Function => write!(f, "Token_Function"),
            Token::Local => write!(f, "Token_Local"),
            Token::Return => write!(f, "Token_Return"),
            Token::Break => write!(f, "Token_Break"),
            Token::Goto => write!(f, "Token_Goto"),
            Token::Not => write!(f, "Token_Not"),
            Token::And => write!(f, "Token_And"),
            Token::Or => write!(f, "Token_Or"),
            Token::Nil => write!(f, "Token_Nil"),
            Token::True => write!(f, "Token_True"),
            Token::False => write!(f, "Token_False"),
            Token::Then => write!(f, "Token_Then"),
            Token::In => write!(f, "Token_In"),
            _ => write!(f, "Token_unknow"),
        }
    }
}
