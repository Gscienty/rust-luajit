#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Value {
    Nil,
    Boolean(bool),
    Integer(i64),
    Number(f64),
}