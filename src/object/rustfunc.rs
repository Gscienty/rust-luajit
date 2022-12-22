use crate::{state::LuaState, vm::ExecError};

pub type RustFunc = fn(state: &LuaState) -> Result<(), ExecError>;
