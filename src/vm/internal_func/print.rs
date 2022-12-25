use std::ops::Deref;

use crate::{object::Value, state::LuaState, vm::ExecError};

pub(super) fn internal_print(state: &LuaState) -> Result<(), ExecError> {
    for i in 1..(state.nparams() + 1) {
        if i != 1 {
            print!(" ");
        }

        match state.get(i).get().deref() {
            Value::String(v) => print!("{}", v),
            Value::Integer(v) => print!("{}", v),
            Value::Number(v) => print!("{}", v),
            Value::Boolean(v) => print!("{}", v),
            Value::Nil => break,
            _ => {}
        }
    }

    println!();

    Ok(())
}
