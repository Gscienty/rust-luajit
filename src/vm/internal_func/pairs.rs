use std::ops::Deref;

use crate::{
    object::{RefValue, RustFunc, Value},
    state::LuaState,
    vm::ExecError,
};

fn internal_ipairs_aux(state: &LuaState) -> Result<(), ExecError> {
    if let Value::Table(table) = state.get(1).get().deref() {
        if let Value::Integer(off) = state.get(2).get().deref() {
            let key = RefValue::from(*off + 1);

            if let Some(value) = table.get(key.get()) {
                state.set(1, key);
                state.set(2, value);
            } else {
                state.set(1, RefValue::new());
                state.set(2, RefValue::new());
            }

            Ok(())
        } else {
            Err(ExecError::BadOperand)
        }
    } else {
        Err(ExecError::BadOperand)
    }
}

pub(super) fn internal_ipairs(state: &LuaState) -> Result<(), ExecError> {
    state.set(0, RefValue::from(internal_ipairs_aux as RustFunc));
    state.set(2, RefValue::from(0)); // initial value

    Ok(())
}

fn internal_pairs_aux(state: &LuaState) -> Result<(), ExecError> {
    if let Value::Table(table) = state.get(1).get().deref() {
        let key = state.get(2);
        if let Some((key, value)) = table.next(key.get()) {
            state.set(1, key);
            state.set(2, value);
        } else {
            state.set(1, RefValue::new());
            state.set(2, RefValue::new());
        }

        Ok(())
    } else {
        Err(ExecError::BadOperand)
    }
}

pub(super) fn internal_pairs(state: &LuaState) -> Result<(), ExecError> {
    state.set(0, RefValue::from(internal_pairs_aux as RustFunc));
    state.set(2, RefValue::new());

    Ok(())
}
