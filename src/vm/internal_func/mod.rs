use std::ops::DerefMut;

use crate::object::{RefValue, RustFunc, Table, Value};

mod ipairs;
mod print;

fn set_func(table: &mut Table, name: &str, func: RustFunc) {
    table.set(RefValue::from(name), RefValue::from(func))
}

pub(crate) fn register(table: &mut RefValue) {
    match table.get_mut().deref_mut() {
        Value::Table(table) => {
            set_func(table, "print", print::internal_print);
            set_func(table, "ipairs", ipairs::internal_ipairs);
        }
        _ => unreachable!(),
    }
}
