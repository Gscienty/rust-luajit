use std::{cell::Ref, ops::Deref};

use crate::object::{RefValue, Value};

use super::Parser;

pub(super) struct ParseGTab<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseGTab<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
    }

    pub(super) fn get(&self, key: Ref<Value>) -> Option<usize> {
        match self
            .p
            .global(|t| t.get(key))
            .and_then(|v| Some(v.get().clone()))
        {
            Some(Value::Integer(value)) => Some(value as usize),
            _ => None,
        }
    }

    pub(super) fn new_reg(&mut self, key: Ref<Value>) -> usize {
        match key.deref() {
            Value::Integer(_) | Value::Number(_) => {
                let reg = self.p.nkn;
                self.p.nkn += 1;

                reg
            }
            _ => {
                let reg = self.p.nkgc;
                self.p.nkgc += 1;

                reg
            }
        }
    }

    pub(super) fn new_val(&mut self, key: RefValue) -> usize {
        let reg = self.new_reg(key.get());
        self.p
            .global_mut(|t| t.set(key, RefValue::from(reg as i64)));

        reg
    }

    pub(super) fn set_val(&mut self, key: RefValue) -> usize {
        match self.get(key.get()) {
            Some(reg) => reg,
            _ => self.new_val(key),
        }
    }
}
