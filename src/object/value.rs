use std::{
    cell::{Ref, RefCell, RefMut},
    ops::{Deref, DerefMut},
    rc::Rc,
};

use super::Table;

#[derive(Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Number(f64),
    Boolean(bool),
    String(String),
    Table(Table),

    Nil,
}

#[derive(Clone, PartialEq)]
pub struct RefValue(pub(crate) Rc<RefCell<Value>>);

impl RefValue {
    pub fn new() -> Self {
        RefValue(Rc::new(RefCell::new(Value::Nil)))
    }

    #[inline]
    pub fn is_void(&self) -> bool {
        matches!(self.get().deref(), Value::Nil)
    }

    #[inline]
    pub fn get(&self) -> Ref<Value> {
        self.0.borrow()
    }

    #[inline]
    pub fn get_mut(&mut self) -> RefMut<Value> {
        self.0.borrow_mut()
    }

    #[inline]
    pub fn modify<U, F>(&mut self, f: F) -> U
    where
        F: FnOnce(&mut Value) -> U,
    {
        f(self.get_mut().deref_mut())
    }

    #[inline]
    pub fn integer(&self) -> Option<i64> {
        match self.get().deref() {
            Value::Integer(val) => Some(*val),
            _ => None,
        }
    }

    #[inline]
    pub fn number(&self) -> Option<f64> {
        match self.get().deref() {
            Value::Number(val) => Some(*val),
            _ => None,
        }
    }

    #[inline]
    pub fn boolean(&self) -> Option<bool> {
        match self.get().deref() {
            Value::Boolean(val) => Some(*val),
            _ => None,
        }
    }

    #[inline]
    pub fn string(&self) -> Option<String> {
        match self.get().deref() {
            Value::String(val) => Some(String::from(val)),
            _ => None,
        }
    }

    #[inline]
    pub fn table_mut<U, F>(&mut self, f: F) -> Option<U>
    where
        F: FnOnce(&mut Table) -> U,
    {
        match self.get_mut().deref_mut() {
            Value::Table(table) => Some(f(table)),
            _ => None,
        }
    }

    #[inline]
    pub fn table<U, F>(&self, f: F) -> Option<U>
    where
        F: FnOnce(&Table) -> U,
    {
        match self.get().deref() {
            Value::Table(table) => Some(f(table)),
            _ => None,
        }
    }
}

impl From<i64> for RefValue {
    fn from(value: i64) -> Self {
        RefValue(Rc::new(RefCell::new(Value::Integer(value))))
    }
}

impl From<f64> for RefValue {
    fn from(value: f64) -> Self {
        RefValue(Rc::new(RefCell::new(Value::Number(value))))
    }
}

impl From<bool> for RefValue {
    fn from(value: bool) -> Self {
        RefValue(Rc::new(RefCell::new(Value::Boolean(value))))
    }
}

impl From<&str> for RefValue {
    fn from(value: &str) -> Self {
        RefValue(Rc::new(RefCell::new(Value::String(String::from(value)))))
    }
}