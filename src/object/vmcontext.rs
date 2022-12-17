use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use super::{Prototype, RefValue, Table};

#[derive(Clone)]
pub(crate) enum CallFunc {
    LuaFunc(Prototype),
}

#[derive(Clone)]
pub(crate) struct CallInfoContent {
    pub(crate) prev: Option<CallInfo>,

    pub(crate) pc: usize,
    pub(crate) regbase: usize,

    pub(crate) nresults: usize,

    pub(crate) func: CallFunc,
}

#[derive(Clone)]
pub(crate) struct CallInfo(Rc<RefCell<CallInfoContent>>);

impl CallInfo {
    pub(crate) fn new_luacall(regbase: usize, proto: Prototype) -> Self {
        Self(Rc::new(RefCell::new(CallInfoContent {
            prev: None,

            pc: 0,
            regbase,

            nresults: 0,

            func: CallFunc::LuaFunc(proto),
        })))
    }

    pub(crate) fn prop(&self) -> Ref<CallInfoContent> {
        self.0.borrow()
    }

    pub(crate) fn prop_mut(&self) -> RefMut<CallInfoContent> {
        self.0.borrow_mut()
    }
}

pub(crate) struct VMContext {
    pub(crate) reg: Vec<RefValue>,

    pub(crate) callinfo: CallInfo,
}

impl VMContext {
    pub(crate) fn new(callinfo: CallInfo) -> Self {
        Self {
            reg: vec![Table::new()],
            callinfo,
        }
    }

    pub(crate) fn get_abs(&self, regidx: usize) -> RefValue {
        if let Some(stored) = self.reg.get(regidx) {
            stored.clone()
        } else {
            RefValue::new()
        }
    }

    pub(crate) fn set_abs(&mut self, regidx: usize, value: RefValue) {
        if let Some(stored) = self.reg.get_mut(regidx) {
            *stored = value;
        } else if self.reg.len() == 0 {
            self.reg.push(value);
        } else {
            while regidx < self.reg.len() - 1 {
                self.reg.push(RefValue::new());
            }
            self.reg.push(value);
        }
    }

    pub(crate) fn get(&self, regidx: usize) -> RefValue {
        let regidx = self.callinfo.prop().regbase + regidx;

        self.get_abs(regidx)
    }

    pub(crate) fn set(&mut self, regidx: usize, value: RefValue) {
        let regidx = self.callinfo.prop().regbase + regidx;

        self.set_abs(regidx, value)
    }
}
