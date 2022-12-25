use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use super::{Prototype, RefValue, RustFunc};

#[derive(Clone)]
pub enum CallFunc {
    LuaFunc(Prototype),
    RustFunc(RustFunc),
}

impl PartialEq for CallFunc {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

#[derive(Clone)]
pub(crate) struct CallInfoContent {
    pub(crate) prev: Option<CallInfo>,

    pub(crate) pc: usize,
    pub(crate) regbase: usize,

    pub(crate) nparams: usize,
    pub(crate) nresults: usize,

    pub(crate) func: CallFunc,
}

#[derive(Clone)]
pub(crate) struct CallInfo(Rc<RefCell<CallInfoContent>>);

impl CallInfo {
    pub(crate) fn new(regbase: usize, func: CallFunc) -> Self {
        Self(Rc::new(RefCell::new(CallInfoContent {
            prev: None,

            pc: 0,
            regbase,

            nparams: 0,
            nresults: 0,

            func,
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
    pub(crate) fn new(callinfo: CallInfo, env: RefValue) -> Self {
        Self {
            reg: vec![env],
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

    pub(crate) fn ret(&mut self, regidx: usize, value: RefValue) {
        let regidx = self.callinfo.prop().regbase + regidx - 1;

        self.set_abs(regidx, value)
    }
}
