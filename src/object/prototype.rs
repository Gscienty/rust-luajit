use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use crate::code::Code;

use super::{LocVar, UpvalDesc};

pub(crate) struct PrototypeContent {
    pub(crate) is_vararg: bool,
    pub(crate) nparams: u32,

    pub(crate) code: Vec<Code>,

    pub(crate) upvalues: Vec<UpvalDesc>,
    pub(crate) locvars: Vec<LocVar>,
}

pub(crate) struct Prototype(Rc<RefCell<PrototypeContent>>);

impl Prototype {
    pub(crate) fn new() -> Self {
        Self(Rc::new(RefCell::new(PrototypeContent {
            is_vararg: false,
            nparams: 0,

            code: Vec::new(),

            upvalues: Vec::new(),
            locvars: Vec::new(),
        })))
    }

    pub(crate) fn prop(&self) -> Ref<PrototypeContent> {
        self.0.as_ref().borrow()
    }

    pub(crate) fn prop_mut(&mut self) -> RefMut<PrototypeContent> {
        self.0.as_ref().borrow_mut()
    }

    pub(crate) fn emit(&mut self, code: Code) -> usize {
        self.prop_mut().code.push(code);

        self.prop().code.len()
    }

    pub(crate) fn pop_code(&mut self) {
        self.prop_mut().code.pop();
    }

    pub(crate) fn code<U, F>(&self, pc: usize, f: F) -> Option<U>
    where
        F: FnOnce(&Code) -> U,
    {
        self.prop().code.get(pc).and_then(|code| Some(f(code)))
    }

    pub(crate) fn code_mut<U, F>(&mut self, pc: usize, f: F) -> Option<U>
    where
        F: FnOnce(&mut Code) -> U,
    {
        self.prop_mut()
            .code
            .get_mut(pc)
            .and_then(|code| Some(f(code)))
    }

    pub(crate) fn push_upvalue(&mut self, upval: UpvalDesc) -> usize {
        self.prop_mut().upvalues.push(upval);

        self.prop().upvalues.len() - 1
    }

    pub(crate) fn register_locvar(&mut self, name: &str, pc: usize) -> usize {
        self.prop_mut().locvars.push(LocVar {
            name: String::from(name),
            start_pc: pc,
            end_pc: 0,
        });

        self.prop().locvars.len() - 1
    }
}
