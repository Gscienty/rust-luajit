use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use crate::code::InterCode;

use super::{LocVar, Upval};

#[derive(Clone)]
pub(crate) struct PrototypeContent {
    pub(crate) vararg: bool,

    pub(crate) locvars: Vec<LocVar>,
    pub(crate) upvars: Vec<Upval>,

    pub(crate) codes: Vec<InterCode>,
}

#[derive(Clone)]
pub(crate) struct Prototype(Rc<RefCell<PrototypeContent>>);

impl Prototype {
    pub(crate) fn new() -> Self {
        Self(Rc::new(RefCell::new(PrototypeContent {
            vararg: false,

            locvars: Vec::new(),
            upvars: Vec::new(),

            codes: Vec::new(),
        })))
    }

    pub(crate) fn prop(&self) -> Ref<PrototypeContent> {
        self.0.as_ref().borrow()
    }

    pub(crate) fn prop_mut(&mut self) -> RefMut<PrototypeContent> {
        self.0.as_ref().borrow_mut()
    }
}
