use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use super::LocVar;

pub(crate) struct PrototypeContent {
    pub(crate) vararg: bool,
    pub(crate) p: Vec<Prototype>,

    pub(crate) locvars: Vec<LocVar>,
}

pub(crate) struct Prototype(Rc<RefCell<PrototypeContent>>);

impl Prototype {
    pub(crate) fn new() -> Self {
        Self(Rc::new(RefCell::new(PrototypeContent {
            vararg: false,
            p: Vec::new(),

            locvars: Vec::new(),
        })))
    }

    pub(crate) fn prop(&self) -> Ref<PrototypeContent> {
        self.0.as_ref().borrow()
    }

    pub(crate) fn prop_mut(&mut self) -> RefMut<PrototypeContent> {
        self.0.as_ref().borrow_mut()
    }
}
