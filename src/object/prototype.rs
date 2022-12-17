use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use crate::code::InterCode;

use super::{LocVar, Upval};

#[derive(Clone)]
pub(crate) struct PrototypeContent {
    pub(crate) nparams: usize,
    pub(crate) vararg: bool,

    pub(crate) locvars: Vec<LocVar>,
    pub(crate) upvars: Vec<Upval>,
    pub(crate) children_proto: Vec<Prototype>,

    pub(crate) codes: Vec<InterCode>,
}

#[derive(Clone)]
pub struct Prototype(Rc<RefCell<PrototypeContent>>);

impl Prototype {
    pub(crate) fn new() -> Self {
        let p = Self(Rc::new(RefCell::new(PrototypeContent {
            nparams: 0,
            vararg: false,
            children_proto: Vec::new(),

            locvars: Vec::new(),
            upvars: Vec::new(),

            codes: Vec::new(),
        })));

        let inp = p.clone();

        p.prop_mut().children_proto.push(inp);

        p
    }

    pub(crate) fn prop(&self) -> Ref<PrototypeContent> {
        self.0.as_ref().borrow()
    }

    pub(crate) fn prop_mut(&self) -> RefMut<PrototypeContent> {
        self.0.as_ref().borrow_mut()
    }
}

impl PartialEq for Prototype {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}
