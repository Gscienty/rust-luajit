use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use crate::object::Prototype;

use super::Block;

pub(super) struct FuncStateContent {
    pub(super) prev: Option<FuncState>,
    pub(super) proto: Prototype,
    pub(super) block: Block,

    pub(super) nk: usize,
    pub(super) nactvar: usize,
    pub(super) nups: usize,

    pub(super) first_local: usize,
    pub(super) first_label: usize,

    pub(super) freereg: usize,

    pub(super) needclose: bool,
}

#[derive(Clone)]
pub(super) struct FuncState(Rc<RefCell<FuncStateContent>>);

impl FuncState {
    pub(super) fn new() -> Self {
        FuncState(Rc::new(RefCell::new(FuncStateContent {
            prev: None,
            proto: Prototype::new(),
            block: Block::new(),

            nk: 0,
            nactvar: 0,
            nups: 0,

            first_local: 0,
            first_label: 0,

            freereg: 0,

            needclose: false,
        })))
    }

    pub(super) fn prop(&self) -> Ref<FuncStateContent> {
        self.0.as_ref().borrow()
    }

    pub(super) fn prop_mut(&self) -> RefMut<FuncStateContent> {
        self.0.as_ref().borrow_mut()
    }

    pub(super) fn is_global(&self) -> bool {
        matches!(self.prop().prev, None)
    }
}
