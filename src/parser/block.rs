use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

pub(super) struct BlockContent {
    pub(super) prev: Option<Block>,

    pub(super) nactvar: usize,
    pub(super) upval: bool,
    pub(super) firstlabel: usize,
    pub(super) firstgoto: usize,
    pub(super) is_loop: bool,
    pub(super) inside_tobeclosed: bool,
    pub(super) needclose: bool,
}

#[derive(Clone)]
pub(super) struct Block(pub(crate) Rc<RefCell<BlockContent>>);

impl Block {
    pub(super) fn new() -> Self {
        Block(Rc::new(RefCell::new(BlockContent {
            prev: None,

            nactvar: 0,
            upval: false,
            firstlabel: 0,
            firstgoto: 0,
            is_loop: false,
            inside_tobeclosed: false,
            needclose: false,
        })))
    }

    pub(super) fn prop(&self) -> Ref<BlockContent> {
        self.0.as_ref().borrow()
    }

    pub(super) fn prop_mut(&self) -> RefMut<BlockContent> {
        self.0.as_ref().borrow_mut()
    }
}
