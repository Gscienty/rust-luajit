use std::{rc::Rc, cell::RefCell};

pub(super) mod scope_flags {
    pub(super) const LOOP: u8 = 0x01;
    pub(super) const BREAK: u8 = 0x02;
    pub(super) const GOTO_LABEL: u8 = 0x04;
    pub(super) const UPVAL: u8 = 0x08;
    pub(super) const NOCLOSE: u8 = 0x10;
}

pub(crate) struct Scope {
    pub(super) prev: Option<Rc<RefCell<Scope>>>,

    pub(super) start: u32,
    pub(super) active_var_count: u32,
    pub(super) flags: u8,
}

impl Scope {
    pub(super) fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(
            Scope {
                prev: None,

                start: 0,
                active_var_count: 0,
                flags: 0,
            }
        ))
    }
}