use std::{rc::Rc, cell::RefCell};

pub(super) mod scope_flags {
    pub const LOOP: u8 = 0x01;
    pub const BREAK: u8 = 0x02;
    pub const GOTO_LABEL: u8 = 0x04;
    pub const UPVAL: u8 = 0x08;
    pub const NOCLOSE: u8 = 0x10;
}

pub(crate) struct Scope {
    pub(super) prev: Option<Rc<RefCell<Scope>>>,

    pub(super) var_start: u32,
    pub(super) active_var_count: u32,
    pub(super) flags: u8,
}

impl Scope {
    pub(super) fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(
            Scope {
                prev: None,

                var_start: 0,
                active_var_count: 0,
                flags: 0,
            }
        ))
    }
}