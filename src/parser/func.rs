use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use crate::{
    code::{codelimit, Code, OpCode},
    object::Prototype,
};

use super::{Block, ParseErr};

pub(super) struct FuncStateContent {
    pub(super) proto: Prototype,
    pub(super) prev: Option<FuncState>,
    pub(super) block: Option<Block>,

    pub(super) pc: usize,
    pub(super) last_target: usize,

    pub(super) nparams: usize,
    pub(super) nactvar: usize,
    pub(super) freereg: u32,

    pub(super) firstlocal: usize,

    pub(super) needclose: bool,
}

#[derive(Clone)]
pub(super) struct FuncState(Rc<RefCell<FuncStateContent>>);

impl FuncState {
    pub(super) fn new() -> Self {
        FuncState(Rc::new(RefCell::new(FuncStateContent {
            proto: Prototype::new(),
            prev: None,
            block: None,

            pc: 0,
            last_target: codelimit::NO_JMP,

            nparams: 0,
            nactvar: 0,
            freereg: 0,

            firstlocal: 0,

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

    pub(super) fn setvararg(&mut self, nparams: u32) -> Result<(), ParseErr> {
        let proto = &mut self.prop_mut().proto;

        let code = Code::new_abc(OpCode::VARARGPREP, nparams, 0, 0, false)?;
        proto.prop_mut().is_vararg = true;
        proto.emit(code);

        Ok(())
    }

    pub(super) fn emit(&mut self, code: Code) -> usize {
        self.prop_mut().pc = self.prop_mut().proto.emit(code);

        self.prop().pc - 1
    }

    pub(super) fn pop_code(&mut self) {
        self.prop_mut().proto.pop_code();

        self.prop_mut().pc -= 1;
    }

    pub(crate) fn code<U, F>(&self, pc: usize, f: F) -> Option<U>
    where
        F: FnOnce(&Code) -> U,
    {
        self.prop().proto.code(pc, f)
    }

    pub(crate) fn code_mut<U, F>(&mut self, pc: usize, f: F) -> Option<U>
    where
        F: FnOnce(&mut Code) -> U,
    {
        self.prop_mut().proto.code_mut(pc, f)
    }

    pub(crate) fn register_locvar(&mut self, name: &str) -> usize {
        self.prop_mut().proto.register_locvar(name, self.prop().pc)
    }
}
