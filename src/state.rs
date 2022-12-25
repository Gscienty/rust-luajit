use std::{
    cell::{RefCell, RefMut},
    ops::DerefMut,
    rc::Rc,
};

use crate::{
    errors::LuaError,
    object::{
        CallFunc, CallInfo, ConstantPool, Prototype, RefValue, RustFunc, Table, VMContext, Value,
    },
    parser::Parser,
    vm::{self, ExecError, VMExec},
};

pub struct LuaState {
    pub(crate) proto: Prototype,
    pub(crate) constant_pool: Rc<RefCell<ConstantPool>>,

    pub(crate) env: RefValue,
    pub(crate) ctx: Rc<RefCell<VMContext>>,
}

impl LuaState {
    pub fn new() -> Self {
        let proto = Prototype::new();
        let mut env = Table::new();
        let callinfo = CallInfo::new(1, CallFunc::LuaFunc(proto.clone()));

        vm::register(&mut env);

        Self {
            env: env.clone(),
            proto,
            constant_pool: Rc::new(RefCell::new(ConstantPool::new())),

            ctx: Rc::new(RefCell::new(VMContext::new(callinfo, env.clone()))),
        }
    }

    pub fn parse(&mut self, source: &str) -> Result<(), LuaError> {
        Parser::new(source, self.proto.clone(), self.constant_pool.clone())
            .parse()
            .or(Err(LuaError::ParseError))
    }

    pub fn exec(&mut self) -> Result<(), ExecError> {
        let mut exec = VMExec::new(self);

        exec.exec()
    }

    pub fn register(&mut self, funcname: &str, func: RustFunc) {
        let key = RefValue::from(funcname);
        let func = RefValue::from(func);

        match self.env.get_mut().deref_mut() {
            Value::Table(table) => table.set(key, func),
            _ => unreachable!(),
        }
    }

    pub fn get(&self, off: usize) -> RefValue {
        self.ctx.borrow().get(off)
    }

    pub fn set(&self, off: usize, value: RefValue) {
        self.ctx.borrow_mut().set(off, value)
    }

    pub fn nparams(&self) -> usize {
        self.ctx.borrow().callinfo.prop().nparams
    }

    pub(crate) fn get_ctx(&self) -> RefMut<VMContext> {
        self.ctx.borrow_mut()
    }
}

#[cfg(test)]
mod tests {

    use crate::utils::Logger;

    use super::*;

    #[test]
    fn test_compile() {
        log::set_logger(&Logger {}).unwrap();
        log::set_max_level(log::LevelFilter::Debug);

        let mut state = LuaState::new();
        let ret = state.parse(
            "
            local table = { 
                a = 1,
                b = 2,
                c = 3,
            };

            for key, value in pairs(table) do
                print(key, value)
            end
        ",
        );

        assert!(ret.is_ok());

        for proto in &state.proto.prop().children_proto {
            println!("#######################");
            let mut ci = 0;
            for c in &proto.prop().codes {
                println!("{}\t{}", ci, c);
                ci += 1;
            }
        }

        _ = state.exec();
    }
}
