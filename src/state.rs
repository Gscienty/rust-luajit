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
    vm::{ExecError, VMExec},
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
        let env = Table::new();
        let callinfo = CallInfo::new(1, CallFunc::LuaFunc(proto.clone()));

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
    use std::ops::Deref;

    use crate::utils::Logger;

    use super::*;

    fn test_print(state: &LuaState) -> Result<(), ExecError> {
        for i in 0..state.nparams() {
            if i != 0 {
                print!(" ");
            }

            match state.get(i).get().deref() {
                Value::String(v) => print!("{}", v),
                Value::Integer(v) => print!("{}", v),
                Value::Number(v) => print!("{}", v),
                Value::Boolean(v) => print!("{}", v),
                Value::Nil => break,
                _ => {}
            }
        }

        println!();

        Ok(())
    }

    #[test]
    fn test_compile() {
        log::set_logger(&Logger {}).unwrap();
        log::set_max_level(log::LevelFilter::Debug);

        let mut state = LuaState::new();
        let ret = state.parse(
            "
            function add(a, b)
                return a + b;
            end
            local a = 'hello world';

            if #a ~= 10 then
                local c = add(10, 5);
                print(c);
            else
                local c = add(7, 1);
                print(c);
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

        state.register("print", test_print);

        _ = state.exec();
    }
}
