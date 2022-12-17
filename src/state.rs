use std::{cell::RefCell, rc::Rc};

use crate::{
    errors::LuaError,
    object::{ConstantPool, Prototype},
    parser::Parser,
    vm::{ExecError, VMExec},
};

pub struct LuaState {
    pub(crate) proto: Prototype,
    pub(crate) constant_pool: Rc<RefCell<ConstantPool>>,
}

impl LuaState {
    pub fn new() -> Self {
        Self {
            proto: Prototype::new(),
            constant_pool: Rc::new(RefCell::new(ConstantPool::new())),
        }
    }

    pub fn parse(&mut self, source: &str) -> Result<(), LuaError> {
        Parser::new(source, self.proto.clone(), self.constant_pool.clone())
            .parse()
            .or(Err(LuaError::ParseError))
    }

    pub fn exec(&self) -> Result<(), ExecError> {
        let mut exec = VMExec::new(self);

        exec.exec()
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
            function add(a, b)
                return a + b;
            end

            function sub(a, b)
                return add(a - b, a + b) - a - b;
            end

            local a = 'hello world';
            local b = 10;
            if #a == 11 then
                b = add(b, 2) + 100;
            else 
                b = sub(b, 3) + 100;
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
