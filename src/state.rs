use std::{cell::RefCell, rc::Rc};

use crate::{
    errors::LuaError,
    object::{ConstantPool, Prototype},
    parser::Parser,
};

pub struct LuaState {
    proto: Prototype,
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
}

#[cfg(test)]
mod tests {
    use crate::{object::VMContext, utils::Logger, vm::VMExec};

    use super::*;

    #[test]
    fn test_compile() {
        log::set_logger(&Logger {}).unwrap();
        log::set_max_level(log::LevelFilter::Debug);

        let mut state = LuaState::new();
        let ret = state.parse(
            "
            local a = 'hello world';
            local b = 10;
            if #a == 11 then
                b = b + 5;
            else 
                b = b + 10;
            end
        ",
        );

        assert!(ret.is_ok());

        for proto in &state.proto.prop().children_proto {
            println!("#######################");
            let mut ci = 0;
            for c in &proto.prop().codes {
                ci += 1;

                println!("{}\t{}", ci, c);
            }
        }

        let mut ctx = &mut VMContext::new();

        loop {
            println!("pc: {}", ctx.pc);

            if let Some(code) = state.proto.prop().codes.get(ctx.pc) {
                if VMExec::new(&state, &mut ctx).exec(code).is_err() {
                    println!("occur err");
                    break;
                }
            } else {
                break;
            };

            let mut off = 0;
            for reg in ctx.reg.iter() {
                println!("reg #{}: {}", off, reg);
                off += 1;
            }
            println!("pc: {}", ctx.pc);
        }
    }
}
