use crate::{errors::LuaError, object::Prototype, parser::Parser};

pub struct LuaState {
    proto: Prototype,
}

impl LuaState {
    pub fn new() -> Self {
        Self {
            proto: Prototype::new(),
        }
    }

    pub fn parse(&self, source: &str) -> Result<(), LuaError> {
        Parser::new(source, self.proto.clone())
            .parse()
            .or(Err(LuaError::ParseError))
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

        let state = LuaState::new();

        let ret = state.parse(
            "
            function add(a, b)
                return a + b + 1;
            end

            function sub(a, b)
                return add(a, b);
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
    }
}
