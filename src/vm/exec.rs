use std::ops::{Deref, DerefMut};

use crate::{
    code::InterCode,
    lexer::{Lexer, Token},
    object::{CallFunc, CallInfo, ConstantValue, RefValue, Table, Value},
    state::LuaState,
};

use super::ExecError;

enum VMExecOperator {
    ADD,
    SUB,
    MUL,
    DIV,
    IDIV,
    MOD,
    POW,
    SHL,
    SHR,
    BAND,
    BOR,
    BXOR,
    EQ,
    NE,
    LT,
    LE,
    GT,
    GE,
}

pub(crate) struct VMExec<'s> {
    state: &'s mut LuaState,
}

impl<'s> VMExec<'s> {
    pub(crate) fn new(state: &'s mut LuaState) -> Self {
        Self { state }
    }

    pub(crate) fn exec(&mut self) -> Result<(), ExecError> {
        loop {
            let func = self.state.get_ctx().callinfo.prop().func.clone();

            match func {
                CallFunc::LuaFunc(prop) => {
                    let pc = self.state.get_ctx().callinfo.prop().pc;

                    log::debug!("pre exec pc: {}", pc);

                    if let Some(code) = prop.prop().codes.get(pc) {
                        self.exec_code(code)?;
                    } else {
                        break;
                    }
                }
                CallFunc::RustFunc(func) => {
                    func(self.state)?;
                    self.exec_return0()?;
                }
            }

            if log::Level::Debug <= log::max_level() {
                log::debug!("pc: {}", self.state.get_ctx().callinfo.prop().pc);
                let mut ridx = 0;
                for reg in self.state.get_ctx().reg.iter() {
                    log::debug!("r#{}, ({})", ridx, reg);
                    ridx += 1;
                }
            }
        }
        Ok(())
    }

    pub(crate) fn exec_code(&mut self, code: &InterCode) -> Result<(), ExecError> {
        log::debug!("exec code: {}", code);

        match *code {
            InterCode::LOADNIL(ra, rb) => self.exec_loadnil(ra, rb),
            InterCode::LOADTRUE(ra) => self.exec_loadbool(ra, true),
            InterCode::LOADFALSE(ra) => self.exec_loadbool(ra, false),
            InterCode::LFALSESKIP(ra) => self.exec_lfalseskip(ra),
            InterCode::LOADK(ra, rb) => self.exec_loadk(ra, rb),
            InterCode::LOADINT(ra, rb) => self.exec_loadint(ra, rb),
            InterCode::LOADFLOAT(ra, rb) => self.exec_loadfloat(ra, rb),
            InterCode::VARARG(_ra, _rb) => {} // TODO
            InterCode::JMP(off) => self.exec_jmp(off),
            InterCode::MOVE(ra, rb) => self.exec_move(ra, rb),
            InterCode::CONCAT(ra, rb) => self.exec_concat(ra, rb),
            InterCode::ADDI(ra, rb, imm) => self.exec_addi(ra, rb, imm)?,
            InterCode::ADDK(ra, rb, rc) => self.exec_addk(ra, rb, rc)?,
            InterCode::ADD(ra, rb, rc) => self.exec_add(ra, rb, rc)?,
            InterCode::SUBK(ra, rb, rc) => self.exec_subk(ra, rb, rc)?,
            InterCode::SUB(ra, rb, rc) => self.exec_sub(ra, rb, rc)?,
            InterCode::MULK(ra, rb, rc) => self.exec_mulk(ra, rb, rc)?,
            InterCode::MUL(ra, rb, rc) => self.exec_mul(ra, rb, rc)?,
            InterCode::DIVK(ra, rb, rc) => self.exec_divk(ra, rb, rc)?,
            InterCode::DIV(ra, rb, rc) => self.exec_div(ra, rb, rc)?,
            InterCode::IDIVK(ra, rb, rc) => self.exec_idivk(ra, rb, rc)?,
            InterCode::IDIV(ra, rb, rc) => self.exec_idiv(ra, rb, rc)?,
            InterCode::MODK(ra, rb, rc) => self.exec_modk(ra, rb, rc)?,
            InterCode::MOD(ra, rb, rc) => self.exec_mod(ra, rb, rc)?,
            InterCode::POWK(ra, rb, rc) => self.exec_powk(ra, rb, rc)?,
            InterCode::POW(ra, rb, rc) => self.exec_pow(ra, rb, rc)?,
            InterCode::SHL(ra, rb, rc) => self.exec_shl(ra, rb, rc)?,
            InterCode::SHLI(ra, rb, imm) => self.exec_shli(ra, rb, imm)?,
            InterCode::SHR(ra, rb, rc) => self.exec_shr(ra, rb, rc)?,
            InterCode::SHRI(ra, rb, imm) => self.exec_shri(ra, rb, imm)?,
            InterCode::EQI(ra, rb, k) => self.exec_eqi(ra, rb, k)?,
            InterCode::EQ(ra, rb, k) => self.exec_eq(ra, rb, k)?,
            InterCode::EQK(ra, rb, k) => self.exec_eqk(ra, rb, k)?,
            InterCode::LT(ra, rb, k) => self.exec_lt(ra, rb, k)?,
            InterCode::LTI(ra, rb, k) => self.exec_lti(ra, rb, k)?,
            InterCode::LE(ra, rb, k) => self.exec_le(ra, rb, k)?,
            InterCode::LEI(ra, rb, k) => self.exec_lei(ra, rb, k)?,
            InterCode::GTI(ra, rb, k) => self.exec_gti(ra, rb, k)?,
            InterCode::GEI(ra, rb, k) => self.exec_gei(ra, rb, k)?,
            InterCode::TEST(ra, k) => self.exec_test(ra, k)?,
            InterCode::TESTSET(ra, rb, k) => self.exec_testset(ra, rb, k)?,
            InterCode::NOT(ra, rb) => self.exec_not(ra, rb)?,
            InterCode::UNM(ra, rb) => self.exec_unm(ra, rb)?,
            InterCode::BNOT(ra, rb) => self.exec_bnot(ra, rb)?,
            InterCode::LEN(ra, rb) => self.exec_len(ra, rb)?,
            InterCode::GETUPVAL(ra, rb) => self.exec_getupval(ra, rb)?,
            InterCode::SETUPVAL(ra, rb) => self.exec_setupval(ra, rb)?,
            InterCode::CLOSURE(ra, rb) => self.exec_closure(ra, rb)?,
            InterCode::SETFIELD(ra, rb, rc, iskey) => self.exec_setfield(ra, rb, rc, iskey)?,
            InterCode::SETTABLE(ra, rb, rc, iskey) => self.exec_settable(ra, rb, rc, iskey)?,
            InterCode::SETI(ra, rb, rc, iskey) => self.exec_seti(ra, rb, rc, iskey)?,
            InterCode::SETTABUP(ra, rb, rc, iskey) => self.exec_settableup(ra, rb, rc, iskey)?,
            InterCode::SETLIST(ra, rb, rc, extra) => self.exec_setlist(ra, rb, rc, extra)?,
            InterCode::GETFIELD(ra, rb, rc) => self.exec_getfield(ra, rb, rc)?,
            InterCode::GETTABLE(ra, rb, rc) => self.exec_gettable(ra, rb, rc)?,
            InterCode::GETI(ra, rb, rc) => self.exec_geti(ra, rb, rc)?,
            InterCode::GETTABUP(ra, rb, rc) => self.exec_gettableup(ra, rb, rc)?,
            InterCode::NEWTABLE(ra, rb, rc, _extra) => self.exec_newtable(ra, rb, rc)?,
            InterCode::CALL(ra, rb, rc) => {
                self.state.get_ctx().callinfo.prop_mut().pc += 1;
                return self.exec_call(ra, rb, rc);
            }
            InterCode::RETURN(ra, rb) => return self.exec_return(ra, rb),
            InterCode::RETURN1(ra) => return self.exec_return1(ra),
            InterCode::RETURN0 => return self.exec_return0(),
            InterCode::SELF(ra, rb, rc, iskey) => self.exec_self(ra, rb, rc, iskey)?,
            InterCode::FORPREP(ra, rb) => self.exec_forprep(ra, rb)?,
            InterCode::FORLOOP(ra, rb) => self.exec_forloop(ra, rb)?,
            InterCode::TFORPREP(ra, rb) => self.exec_tforprep(ra, rb)?,
            InterCode::TFORCALL(ra, rb) => {
                self.state.get_ctx().callinfo.prop_mut().pc += 1;
                return self.exec_tforcall(ra, rb);
            }
            InterCode::TFORLOOP(ra, rb) => self.exec_tforloop(ra, rb)?,
            _ => {}
        }
        self.state.get_ctx().callinfo.prop_mut().pc += 1;

        Ok(())
    }

    fn exec_loadnil(&mut self, ra: usize, rb: usize) {
        for off in 0..rb {
            self.state.set(ra + off, RefValue::new());
        }
    }

    fn exec_loadbool(&mut self, ra: usize, value: bool) {
        self.state.set(ra, RefValue::from(value))
    }

    fn exec_lfalseskip(&mut self, ra: usize) {
        self.state.set(ra, RefValue::from(false));
        self.state.get_ctx().callinfo.prop_mut().pc += 1;
    }

    fn exec_loadk(&mut self, ra: usize, rb: usize) {
        if let Some(k) = self.state.constant_pool.as_ref().borrow().get(rb) {
            match k {
                ConstantValue::Float(v) => self.state.set(ra, RefValue::from(*v)),
                ConstantValue::Integer(v) => self.state.set(ra, RefValue::from(*v)),
                ConstantValue::String(v) => self.state.set(ra, RefValue::from(v.as_str())),
                ConstantValue::Bool(v) => self.state.set(ra, RefValue::from(*v)),
                ConstantValue::Nil => self.state.set(ra, RefValue::new()),
            }
        } else {
            self.state.set(ra, RefValue::new())
        }
    }

    fn exec_loadint(&mut self, ra: usize, rb: u32) {
        self.state.set(ra, RefValue::from(rb as i64))
    }

    fn exec_loadfloat(&mut self, ra: usize, rb: u32) {
        self.state.set(ra, RefValue::from(rb as f64))
    }

    fn exec_jmp(&mut self, offset: Option<i32>) {
        if let Some(offset) = offset {
            if offset < 0 {
                self.state.get_ctx().callinfo.prop_mut().pc -= offset.abs() as usize;
            } else {
                self.state.get_ctx().callinfo.prop_mut().pc += offset.abs() as usize;
            }
        }
    }

    fn exec_move(&mut self, ra: usize, rb: usize) {
        let value = self.state.get(rb);
        self.state.set(ra, value);
    }

    fn exec_concat(&mut self, ra: usize, rb: usize) {
        let mut value = String::new();

        for off in 0..rb {
            match self.state.get(ra + off).get().deref() {
                Value::String(iv) => value.push_str(iv.as_str()),
                Value::Boolean(iv) => value.push_str(iv.to_string().as_str()),
                Value::Number(iv) => value.push_str(iv.to_string().as_str()),
                Value::Integer(iv) => value.push_str(iv.to_string().as_str()),
                _ => {}
            }
        }

        self.state.set(ra, RefValue::from(value.as_str()))
    }

    fn string_tonumeric(&self, a: &str) -> Result<RefValue, ExecError> {
        let mut lex = Lexer::new(a);

        lex.token_next().or(Err(ExecError::BadOperand))?;

        let value = match &lex.token {
            Token::String(v) => RefValue::from(v.as_str()),
            Token::Integer(v) => RefValue::from(*v),
            Token::Number(v) => RefValue::from(*v),
            Token::True => RefValue::from(true),
            Token::False => RefValue::from(false),
            _ => return Err(ExecError::BadOperand),
        };

        Ok(value)
    }

    fn refvalue_op(
        &self,
        op: VMExecOperator,
        a: RefValue,
        b: RefValue,
    ) -> Result<RefValue, ExecError> {
        let a = match a.get().deref() {
            Value::String(v) => {
                let a = self.string_tonumeric(v.as_str())?;
                return self.refvalue_op(op, a, b);
            }
            Value::Boolean(..) | Value::Number(..) | Value::Integer(..) | Value::Nil => a.clone(),
            _ => return Err(ExecError::BadOperand),
        };

        let b = match b.get().deref() {
            Value::String(v) => {
                let b = self.string_tonumeric(v.as_str())?;
                return self.refvalue_op(op, a, b);
            }
            Value::Boolean(..) | Value::Number(..) | Value::Integer(..) | Value::Nil => b.clone(),
            _ => return Err(ExecError::BadOperand),
        };

        let result = match a.get().deref() {
            Value::Integer(a) => match b.get().deref() {
                Value::Integer(b) => match op {
                    VMExecOperator::ADD => RefValue::from(*a + *b),
                    VMExecOperator::SUB => RefValue::from(*a - *b),
                    VMExecOperator::MUL => RefValue::from(*a * *b),
                    VMExecOperator::DIV => RefValue::from(*a / *b),
                    VMExecOperator::IDIV => RefValue::from((*a / *b) as i64),
                    VMExecOperator::MOD => RefValue::from(*a % *b),
                    VMExecOperator::POW => RefValue::from(a.pow(*b as u32)),
                    VMExecOperator::SHL => RefValue::from(*a << *b),
                    VMExecOperator::SHR => RefValue::from(*a >> *b),
                    VMExecOperator::BAND => RefValue::from(*a & *b),
                    VMExecOperator::BOR => RefValue::from(*a | *b),
                    VMExecOperator::BXOR => RefValue::from(*a ^ *b),
                    VMExecOperator::EQ => RefValue::from(*a == *b),
                    VMExecOperator::NE => RefValue::from(*a != *b),
                    VMExecOperator::LT => RefValue::from(*a < *b),
                    VMExecOperator::LE => RefValue::from(*a <= *b),
                    VMExecOperator::GT => RefValue::from(*a > *b),
                    VMExecOperator::GE => RefValue::from(*a >= *b),
                },
                Value::Number(b) => match op {
                    VMExecOperator::ADD => RefValue::from(*a as f64 + *b),
                    VMExecOperator::SUB => RefValue::from(*a as f64 - *b),
                    VMExecOperator::MUL => RefValue::from(*a as f64 * *b),
                    VMExecOperator::DIV => RefValue::from(*a as f64 / *b),
                    VMExecOperator::IDIV => RefValue::from((*a as f64 / *b) as i64),
                    VMExecOperator::MOD => RefValue::from(*a as f64 % *b),
                    VMExecOperator::POW => RefValue::from((*a as f64).powf(*b)),
                    VMExecOperator::SHL => RefValue::from((*a as i64) << (*b as i64)),
                    VMExecOperator::SHR => RefValue::from((*a as i64) >> (*b as i64)),
                    VMExecOperator::BAND => RefValue::from((*a as i64) & (*b as i64)),
                    VMExecOperator::BOR => RefValue::from((*a as i64) | (*b as i64)),
                    VMExecOperator::BXOR => RefValue::from((*a as i64) ^ (*b as i64)),
                    VMExecOperator::EQ => RefValue::from(*a as f64 == *b),
                    VMExecOperator::NE => RefValue::from(*a as f64 != *b),
                    VMExecOperator::LT => RefValue::from((*a as f64) < *b),
                    VMExecOperator::LE => RefValue::from(*a as f64 <= *b),
                    VMExecOperator::GT => RefValue::from(*a as f64 > *b),
                    VMExecOperator::GE => RefValue::from(*a as f64 >= *b),
                },
                Value::Boolean(b) => match op {
                    VMExecOperator::ADD => RefValue::from(*a + *b as i64),
                    VMExecOperator::SUB => RefValue::from(*a - *b as i64),
                    VMExecOperator::MUL => RefValue::from(*a * *b as i64),
                    VMExecOperator::DIV => RefValue::from(*a / *b as i64),
                    VMExecOperator::IDIV => RefValue::from((*a / *b as i64) as i64),
                    VMExecOperator::MOD => RefValue::from(*a % *b as i64),
                    VMExecOperator::POW => RefValue::from(a.pow(*b as u32)),
                    VMExecOperator::SHL => RefValue::from(*a << *b as i64),
                    VMExecOperator::SHR => RefValue::from(*a >> *b as i64),
                    VMExecOperator::BAND => RefValue::from(*a & *b as i64),
                    VMExecOperator::BOR => RefValue::from(*a | *b as i64),
                    VMExecOperator::BXOR => RefValue::from(*a ^ *b as i64),
                    VMExecOperator::EQ => RefValue::from(*a == *b as i64),
                    VMExecOperator::NE => RefValue::from(*a != *b as i64),
                    VMExecOperator::LT => RefValue::from(*a < (*b as i64)),
                    VMExecOperator::LE => RefValue::from(*a <= *b as i64),
                    VMExecOperator::GT => RefValue::from(*a > *b as i64),
                    VMExecOperator::GE => RefValue::from(*a >= *b as i64),
                },
                Value::Nil => RefValue::from(match op {
                    VMExecOperator::ADD => RefValue::from(*a),
                    VMExecOperator::SUB => RefValue::from(*a),
                    VMExecOperator::MUL => RefValue::from(0),
                    VMExecOperator::DIV => RefValue::from(f64::NAN),
                    VMExecOperator::IDIV => RefValue::from(f64::NAN),
                    VMExecOperator::MOD => RefValue::from(f64::NAN),
                    VMExecOperator::POW => RefValue::from(0),
                    VMExecOperator::SHL => RefValue::from(*a),
                    VMExecOperator::SHR => RefValue::from(*a),
                    VMExecOperator::BAND => RefValue::from(0),
                    VMExecOperator::BOR => RefValue::from(*a),
                    VMExecOperator::BXOR => RefValue::from(*a ^ 0),
                    VMExecOperator::EQ => RefValue::from(false),
                    VMExecOperator::NE => RefValue::from(true),
                    VMExecOperator::LT => RefValue::from(*a < 0),
                    VMExecOperator::LE => RefValue::from(*a <= 0),
                    VMExecOperator::GT => RefValue::from(*a > 0),
                    VMExecOperator::GE => RefValue::from(*a >= 0),
                }),
                _ => return Err(ExecError::BadOperand),
            },
            Value::Number(a) => match b.get().deref() {
                Value::Integer(b) => match op {
                    VMExecOperator::ADD => RefValue::from(*a + *b as f64),
                    VMExecOperator::SUB => RefValue::from(*a - *b as f64),
                    VMExecOperator::MUL => RefValue::from(*a * *b as f64),
                    VMExecOperator::DIV => RefValue::from(*a / *b as f64),
                    VMExecOperator::IDIV => RefValue::from((*a / *b as f64) as i64),
                    VMExecOperator::MOD => RefValue::from(*a as i64 % *b as i64),
                    VMExecOperator::POW => RefValue::from(a.powi(*b as i32)),
                    VMExecOperator::SHL => RefValue::from((*a as i64) << *b),
                    VMExecOperator::SHR => RefValue::from((*a as i64) >> *b),
                    VMExecOperator::BAND => RefValue::from((*a as i64) & *b),
                    VMExecOperator::BOR => RefValue::from((*a as i64) | *b),
                    VMExecOperator::BXOR => RefValue::from((*a as i64) ^ (*b as i64)),
                    VMExecOperator::EQ => RefValue::from(*a == *b as f64),
                    VMExecOperator::NE => RefValue::from(*a != *b as f64),
                    VMExecOperator::LT => RefValue::from(*a < (*b as f64)),
                    VMExecOperator::LE => RefValue::from(*a <= *b as f64),
                    VMExecOperator::GT => RefValue::from(*a > *b as f64),
                    VMExecOperator::GE => RefValue::from(*a >= *b as f64),
                },
                Value::Number(b) => match op {
                    VMExecOperator::ADD => RefValue::from(*a + *b),
                    VMExecOperator::SUB => RefValue::from(*a - *b),
                    VMExecOperator::MUL => RefValue::from(*a * *b),
                    VMExecOperator::DIV => RefValue::from(*a / *b),
                    VMExecOperator::IDIV => RefValue::from((*a / *b) as i64),
                    VMExecOperator::MOD => RefValue::from(*a as i64 % *b as i64),
                    VMExecOperator::POW => RefValue::from(a.powi(*b as i32)),
                    VMExecOperator::SHL => RefValue::from((*a as i64) << (*b as i64)),
                    VMExecOperator::SHR => RefValue::from((*a as i64) >> (*b as i64)),
                    VMExecOperator::BAND => RefValue::from((*a as i64) & (*b as i64)),
                    VMExecOperator::BOR => RefValue::from((*a as i64) | (*b as i64)),
                    VMExecOperator::BXOR => RefValue::from((*a as i64) ^ (*b as i64)),
                    VMExecOperator::EQ => RefValue::from(*a == *b),
                    VMExecOperator::NE => RefValue::from(*a != *b),
                    VMExecOperator::LT => RefValue::from(*a < *b),
                    VMExecOperator::LE => RefValue::from(*a <= *b),
                    VMExecOperator::GT => RefValue::from(*a > *b),
                    VMExecOperator::GE => RefValue::from(*a >= *b),
                },
                Value::Boolean(b) => match op {
                    VMExecOperator::ADD => RefValue::from(*a + *b as i64 as f64),
                    VMExecOperator::SUB => RefValue::from(*a - *b as i64 as f64),
                    VMExecOperator::MUL => RefValue::from(*a * *b as i64 as f64),
                    VMExecOperator::DIV => RefValue::from(*a / *b as i64 as f64),
                    VMExecOperator::IDIV => RefValue::from((*a / *b as i64 as f64) as i64),
                    VMExecOperator::MOD => RefValue::from(*a as i64 % *b as i64),
                    VMExecOperator::POW => RefValue::from(a.powi(*b as i32)),
                    VMExecOperator::SHL => RefValue::from((*a as i64) << (*b as i64)),
                    VMExecOperator::SHR => RefValue::from((*a as i64) >> (*b as i64)),
                    VMExecOperator::BAND => RefValue::from((*a as i64) & (*b as i64)),
                    VMExecOperator::BOR => RefValue::from((*a as i64) | (*b as i64)),
                    VMExecOperator::BXOR => RefValue::from((*a as i64) ^ (*b as i64)),
                    VMExecOperator::EQ => RefValue::from(*a == *b as i64 as f64),
                    VMExecOperator::NE => RefValue::from(*a != *b as i64 as f64),
                    VMExecOperator::LT => RefValue::from(*a < (*b as i64 as f64)),
                    VMExecOperator::LE => RefValue::from(*a <= *b as i64 as f64),
                    VMExecOperator::GT => RefValue::from(*a > *b as i64 as f64),
                    VMExecOperator::GE => RefValue::from(*a >= *b as i64 as f64),
                },
                Value::Nil => match op {
                    VMExecOperator::ADD => RefValue::from(*a),
                    VMExecOperator::SUB => RefValue::from(*a),
                    VMExecOperator::MUL => RefValue::from(0),
                    VMExecOperator::DIV => RefValue::from(f64::NAN),
                    VMExecOperator::IDIV => RefValue::from(f64::NAN),
                    VMExecOperator::MOD => RefValue::from(f64::NAN),
                    VMExecOperator::POW => RefValue::from(1),
                    VMExecOperator::SHL => RefValue::from(*a),
                    VMExecOperator::SHR => RefValue::from(*a),
                    VMExecOperator::BAND => RefValue::from(0),
                    VMExecOperator::BOR => RefValue::from(*a),
                    VMExecOperator::BXOR => RefValue::from((*a as i64) ^ 0),
                    VMExecOperator::EQ => RefValue::from(false),
                    VMExecOperator::NE => RefValue::from(true),
                    VMExecOperator::LT => RefValue::from(*a < 0f64),
                    VMExecOperator::LE => RefValue::from(*a <= 0f64),
                    VMExecOperator::GT => RefValue::from(*a > 0f64),
                    VMExecOperator::GE => RefValue::from(*a >= 0f64),
                },
                _ => return Err(ExecError::BadOperand),
            },
            Value::Boolean(a) => match b.get().deref() {
                Value::Integer(b) => match op {
                    VMExecOperator::ADD => RefValue::from(*a as i64 + *b),
                    VMExecOperator::SUB => RefValue::from(*a as i64 - *b),
                    VMExecOperator::MUL => RefValue::from(*a as i64 * *b),
                    VMExecOperator::DIV => RefValue::from(*a as i64 / *b),
                    VMExecOperator::IDIV => RefValue::from((*a as i64 / *b) as i64),
                    VMExecOperator::MOD => RefValue::from(*a as i64 % *b),
                    VMExecOperator::POW => RefValue::from((*a as i64).pow(*b as u32)),
                    VMExecOperator::SHL => RefValue::from((*a as i64) << *b),
                    VMExecOperator::SHR => RefValue::from(*a as i64 >> *b),
                    VMExecOperator::BAND => RefValue::from(*a as i64 & *b),
                    VMExecOperator::BOR => RefValue::from(*a as i64 | *b),
                    VMExecOperator::BXOR => RefValue::from(*a as i64 ^ *b),
                    VMExecOperator::EQ => RefValue::from(*a as i64 == *b),
                    VMExecOperator::NE => RefValue::from(*a as i64 != *b),
                    VMExecOperator::LT => RefValue::from((*a as i64) < *b),
                    VMExecOperator::LE => RefValue::from(*a as i64 <= *b),
                    VMExecOperator::GT => RefValue::from(*a as i64 > *b),
                    VMExecOperator::GE => RefValue::from(*a as i64 >= *b),
                },
                Value::Number(b) => match op {
                    VMExecOperator::ADD => RefValue::from(*a as i64 as f64 + *b),
                    VMExecOperator::SUB => RefValue::from(*a as i64 as f64 - *b),
                    VMExecOperator::MUL => RefValue::from(*a as i64 as f64 * *b),
                    VMExecOperator::DIV => RefValue::from(*a as i64 as f64 / *b),
                    VMExecOperator::IDIV => RefValue::from((*a as i64 as f64 / *b) as i64),
                    VMExecOperator::MOD => RefValue::from(*a as i64 % *b as i64),
                    VMExecOperator::POW => RefValue::from(*a as i64),
                    VMExecOperator::SHL => RefValue::from((*a as i64) << (*b as i64)),
                    VMExecOperator::SHR => RefValue::from(*a as i64 >> (*b as i64)),
                    VMExecOperator::BAND => RefValue::from(*a as i64 & *b as i64),
                    VMExecOperator::BOR => RefValue::from(*a as i64 | *b as i64),
                    VMExecOperator::BXOR => RefValue::from(*a as i64 ^ *b as i64),
                    VMExecOperator::EQ => RefValue::from(*a as i64 == *b as i64),
                    VMExecOperator::NE => RefValue::from(*a as i64 != *b as i64),
                    VMExecOperator::LT => RefValue::from((*a as i64) < *b as i64),
                    VMExecOperator::LE => RefValue::from(*a as i64 <= *b as i64),
                    VMExecOperator::GT => RefValue::from(*a as i64 > *b as i64),
                    VMExecOperator::GE => RefValue::from(*a as i64 >= *b as i64),
                },
                Value::Boolean(b) => match op {
                    VMExecOperator::ADD => RefValue::from(*a as i64 + *b as i64),
                    VMExecOperator::SUB => RefValue::from(*a as i64 - *b as i64),
                    VMExecOperator::MUL => RefValue::from(*a as i64 * *b as i64),
                    VMExecOperator::DIV => RefValue::from(*a as i64 / *b as i64),
                    VMExecOperator::IDIV => RefValue::from((*a as i64 / *b as i64) as i64),
                    VMExecOperator::MOD => RefValue::from(*a as i64 % *b as i64),
                    VMExecOperator::POW => RefValue::from(*a as i64),
                    VMExecOperator::SHL => RefValue::from((*a as i64) << (*b as i64)),
                    VMExecOperator::SHR => RefValue::from(*a as i64 >> (*b as i64)),
                    VMExecOperator::BAND => RefValue::from(*a && *b),
                    VMExecOperator::BOR => RefValue::from(*a || *b),
                    VMExecOperator::BXOR => RefValue::from(*a ^ *b),
                    VMExecOperator::EQ => RefValue::from(*a == *b),
                    VMExecOperator::NE => RefValue::from(*a != *b),
                    VMExecOperator::LT => RefValue::from((*a as i64) < *b as i64),
                    VMExecOperator::LE => RefValue::from(*a as i64 <= *b as i64),
                    VMExecOperator::GT => RefValue::from(*a as i64 > *b as i64),
                    VMExecOperator::GE => RefValue::from(*a as i64 >= *b as i64),
                },
                Value::Nil => match op {
                    VMExecOperator::ADD => RefValue::from(*a as i64),
                    VMExecOperator::SUB => RefValue::from(*a as i64),
                    VMExecOperator::MUL => RefValue::from(0),
                    VMExecOperator::DIV => RefValue::from(0),
                    VMExecOperator::IDIV => RefValue::from(0),
                    VMExecOperator::MOD => RefValue::from(f64::NAN),
                    VMExecOperator::POW => RefValue::from(*a),
                    VMExecOperator::SHL => RefValue::from(*a as i64),
                    VMExecOperator::SHR => RefValue::from(*a as i64),
                    VMExecOperator::BAND => RefValue::from(false),
                    VMExecOperator::BOR => RefValue::from(*a),
                    VMExecOperator::BXOR => RefValue::from(*a ^ false),
                    VMExecOperator::EQ => RefValue::from(*a == false),
                    VMExecOperator::NE => RefValue::from(*a != false),
                    VMExecOperator::LT => RefValue::from((*a as i64) < 0),
                    VMExecOperator::LE => RefValue::from(*a as i64 <= 0),
                    VMExecOperator::GT => RefValue::from(*a as i64 > 0),
                    VMExecOperator::GE => RefValue::from(*a as i64 >= 0),
                },
                _ => return Err(ExecError::BadOperand),
            },
            Value::Nil => match b.get().deref() {
                Value::Integer(b) => match op {
                    VMExecOperator::ADD => RefValue::from(*b),
                    VMExecOperator::SUB => RefValue::from(-*b),
                    VMExecOperator::MUL => RefValue::from(0),
                    VMExecOperator::DIV => RefValue::from(0),
                    VMExecOperator::IDIV => RefValue::from(0),
                    VMExecOperator::MOD => RefValue::from(0),
                    VMExecOperator::POW => RefValue::from(0),
                    VMExecOperator::SHL => RefValue::from(0),
                    VMExecOperator::SHR => RefValue::from(0),
                    VMExecOperator::BAND => RefValue::from(0),
                    VMExecOperator::BOR => RefValue::from(*b),
                    VMExecOperator::BXOR => RefValue::from(0 ^ *b),
                    VMExecOperator::EQ => RefValue::from(0 == *b),
                    VMExecOperator::NE => RefValue::from(0 != *b),
                    VMExecOperator::LT => RefValue::from(0 < *b),
                    VMExecOperator::LE => RefValue::from(0 <= *b),
                    VMExecOperator::GT => RefValue::from(0 > *b),
                    VMExecOperator::GE => RefValue::from(0 >= *b),
                },
                Value::Number(b) => match op {
                    VMExecOperator::ADD => RefValue::from(*b),
                    VMExecOperator::SUB => RefValue::from(-*b),
                    VMExecOperator::MUL => RefValue::from(0),
                    VMExecOperator::DIV => RefValue::from(0),
                    VMExecOperator::IDIV => RefValue::from(0),
                    VMExecOperator::MOD => RefValue::from(0),
                    VMExecOperator::POW => RefValue::from(0),
                    VMExecOperator::SHL => RefValue::from(0),
                    VMExecOperator::SHR => RefValue::from(0),
                    VMExecOperator::BAND => RefValue::from(0),
                    VMExecOperator::BOR => RefValue::from(*b),
                    VMExecOperator::BXOR => RefValue::from(0 ^ *b as i64),
                    VMExecOperator::EQ => RefValue::from(0f64 == *b),
                    VMExecOperator::NE => RefValue::from(0f64 != *b),
                    VMExecOperator::LT => RefValue::from(0f64 < *b),
                    VMExecOperator::LE => RefValue::from(0f64 <= *b),
                    VMExecOperator::GT => RefValue::from(0f64 > *b),
                    VMExecOperator::GE => RefValue::from(0f64 >= *b),
                },
                Value::Boolean(b) => match op {
                    VMExecOperator::ADD => RefValue::from(*b as i64),
                    VMExecOperator::SUB => RefValue::from(-(*b as i64)),
                    VMExecOperator::MUL => RefValue::from(0),
                    VMExecOperator::DIV => RefValue::from(0),
                    VMExecOperator::IDIV => RefValue::from(0),
                    VMExecOperator::MOD => RefValue::from(0),
                    VMExecOperator::POW => RefValue::from(0),
                    VMExecOperator::SHL => RefValue::from(0),
                    VMExecOperator::SHR => RefValue::from(0),
                    VMExecOperator::BAND => RefValue::from(0),
                    VMExecOperator::BOR => RefValue::from(*b),
                    VMExecOperator::BXOR => RefValue::from(0 ^ *b as i64),
                    VMExecOperator::EQ => RefValue::from(false == *b),
                    VMExecOperator::NE => RefValue::from(false != *b),
                    VMExecOperator::LT => RefValue::from(0 < *b as i64),
                    VMExecOperator::LE => RefValue::from(0 <= *b as i64),
                    VMExecOperator::GT => RefValue::from(0 > *b as i64),
                    VMExecOperator::GE => RefValue::from(0 >= *b as i64),
                },
                Value::Nil => RefValue::from(0),
                _ => return Err(ExecError::BadOperand),
            },
            _ => return Err(ExecError::BadOperand),
        };

        Ok(result)
    }

    fn ktorefvalue(&self, k: usize) -> RefValue {
        match self.state.constant_pool.as_ref().borrow().get(k) {
            Some(ConstantValue::Float(v)) => RefValue::from(*v),
            Some(ConstantValue::Integer(v)) => RefValue::from(*v),
            Some(ConstantValue::Bool(v)) => RefValue::from(*v),
            Some(ConstantValue::String(v)) => RefValue::from(v.as_str()),
            _ => RefValue::new(),
        }
    }

    fn exec_addi(&mut self, ra: usize, rb: usize, imm: u8) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = RefValue::from(imm as i64);

        let result = self.refvalue_op(VMExecOperator::ADD, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_addk(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::ADD, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_add(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.state.get(rc);

        let result = self.refvalue_op(VMExecOperator::ADD, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_subk(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::SUB, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_sub(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.state.get(rc);

        let result = self.refvalue_op(VMExecOperator::SUB, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_mulk(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::MUL, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_mul(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.state.get(rc);

        let result = self.refvalue_op(VMExecOperator::MUL, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_divk(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::DIV, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_div(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.state.get(rc);

        let result = self.refvalue_op(VMExecOperator::DIV, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_idivk(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::IDIV, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_idiv(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.state.get(rc);

        let result = self.refvalue_op(VMExecOperator::IDIV, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_modk(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::MOD, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_mod(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.state.get(rc);

        let result = self.refvalue_op(VMExecOperator::MOD, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_powk(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::POW, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_pow(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.state.get(rc);

        let result = self.refvalue_op(VMExecOperator::POW, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_shli(&mut self, ra: usize, rb: usize, imm: u8) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = RefValue::from(imm as i64);

        let result = self.refvalue_op(VMExecOperator::SHL, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_shl(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.state.get(rc);

        let result = self.refvalue_op(VMExecOperator::SHL, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_shri(&mut self, ra: usize, rb: usize, imm: u8) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = RefValue::from(imm as i64);

        let result = self.refvalue_op(VMExecOperator::SHR, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_shr(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.state.get(rc);

        let result = self.refvalue_op(VMExecOperator::SHR, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_bandk(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::BAND, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_band(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.state.get(rc);

        let result = self.refvalue_op(VMExecOperator::BAND, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_bork(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::BOR, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_bor(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.state.get(rc);

        let result = self.refvalue_op(VMExecOperator::BOR, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_bxork(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::BXOR, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_bxor(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.state.get(rb);
        let b = self.state.get(rc);

        let result = self.refvalue_op(VMExecOperator::BXOR, a, b)?;
        self.state.set(ra, result);
        Ok(())
    }

    fn exec_eqi(&mut self, ra: usize, rb: u32, k: bool) -> Result<(), ExecError> {
        let a = self.state.get(ra);
        let b = RefValue::from(rb as i64);
        let op = if k {
            VMExecOperator::EQ
        } else {
            VMExecOperator::NE
        };

        let result = self.refvalue_op(op, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(false)) {
            self.state.get_ctx().callinfo.prop_mut().pc += 1;
        }
        Ok(())
    }

    fn exec_eqk(&mut self, ra: usize, rb: usize, k: bool) -> Result<(), ExecError> {
        let a = self.state.get(ra);
        let b = self.ktorefvalue(rb);
        let op = if k {
            VMExecOperator::EQ
        } else {
            VMExecOperator::NE
        };

        let result = self.refvalue_op(op, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(false)) {
            self.state.get_ctx().callinfo.prop_mut().pc += 1;
        }
        Ok(())
    }

    fn exec_eq(&mut self, ra: usize, rb: usize, k: bool) -> Result<(), ExecError> {
        let a = self.state.get(ra);
        let b = self.state.get(rb);
        let op = if k {
            VMExecOperator::EQ
        } else {
            VMExecOperator::NE
        };

        let result = self.refvalue_op(op, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(false)) {
            self.state.get_ctx().callinfo.prop_mut().pc += 1;
        }
        Ok(())
    }

    fn exec_lt(&mut self, ra: usize, rb: usize, k: bool) -> Result<(), ExecError> {
        let a = self.state.get(ra);
        let b = self.state.get(rb);
        let op = if k {
            VMExecOperator::LT
        } else {
            VMExecOperator::GT
        };

        let result = self.refvalue_op(op, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(false)) {
            self.state.get_ctx().callinfo.prop_mut().pc += 1;
        }
        Ok(())
    }

    fn exec_le(&mut self, ra: usize, rb: usize, k: bool) -> Result<(), ExecError> {
        let a = self.state.get(ra);
        let b = self.state.get(rb);
        let op = if k {
            VMExecOperator::LE
        } else {
            VMExecOperator::GE
        };

        let result = self.refvalue_op(op, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(false)) {
            self.state.get_ctx().callinfo.prop_mut().pc += 1;
        }
        Ok(())
    }

    fn exec_lti(&mut self, ra: usize, rb: u32, k: bool) -> Result<(), ExecError> {
        let a = self.state.get(ra);
        let b = RefValue::from(rb as i64);

        let result = self.refvalue_op(VMExecOperator::LT, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(result) if *result != k) {
            self.state.get_ctx().callinfo.prop_mut().pc += 1;
        }
        Ok(())
    }

    fn exec_lei(&mut self, ra: usize, rb: u32, k: bool) -> Result<(), ExecError> {
        let a = self.state.get(ra);
        let b = RefValue::from(rb as i64);

        let result = self.refvalue_op(VMExecOperator::LE, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(result) if *result != k) {
            self.state.get_ctx().callinfo.prop_mut().pc += 1;
        }
        Ok(())
    }

    fn exec_gti(&mut self, ra: usize, rb: u32, k: bool) -> Result<(), ExecError> {
        let a = self.state.get(ra);
        let b = RefValue::from(rb as i64);

        let result = self.refvalue_op(VMExecOperator::GT, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(result) if *result != k) {
            self.state.get_ctx().callinfo.prop_mut().pc += 1;
        }
        Ok(())
    }

    fn exec_gei(&mut self, ra: usize, rb: u32, k: bool) -> Result<(), ExecError> {
        let a = self.state.get(ra);
        let b = RefValue::from(rb as i64);

        let result = self.refvalue_op(VMExecOperator::GE, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(result) if *result != k) {
            self.state.get_ctx().callinfo.prop_mut().pc += 1;
        }
        Ok(())
    }

    fn exec_test(&mut self, ra: usize, k: bool) -> Result<(), ExecError> {
        let a = self.state.get(ra);

        let cond = match a.get().deref() {
            Value::Nil if k => true,
            Value::Integer(v) if (*v == 0) == k => true,
            Value::Number(v) if (*v == 0.0) == k => true,
            Value::Boolean(v) if !*v == k => true,
            Value::String(v) if (v.len() == 0) == k => true,
            Value::Table(..) if !k => true,
            _ => false,
        };

        if cond {
            self.state.get_ctx().callinfo.prop_mut().pc += 1;
        }

        Ok(())
    }

    fn exec_testset(&mut self, ra: usize, rb: usize, k: bool) -> Result<(), ExecError> {
        let a = self.state.get(ra);

        let cond = match a.get().deref() {
            Value::Nil if k => true,
            Value::Integer(v) if (*v == 0) == k => true,
            Value::Number(v) if (*v == 0.0) == k => true,
            Value::Boolean(v) if !*v == k => true,
            Value::String(v) if (v.len() == 0) == k => true,
            Value::Table(..) if !k => true,
            _ => false,
        };

        if cond {
            self.state.get_ctx().callinfo.prop_mut().pc += 1;
        } else {
            let value = self.state.get(rb);
            self.state.set(ra, value)
        }

        Ok(())
    }

    fn exec_not(&mut self, ra: usize, rb: usize) -> Result<(), ExecError> {
        let b = self.state.get(rb);

        let value = match b.get().deref() {
            Value::Nil => RefValue::from(true),
            Value::Integer(v) => RefValue::from(*v == 0),
            Value::Number(v) => RefValue::from(*v == 0f64),
            Value::String(v) if v.eq_ignore_ascii_case("true") => RefValue::from(false),
            Value::String(v) if v.eq_ignore_ascii_case("false") => RefValue::from(true),
            Value::String(v) => RefValue::from(v.len() == 0),
            Value::Boolean(v) => RefValue::from(!*v),
            Value::Table(..) => RefValue::from(false),
            Value::Closure(..) => RefValue::from(false),
        };

        self.state.set(ra, value);

        Ok(())
    }

    fn refvalue_unm(&self, value: RefValue) -> Result<RefValue, ExecError> {
        let value = match value.get().deref() {
            Value::String(v) => {
                let b = self.string_tonumeric(v.as_str())?;

                return self.refvalue_unm(b);
            }
            Value::Nil => RefValue::from(true),
            Value::Integer(v) => RefValue::from(-*v),
            Value::Number(v) => RefValue::from(-*v),
            Value::Boolean(v) => RefValue::from(!*v),
            Value::Table(..) => RefValue::from(0),
            Value::Closure(..) => RefValue::from(0),
        };

        Ok(value)
    }

    fn exec_unm(&mut self, ra: usize, rb: usize) -> Result<(), ExecError> {
        let b = self.state.get(rb);

        let value = self.refvalue_unm(b)?;
        self.state.set(ra, value);

        Ok(())
    }

    fn exec_len(&mut self, ra: usize, rb: usize) -> Result<(), ExecError> {
        let b = self.state.get(rb);

        let value = match b.get().deref() {
            Value::Nil => RefValue::from(0),
            Value::Integer(..) => RefValue::from(0),
            Value::Number(..) => RefValue::from(0),
            Value::String(v) => RefValue::from(v.len() as i64),
            Value::Boolean(..) => RefValue::from(0),
            Value::Table(..) => RefValue::from(0),
            Value::Closure(..) => RefValue::from(0),
        };

        self.state.set(ra, value);

        Ok(())
    }

    fn refvalue_bnot(&self, value: RefValue) -> Result<RefValue, ExecError> {
        let value = match value.get().deref() {
            Value::Nil => RefValue::from(!0),
            Value::Integer(v) => RefValue::from(!*v),
            Value::Number(v) => RefValue::from(!(*v as i64)),
            Value::String(v) => {
                let value = self.string_tonumeric(v.as_str())?;

                return self.refvalue_bnot(value);
            }
            Value::Boolean(v) => RefValue::from(!*v),
            Value::Table(..) => RefValue::from(false),
            Value::Closure(..) => RefValue::from(false),
        };

        Ok(value)
    }

    fn exec_bnot(&mut self, ra: usize, rb: usize) -> Result<(), ExecError> {
        let b = self.state.get(rb);

        let value = self.refvalue_bnot(b)?;
        self.state.set(ra, value);
        Ok(())
    }

    fn exec_getupval(&mut self, ra: usize, rb: usize) -> Result<(), ExecError> {
        if let Some(b) = self.state.proto.prop().upvars.get(rb) {
            let value = self.state.get_ctx().get_abs(b.idx);
            self.state.set(ra, value);
        } else {
            self.state.set(ra, RefValue::new());
        }

        Ok(())
    }

    fn exec_setupval(&mut self, ra: usize, rb: usize) -> Result<(), ExecError> {
        if let Some(b) = self.state.proto.prop().upvars.get(rb) {
            let value = self.state.get(ra);
            self.state.get_ctx().set_abs(b.idx, value);

            Ok(())
        } else {
            Err(ExecError::BadOperand)
        }
    }

    fn exec_closure(&mut self, ra: usize, rb: usize) -> Result<(), ExecError> {
        let closure = match &self.state.get_ctx().callinfo.prop().func {
            CallFunc::LuaFunc(proto) => {
                if let Some(p) = proto.prop().children_proto.get(rb) {
                    RefValue::from(p.clone())
                } else {
                    return Err(ExecError::BadOperand);
                }
            }
            _ => return Err(ExecError::BadOperand),
        };

        self.state.set(ra, closure);

        Ok(())
    }

    fn exec_setfield(
        &mut self,
        ra: usize,
        rb: usize,
        rc: usize,
        iskey: bool,
    ) -> Result<(), ExecError> {
        let key = self.ktorefvalue(rb);
        let value = if iskey {
            self.ktorefvalue(rc)
        } else {
            self.state.get(rc)
        };

        match self.state.get(ra).get_mut().deref_mut() {
            Value::Table(table) => table.set(key, value),
            _ => return Err(ExecError::BadOperand),
        };

        Ok(())
    }

    fn exec_settable(
        &mut self,
        ra: usize,
        rb: usize,
        rc: usize,
        iskey: bool,
    ) -> Result<(), ExecError> {
        let key = self.state.get(rb);
        let value = if iskey {
            self.ktorefvalue(rc)
        } else {
            self.state.get(rc)
        };

        match self.state.get(ra).get_mut().deref_mut() {
            Value::Table(table) => table.set(key, value),
            _ => return Err(ExecError::BadOperand),
        };

        Ok(())
    }

    fn exec_settableup(
        &mut self,
        ra: usize,
        rb: usize,
        rc: usize,
        iskey: bool,
    ) -> Result<(), ExecError> {
        let key = self.ktorefvalue(rb);
        let value = if iskey {
            self.ktorefvalue(rc)
        } else {
            self.state.get(rc)
        };

        if let Some(b) = self.state.proto.prop().upvars.get(ra) {
            match self.state.get_ctx().get_abs(b.idx).get_mut().deref_mut() {
                Value::Table(table) => table.set(key, value),
                _ => return Err(ExecError::BadOperand),
            }

            Ok(())
        } else {
            Err(ExecError::BadOperand)
        }
    }

    fn exec_gettableup(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let key = self.ktorefvalue(rc);

        if let Some(b) = self.state.proto.prop().upvars.get(rb) {
            let value = match self.state.get_ctx().get_abs(b.idx).get_mut().deref_mut() {
                Value::Table(table) => table.get(key.get()),
                _ => return Err(ExecError::BadOperand),
            };

            if let Some(value) = value {
                self.state.set(ra, value)
            } else {
                self.state.set(ra, RefValue::new())
            }

            Ok(())
        } else {
            Err(ExecError::BadOperand)
        }
    }

    fn exec_seti(&mut self, ra: usize, rb: i64, rc: usize, iskey: bool) -> Result<(), ExecError> {
        let key = RefValue::from(rb);
        let value = if iskey {
            self.ktorefvalue(rc)
        } else {
            self.state.get(rc)
        };

        match self.state.get(ra).get_mut().deref_mut() {
            Value::Table(table) => table.set(key, value),
            _ => return Err(ExecError::BadOperand),
        };
        Ok(())
    }

    fn exec_getfield(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let key = self.ktorefvalue(rc);
        let value = match self.state.get(rb).get().deref() {
            Value::Table(table) => table.get(key.get()),
            _ => return Err(ExecError::BadOperand),
        };

        if let Some(value) = value {
            self.state.set(ra, value)
        } else {
            self.state.set(ra, RefValue::new())
        }

        Ok(())
    }

    fn exec_gettable(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let key = self.state.get(rc);
        let value = match self.state.get(rb).get().deref() {
            Value::Table(table) => table.get(key.get()),
            _ => return Err(ExecError::BadOperand),
        };

        if let Some(value) = value {
            self.state.set(ra, value)
        } else {
            self.state.set(ra, RefValue::new())
        }

        Ok(())
    }

    fn exec_geti(&mut self, ra: usize, rb: usize, rc: i64) -> Result<(), ExecError> {
        let key = RefValue::from(rc);
        let value = match self.state.get(rb).get().deref() {
            Value::Table(table) => table.get(key.get()),
            _ => return Err(ExecError::BadOperand),
        };

        if let Some(value) = value {
            self.state.set(ra, value)
        } else {
            self.state.set(ra, RefValue::new())
        }

        Ok(())
    }

    fn exec_call(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let closure = match self.state.get(ra).get().deref() {
            Value::Closure(closure) => closure.clone(),
            _ => return Err(ExecError::BadOperand),
        };

        let base = self.state.get_ctx().callinfo.prop().regbase + ra + 1;

        let ci = CallInfo::new(base, closure);
        ci.prop_mut().nparams = rb - 1;
        ci.prop_mut().prev = Some(self.state.get_ctx().callinfo.clone());
        ci.prop_mut().nresults = rc - 1;

        self.state.get_ctx().callinfo = ci;

        Ok(())
    }

    fn exec_return(&mut self, ra: usize, rb: usize) -> Result<(), ExecError> {
        for off in 0..rb {
            let value = self.state.get(ra + off);
            let regidx = self.state.get_ctx().callinfo.prop().regbase - 1 + off;
            self.state.get_ctx().set_abs(regidx, value);
        }

        self.exec_return0()
    }

    fn exec_return1(&mut self, ra: usize) -> Result<(), ExecError> {
        let value = self.state.get(ra);
        let regidx = self.state.get_ctx().callinfo.prop().regbase - 1;
        self.state.get_ctx().set_abs(regidx, value);

        self.exec_return0()
    }

    fn exec_return0(&mut self) -> Result<(), ExecError> {
        let prev = self.state.get_ctx().callinfo.prop().prev.clone();
        if let Some(prev) = prev {
            self.state.get_ctx().callinfo = prev;
        }
        Ok(())
    }

    fn exec_newtable(&mut self, ra: usize, _rb: usize, _rc: usize) -> Result<(), ExecError> {
        let value = Table::new();
        self.state.set(ra, value);

        Ok(())
    }

    fn exec_setlist(
        &mut self,
        ra: usize,
        rb: usize,
        rc: usize,
        _extra: bool,
    ) -> Result<(), ExecError> {
        match self.state.get(ra).get_mut().deref_mut() {
            Value::Table(table) => {
                for off in 1..(rb + 1) {
                    let key = RefValue::from((rc + off) as i64);
                    let value = self.state.get(ra + off);

                    table.set(key, value);
                }
            }
            _ => return Err(ExecError::BadOperand),
        };

        Ok(())
    }

    fn exec_self(&mut self, ra: usize, rb: usize, rc: usize, iskey: bool) -> Result<(), ExecError> {
        let value = self.state.get(rb);
        self.state.set(ra + 1, value);

        let key = if iskey {
            self.ktorefvalue(rc)
        } else {
            self.state.get(rc)
        };
        let value = match self.state.get(rb).get_mut().deref_mut() {
            Value::Table(table) => table.get(key.get()),
            _ => return Err(ExecError::BadOperand),
        };
        if let Some(value) = value {
            self.state.set(ra, value);
        } else {
            self.state.set(ra, RefValue::new());
        }

        Ok(())
    }

    fn exec_forprep(&mut self, ra: usize, rb: i32) -> Result<(), ExecError> {
        let init = self.state.get(ra);
        let limit = self.state.get(ra + 1);
        let step = self.state.get(ra + 2);

        let isnegative = match step.get().deref() {
            Value::Integer(v) if *v != 0 => *v < 0,
            Value::Number(v) if *v != 0f64 => *v < 0f64,
            _ => return Err(ExecError::BadOperand),
        };

        let exit = match init.get().deref() {
            Value::Integer(i) => match limit.get().deref() {
                Value::Integer(l) => {
                    if isnegative {
                        *i <= *l
                    } else {
                        *i >= *l
                    }
                }
                Value::Number(l) => {
                    if isnegative {
                        *i as f64 <= *l
                    } else {
                        *i as f64 >= *l
                    }
                }
                _ => return Err(ExecError::BadOperand),
            },
            Value::Number(i) => match limit.get().deref() {
                Value::Integer(l) => {
                    if isnegative {
                        *i <= *l as f64
                    } else {
                        *i >= *l as f64
                    }
                }
                Value::Number(l) => {
                    if isnegative {
                        *i <= *l
                    } else {
                        *i >= *l
                    }
                }
                _ => return Err(ExecError::BadOperand),
            },
            _ => return Err(ExecError::BadOperand),
        };

        if exit {
            self.state.get_ctx().callinfo.prop_mut().pc += rb as usize + 1;
        } else {
            self.state.set(ra + 3, init);
        }
        Ok(())
    }

    fn exec_forloop(&mut self, ra: usize, rb: i32) -> Result<(), ExecError> {
        let key = self.state.get(ra + 3);
        let limit = self.state.get(ra + 1);
        let step = self.state.get(ra + 2);

        let isnegative = match step.get().deref() {
            Value::Integer(v) if *v != 0 => *v < 0,
            Value::Number(v) if *v != 0f64 => *v < 0f64,
            _ => return Err(ExecError::BadOperand),
        };

        let next = match key.get().deref() {
            Value::Integer(i) => match step.get().deref() {
                Value::Integer(l) => RefValue::from(*i + *l),
                Value::Number(l) => RefValue::from(*i as f64 + *l),
                _ => return Err(ExecError::BadOperand),
            },
            Value::Number(i) => match limit.get().deref() {
                Value::Integer(l) => RefValue::from(*i + *l as f64),
                Value::Number(l) => RefValue::from(*i + *l),
                _ => return Err(ExecError::BadOperand),
            },
            _ => return Err(ExecError::BadOperand),
        };

        let exit = match next.get().deref() {
            Value::Integer(i) => match limit.get().deref() {
                Value::Integer(l) => {
                    if isnegative {
                        *i <= *l
                    } else {
                        *i >= *l
                    }
                }
                Value::Number(l) => {
                    if isnegative {
                        *i as f64 <= *l
                    } else {
                        *i as f64 >= *l
                    }
                }
                _ => return Err(ExecError::BadOperand),
            },
            Value::Number(i) => match limit.get().deref() {
                Value::Integer(l) => {
                    if isnegative {
                        *i <= *l as f64
                    } else {
                        *i >= *l as f64
                    }
                }
                Value::Number(l) => {
                    if isnegative {
                        *i <= *l
                    } else {
                        *i >= *l
                    }
                }
                _ => return Err(ExecError::BadOperand),
            },
            _ => return Err(ExecError::BadOperand),
        };

        if !exit {
            self.state.set(ra + 3, next);
            self.state.get_ctx().callinfo.prop_mut().pc -= rb as usize;
        }
        Ok(())
    }

    fn exec_tforprep(&mut self, _ra: usize, rb: i32) -> Result<(), ExecError> {
        self.state.get_ctx().callinfo.prop_mut().pc += rb as usize;
        Ok(())
    }

    fn exec_tforcall(&mut self, ra: usize, rb: usize) -> Result<(), ExecError> {
        self.state.set(ra + 3, self.state.get(ra)); // func
        self.state.set(ra + 4, self.state.get(ra + 1)); // table
        self.state.set(ra + 5, self.state.get(ra + 2)); // state
        self.state.set(ra + 6, self.state.get(ra + 3)); // ctrl

        self.exec_call(ra + 3, 4, rb)
    }

    fn exec_tforloop(&mut self, ra: usize, rb: i32) -> Result<(), ExecError> {
        if !matches!(self.state.get(ra + 4).get().deref(), Value::Nil) {
            self.state.set(ra + 2, self.state.get(ra + 4));
            self.state.get_ctx().callinfo.prop_mut().pc -= rb as usize;
        }

        Ok(())
    }
}
