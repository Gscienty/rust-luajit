use std::ops::Deref;

use crate::{
    code::InterCode,
    lexer::{Lexer, Token},
    object::{ConstantValue, RefValue, VMContext, Value},
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
    state: &'s LuaState,
    ctx: &'s mut VMContext,
}

impl<'s> VMExec<'s> {
    pub(crate) fn new(state: &'s LuaState, ctx: &'s mut VMContext) -> Self {
        Self { state, ctx }
    }

    pub(crate) fn exec(&mut self, code: &InterCode) -> Result<(), ExecError> {
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
            _ => {}
        }
        self.ctx.pc += 1;

        Ok(())
    }

    fn exec_loadnil(&mut self, ra: usize, rb: usize) {
        for off in 0..rb {
            self.ctx.set(ra + off, RefValue::new());
        }
    }

    fn exec_loadbool(&mut self, ra: usize, value: bool) {
        self.ctx.set(ra, RefValue::from(value))
    }

    fn exec_lfalseskip(&mut self, ra: usize) {
        self.ctx.set(ra, RefValue::from(false));
        self.ctx.pc += 1;
    }

    fn exec_loadk(&mut self, ra: usize, rb: usize) {
        if let Some(k) = self.state.constant_pool.borrow().get(rb) {
            match k {
                ConstantValue::Float(v) => self.ctx.set(ra, RefValue::from(*v)),
                ConstantValue::Integer(v) => self.ctx.set(ra, RefValue::from(*v)),
                ConstantValue::String(v) => self.ctx.set(ra, RefValue::from(v.as_str())),
                ConstantValue::Bool(v) => self.ctx.set(ra, RefValue::from(*v)),
                ConstantValue::Nil => self.ctx.set(ra, RefValue::new()),
            }
        } else {
            self.ctx.set(ra, RefValue::new())
        }
    }

    fn exec_loadint(&mut self, ra: usize, rb: u32) {
        self.ctx.set(ra, RefValue::from(rb as i64))
    }

    fn exec_loadfloat(&mut self, ra: usize, rb: u32) {
        self.ctx.set(ra, RefValue::from(rb as f64))
    }

    fn exec_jmp(&mut self, offset: Option<i32>) {
        if let Some(offset) = offset {
            if offset < 0 {
                self.ctx.pc -= offset.abs() as usize;
            } else {
                self.ctx.pc += offset.abs() as usize;
            }
        }
    }

    fn exec_move(&mut self, ra: usize, rb: usize) {
        let value = self.ctx.get(rb);
        self.ctx.set(ra, value);
    }

    fn exec_concat(&mut self, ra: usize, rb: usize) {
        let mut value = String::new();

        for off in 0..rb {
            match self.ctx.get(ra + off).get().deref() {
                Value::String(iv) => value.push_str(iv.as_str()),
                Value::Boolean(iv) => value.push_str(iv.to_string().as_str()),
                Value::Number(iv) => value.push_str(iv.to_string().as_str()),
                Value::Integer(iv) => value.push_str(iv.to_string().as_str()),
                _ => {}
            }
        }

        self.ctx.set(ra, RefValue::from(value.as_str()))
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
        match self.state.constant_pool.borrow().get(k) {
            Some(ConstantValue::Float(v)) => RefValue::from(*v),
            Some(ConstantValue::Integer(v)) => RefValue::from(*v),
            Some(ConstantValue::Bool(v)) => RefValue::from(*v),
            Some(ConstantValue::String(v)) => RefValue::from(v.as_str()),
            _ => RefValue::new(),
        }
    }

    fn exec_addi(&mut self, ra: usize, rb: usize, imm: u8) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = RefValue::from(imm as i64);

        let result = self.refvalue_op(VMExecOperator::ADD, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_addk(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::ADD, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_add(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ctx.get(rc);

        let result = self.refvalue_op(VMExecOperator::ADD, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_subk(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::SUB, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_sub(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ctx.get(rc);

        let result = self.refvalue_op(VMExecOperator::SUB, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_mulk(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::MUL, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_mul(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ctx.get(rc);

        let result = self.refvalue_op(VMExecOperator::MUL, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_divk(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::DIV, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_div(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ctx.get(rc);

        let result = self.refvalue_op(VMExecOperator::DIV, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_idivk(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::IDIV, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_idiv(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ctx.get(rc);

        let result = self.refvalue_op(VMExecOperator::IDIV, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_modk(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::MOD, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_mod(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ctx.get(rc);

        let result = self.refvalue_op(VMExecOperator::MOD, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_powk(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::POW, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_pow(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ctx.get(rc);

        let result = self.refvalue_op(VMExecOperator::POW, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_shli(&mut self, ra: usize, rb: usize, imm: u8) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = RefValue::from(imm as i64);

        let result = self.refvalue_op(VMExecOperator::SHL, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_shl(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ctx.get(rc);

        let result = self.refvalue_op(VMExecOperator::SHL, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_shri(&mut self, ra: usize, rb: usize, imm: u8) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = RefValue::from(imm as i64);

        let result = self.refvalue_op(VMExecOperator::SHR, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_shr(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ctx.get(rc);

        let result = self.refvalue_op(VMExecOperator::SHR, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_bandk(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::BAND, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_band(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ctx.get(rc);

        let result = self.refvalue_op(VMExecOperator::BAND, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_bork(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::BOR, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_bor(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ctx.get(rc);

        let result = self.refvalue_op(VMExecOperator::BOR, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_bxork(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ktorefvalue(rc);

        let result = self.refvalue_op(VMExecOperator::BXOR, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_bxor(&mut self, ra: usize, rb: usize, rc: usize) -> Result<(), ExecError> {
        let a = self.ctx.get(rb);
        let b = self.ctx.get(rc);

        let result = self.refvalue_op(VMExecOperator::BXOR, a, b)?;
        self.ctx.set(ra, result);
        Ok(())
    }

    fn exec_eqi(&mut self, ra: usize, rb: u32, k: bool) -> Result<(), ExecError> {
        let a = self.ctx.get(ra);
        let b = RefValue::from(rb as i64);
        let op = if k {
            VMExecOperator::EQ
        } else {
            VMExecOperator::NE
        };

        let result = self.refvalue_op(op, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(false)) {
            self.ctx.pc += 1;
        }
        Ok(())
    }

    fn exec_eqk(&mut self, ra: usize, rb: usize, k: bool) -> Result<(), ExecError> {
        let a = self.ctx.get(ra);
        let b = self.ktorefvalue(rb);
        let op = if k {
            VMExecOperator::EQ
        } else {
            VMExecOperator::NE
        };

        let result = self.refvalue_op(op, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(false)) {
            self.ctx.pc += 1;
        }
        Ok(())
    }

    fn exec_eq(&mut self, ra: usize, rb: usize, k: bool) -> Result<(), ExecError> {
        let a = self.ctx.get(ra);
        let b = self.ctx.get(rb);
        let op = if k {
            VMExecOperator::EQ
        } else {
            VMExecOperator::NE
        };

        let result = self.refvalue_op(op, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(false)) {
            self.ctx.pc += 1;
        }
        Ok(())
    }

    fn exec_lt(&mut self, ra: usize, rb: usize, k: bool) -> Result<(), ExecError> {
        let a = self.ctx.get(ra);
        let b = self.ctx.get(rb);
        let op = if k {
            VMExecOperator::LT
        } else {
            VMExecOperator::GT
        };

        let result = self.refvalue_op(op, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(false)) {
            self.ctx.pc += 1;
        }
        Ok(())
    }

    fn exec_le(&mut self, ra: usize, rb: usize, k: bool) -> Result<(), ExecError> {
        let a = self.ctx.get(ra);
        let b = self.ctx.get(rb);
        let op = if k {
            VMExecOperator::LE
        } else {
            VMExecOperator::GE
        };

        let result = self.refvalue_op(op, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(false)) {
            self.ctx.pc += 1;
        }
        Ok(())
    }

    fn exec_lti(&mut self, ra: usize, rb: u32, k: bool) -> Result<(), ExecError> {
        let a = self.ctx.get(ra);
        let b = RefValue::from(rb as i64);

        let result = self.refvalue_op(VMExecOperator::LT, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(result) if *result != k) {
            self.ctx.pc += 1;
        }
        Ok(())
    }

    fn exec_lei(&mut self, ra: usize, rb: u32, k: bool) -> Result<(), ExecError> {
        let a = self.ctx.get(ra);
        let b = RefValue::from(rb as i64);

        let result = self.refvalue_op(VMExecOperator::LE, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(result) if *result != k) {
            self.ctx.pc += 1;
        }
        Ok(())
    }

    fn exec_gti(&mut self, ra: usize, rb: u32, k: bool) -> Result<(), ExecError> {
        let a = self.ctx.get(ra);
        let b = RefValue::from(rb as i64);

        let result = self.refvalue_op(VMExecOperator::GT, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(result) if *result != k) {
            self.ctx.pc += 1;
        }
        Ok(())
    }

    fn exec_gei(&mut self, ra: usize, rb: u32, k: bool) -> Result<(), ExecError> {
        let a = self.ctx.get(ra);
        let b = RefValue::from(rb as i64);

        let result = self.refvalue_op(VMExecOperator::GE, a, b)?;
        if matches!(result.get().deref(), Value::Boolean(result) if *result != k) {
            self.ctx.pc += 1;
        }
        Ok(())
    }

    fn exec_test(&mut self, ra: usize, k: bool) -> Result<(), ExecError> {
        let a = self.ctx.get(ra);

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
            self.ctx.pc += 1;
        }

        Ok(())
    }

    fn exec_testset(&mut self, ra: usize, rb: usize, k: bool) -> Result<(), ExecError> {
        let a = self.ctx.get(ra);

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
            self.ctx.pc += 1;
        } else {
            let value = self.ctx.get(rb);
            self.ctx.set(ra, value)
        }

        Ok(())
    }

    fn exec_not(&mut self, ra: usize, rb: usize) -> Result<(), ExecError> {
        let b = self.ctx.get(rb);

        let value = match b.get().deref() {
            Value::Nil => RefValue::from(true),
            Value::Integer(v) => RefValue::from(*v == 0),
            Value::Number(v) => RefValue::from(*v == 0f64),
            Value::String(v) if v.eq_ignore_ascii_case("true") => RefValue::from(false),
            Value::String(v) if v.eq_ignore_ascii_case("false") => RefValue::from(true),
            Value::String(v) => RefValue::from(v.len() == 0),
            Value::Boolean(v) => RefValue::from(!*v),
            Value::Table(..) => RefValue::from(false),
        };

        self.ctx.set(ra, value);

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
        };

        Ok(value)
    }

    fn exec_unm(&mut self, ra: usize, rb: usize) -> Result<(), ExecError> {
        let b = self.ctx.get(rb);

        let value = self.refvalue_unm(b)?;
        self.ctx.set(ra, value);

        Ok(())
    }

    fn exec_len(&mut self, ra: usize, rb: usize) -> Result<(), ExecError> {
        let b = self.ctx.get(rb);

        let value = match b.get().deref() {
            Value::Nil => RefValue::from(0),
            Value::Integer(..) => RefValue::from(0),
            Value::Number(..) => RefValue::from(0),
            Value::String(v) => RefValue::from(v.len() as i64),
            Value::Boolean(..) => RefValue::from(0),
            Value::Table(..) => RefValue::from(0),
        };

        self.ctx.set(ra, value);

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
        };

        Ok(value)
    }

    fn exec_bnot(&mut self, ra: usize, rb: usize) -> Result<(), ExecError> {
        let b = self.ctx.get(rb);

        let value = self.refvalue_bnot(b)?;
        self.ctx.set(ra, value);
        Ok(())
    }
}
