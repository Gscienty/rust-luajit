use crate::{
    code::{codelimit, Code, OpCode},
    lexer::Token,
    match_token,
    object::{ExprDesc, ExprKind, RefValue},
};

use super::{BinOpr, FuncState, ParseErr, Parser, UnOpr};

pub(super) struct ParseExpr<'s, 't, 'v> {
    fs: &'s mut FuncState,
    p: &'t mut Parser,

    expr: &'v mut ExprDesc,
}

macro_rules! init_exp {
    ($expr: expr, $kind: expr, $info: expr) => {{
        $expr.t = codelimit::NO_JMP;
        $expr.f = codelimit::NO_JMP;
        $expr.kind = $kind;
        $expr.info = $info;
    }};
    ($expr: expr, $kind: expr, $info: expr, $val: expr) => {{
        $expr.t = codelimit::NO_JMP;
        $expr.f = codelimit::NO_JMP;
        $expr.kind = $kind;
        $expr.info = $info;
        $expr.val = Some($val);
    }};
}

impl<'s, 't, 'v> ParseExpr<'s, 't, 'v> {
    pub(super) fn new(fs: &'s mut FuncState, p: &'t mut Parser, expr: &'v mut ExprDesc) -> Self {
        Self { fs, p, expr }
    }

    pub(super) fn then<U, F>(self, f: F) -> U
    where
        F: FnOnce(Self) -> U,
    {
        f(self)
    }

    // recfield_exp ::= (name | `[` exp `]`) = exp
    pub(super) fn recfield_exp(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse recfield_exp");

        match self.p.lex(|x| x.token.clone()) {
            Token::Name(_name) => {
                // parse name
                self.p.parse_lex().skip();
            }
            Token::Operator('[') => {
                // parse `[`
                self.p.parse_lex().skip();
                // parse exp
                self.expr_exp()?;
                // parse `]`
                match_token!(consume: self.p, Token::Operator(']'))?;
            }
            _ => return Err(ParseErr::BadUsage),
        }
        // parse `=`
        match_token!(consume: self.p, Token::Operator('='))?;
        // parse exp
        self.expr_exp()?;

        Ok(())
    }

    // listfield_exp ::= expr_exp
    pub(super) fn listfield_exp(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse listfield_exp");

        self.expr_exp()
    }

    // field_exp ::= listfield_exp | recfield_exp
    pub(super) fn field_exp(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse field_exp");

        match self.p.lex(|x| x.token.clone()) {
            Token::Name(_) => {
                let lookahead = self.p.lex_mut(|x| x.token_lookahead())?;
                if matches!(lookahead, Token::Operator('=')) {
                    self.recfield_exp()?;
                } else {
                    self.listfield_exp()?;
                }
            }
            Token::Operator('[') => self.recfield_exp()?,
            _ => self.listfield_exp()?,
        }
        Ok(())
    }

    // constructor_exp ::= `{` [ field_exp { sep field_exp } [ sep ] ] `}`
    // sep ::= `,` | `;`
    pub(super) fn constructor_exp(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse constructor_exp");

        // parse `{`
        match_token!(consume: self.p, Token::Operator('{'))?;

        loop {
            if match_token!(test: self.p, Token::Operator('}')) {
                break;
            }

            // parse field_exp
            self.field_exp()?;

            // parse sep
            if !match_token!(test_consume: self.p, Token::Operator(',' | ';')) {
                break;
            }
        }
        // parse `}`
        match_token!(consume: self.p, Token::Operator('}'))?;

        Ok(())
    }

    // simple_exp -> FLT | INT | STRING | NIL | TRUE | FALSE | ... | constructor_exp | FUNCTION body
    // | suffixed_exp
    pub(super) fn simple_exp(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse simple_exp");

        match self.p.parse_lex().current_token() {
            Token::Integer(val) => init_exp!(self.expr, ExprKind::KFLT, 0, RefValue::from(val)),
            Token::Number(val) => init_exp!(self.expr, ExprKind::KINT, 0, RefValue::from(val)),
            Token::String(val) => {
                init_exp!(self.expr, ExprKind::KSTR, 0, RefValue::from(val.as_str()))
            }
            Token::Nil => init_exp!(self.expr, ExprKind::NIL, 0),
            Token::True => init_exp!(self.expr, ExprKind::TRUE, 0),
            Token::False => init_exp!(self.expr, ExprKind::FALSE, 0),
            Token::Dots => {
                if !self.fs.prop().proto.prop().is_vararg {
                    return Err(ParseErr::BadUsage);
                }
                init_exp!(
                    self.expr,
                    ExprKind::VARARG,
                    self.fs.emit(Code::new_abc(OpCode::VARARG, 0, 0, 1, false)?)
                )
            }
            Token::Operator('{') => {
                self.constructor_exp()?;
                return Ok(());
            }
            Token::Function => {
                // parse `function`
                self.p.parse_lex().skip();
                // parse body_stmt
                self.p.parse_stmt(self.fs).body_stmt(self.expr, false)?;

                return Ok(());
            }
            _ => {
                // parse suffixed_exp
                self.suffixed_exp()?;

                return Ok(());
            }
        }
        self.p.parse_lex().skip();

        Ok(())
    }

    // sub_exp -> (simple_exp | unop sub_exp) { binop sub_exp }
    pub(super) fn sub_exp(&mut self, limit: u8) -> Result<BinOpr, ParseErr> {
        log::debug!("parse sub_exp");

        let unop = UnOpr::from(self.p.parse_lex().current_token());

        if matches!(unop, UnOpr::NoOpr) {
            // parse simple_exp
            self.simple_exp()?;
        } else {
            // parse unop
            self.p.parse_lex().skip();
            // parse sub_exp
            self.sub_exp(BinOpr::UNARY_PRI)?;
        }

        let mut binop = BinOpr::from(self.p.parse_lex().current_token());
        while !matches!(binop, BinOpr::NoOpr) && binop.lpri() > limit {
            // parse binop
            self.p.parse_lex().skip();
            // parse sub_exp
            let next_binop = self.sub_exp(binop.rpri())?;

            binop = next_binop;
        }

        Ok(binop)
    }

    pub(super) fn expr_exp(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse expr_exp");

        self.sub_exp(BinOpr::INIT_PRI)?;

        Ok(())
    }

    // exprlist_exp -> expr { `,` expr }
    pub(super) fn exprlist_exp(&mut self) -> Result<usize, ParseErr> {
        log::debug!("parse exprlist_exp");

        let mut n = 1;
        self.expr_exp()?;

        while self.p.lex_mut(|x| match &x.token {
            Token::Operator(',') => {
                x.token_next();
                true
            }
            _ => false,
        }) {
            self.expr_exp()?;
            n += 1;
        }

        Ok(n)
    }

    pub(super) fn cond_exp(&mut self) -> Result<usize, ParseErr> {
        log::debug!("parse cond_exp");

        self.expr_exp()?;
        if matches!(self.expr.kind, ExprKind::NIL) {
            self.expr.kind = ExprKind::FALSE;
        }

        Ok(self.expr.f)
    }

    // primary_exp ::= name | `(` exp `)`
    pub(super) fn primary_exp(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse primary_exp");

        match self.p.lex(|x| x.token.clone()) {
            Token::Operator('(') => {
                // parse `(`
                self.p.parse_lex().skip();
                // parse exp
                self.expr_exp()?;
                // parse `)`
                match_token!(consume: self.p, Token::Operator(')'))?;
            }
            Token::Name(_name) => {
                // parse name
                self.p.parse_lex().skip();
            }
            _ => return Err(ParseErr::BadUsage),
        }
        Ok(())
    }

    // funcargs_exp ::= ( `(` [ exprlist_exp ] `)` | constructor_exp | string )
    pub(super) fn funcargs_exp(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse funcargs_exp");

        match self.p.lex(|x| x.token.clone()) {
            Token::Operator('(') => {
                // parse `(`
                self.p.parse_lex().skip();
                // parse exprlist_exp
                self.exprlist_exp()?;
                // parse `)`
                match_token!(consume: self.p, Token::Operator(')'))?;
            }
            Token::Operator('{') => {
                self.constructor_exp()?;
            }
            Token::String(_str) => {
                // parse string
                self.p.parse_lex().skip();
            }
            _ => return Err(ParseErr::BadUsage),
        }
        Ok(())
    }

    // suffixed_exp ::= primary_exp { `.` name | `[` exp `]` | `:` name funcargs_exp | funcargs_exp }
    pub(super) fn suffixed_exp(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse suffixed_exp");

        // parse primary_exp
        self.primary_exp()?;

        loop {
            match self.p.lex(|x| x.token.clone()) {
                Token::Operator('.') => {
                    // parse '.'
                    self.p.parse_lex().skip();
                    // parse name
                    let _name = self.p.parse_lex().name()?;
                }
                Token::Operator('[') => {
                    // parse `[`
                    self.p.parse_lex().skip();
                    // parse exp
                    self.expr_exp()?;
                    // parse `]`
                    match_token!(consume: self.p, Token::Operator(']'))?;
                }
                Token::Operator(':') => {
                    // parse `:`
                    self.p.parse_lex().skip();
                    // parse name
                    let _name = self.p.parse_lex().name()?;
                    // parse funcargs_exp
                    self.funcargs_exp()?;
                }
                Token::Operator('(') | Token::String(_) | Token::Operator('{') => {
                    // parse funcargs_exp
                    self.funcargs_exp()?;
                }
                _ => break Ok(()),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expr_unm() {
        let mut parser = Parser::new("-10");
        parser.lex_mut(|x| x.token_next());
        let mut fs = FuncState::new();
        let result = parser.parse_expr(&mut fs, &mut ExprDesc::new()).expr_exp();

        println!("{}", result.is_ok())
    }
}
