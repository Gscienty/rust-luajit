use crate::{lexer::Token, match_token};

use super::{BinOpr, Expr, ParseErr, Parser, UnOpr};

pub(super) struct ParseExpr<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseExpr<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
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

        self.expr_exp()?;

        Ok(())
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
    //              | suffixed_exp
    pub(super) fn simple_exp(&mut self) -> Result<Expr, ParseErr> {
        log::debug!("parse simple_exp");

        let expr = match self.p.parse_lex().current_token() {
            Token::Integer(val) => Expr::from(val),
            Token::Number(val) => Expr::from(val),
            Token::String(val) => Expr::from(val.as_str()),
            Token::Nil => Expr::nil(),
            Token::True => Expr::from(true),
            Token::False => Expr::from(false),
            Token::Dots => {
                if !self.p.fs.prop().proto.prop().is_vararg {
                    return Err(ParseErr::BadUsage);
                }
                Expr::vararg(self.p.emiter().emit_vararg())
            }
            Token::Operator('{') => {
                self.constructor_exp()?;
                return Ok(Expr::todo());
            }
            Token::Function => {
                // parse `function`
                self.p.parse_lex().skip();
                // parse body_stmt
                self.p.parse_stmt().body_stmt(false)?;

                return Ok(Expr::todo());
            }
            _ => return self.suffixed_exp(),
        };
        self.p.parse_lex().skip();

        Ok(expr)
    }

    // sub_exp -> (simple_exp | unop sub_exp) { binop sub_exp }
    pub(super) fn sub_exp(&mut self, limit: u8) -> Result<(BinOpr, Expr), ParseErr> {
        log::debug!("parse sub_exp");

        let unop = UnOpr::from(self.p.parse_lex().current_token());

        let mut exp = if matches!(unop, UnOpr::NoOpr) {
            // parse simple_exp
            self.simple_exp()?
        } else {
            // parse unop
            self.p.parse_lex().skip();
            // parse sub_exp
            let (_, exp) = self.sub_exp(BinOpr::UNARY_PRI)?;
            self.p.parse_code().prefix(unop, exp)?
        };

        let mut binop = BinOpr::from(self.p.parse_lex().current_token());
        while !matches!(binop, BinOpr::NoOpr) && binop.lpri() > limit {
            // parse binop
            self.p.parse_lex().skip();

            exp = self.p.parse_reg().infix(binop, exp)?;
            let (nbop, nexp) = self.sub_exp(binop.rpri())?;
            exp = self.p.parse_code().posfix(binop, exp, nexp)?;

            binop = nbop;
        }

        Ok((binop, exp))
    }

    pub(super) fn expr_exp(&mut self) -> Result<Expr, ParseErr> {
        log::debug!("parse expr_exp");

        let (_, exp) = self.sub_exp(BinOpr::INIT_PRI)?;

        Ok(exp)
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

    // primary_exp ::= name | `(` exp `)`
    pub(super) fn primary_exp(&mut self) -> Result<Expr, ParseErr> {
        log::debug!("parse primary_exp");

        match self.p.lex(|x| x.token.clone()) {
            Token::Operator('(') => {
                // parse `(`
                self.p.parse_lex().skip();
                // parse exp
                let exp = self.expr_exp()?;
                // parse `)`
                match_token!(consume: self.p, Token::Operator(')'))?;

                self.p.parse_var().discharge_tovar(exp)
            }
            Token::Name(_name) => {
                // parse name
                self.p.parse_lex().skip();

                Ok(Expr::todo())
            }
            _ => Err(ParseErr::BadUsage),
        }
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
    pub(super) fn suffixed_exp(&mut self) -> Result<Expr, ParseErr> {
        log::debug!("parse suffixed_exp");

        // parse primary_exp
        let exp = self.primary_exp()?;

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
                _ => break Ok(exp),
            }
        }
    }
}
