use crate::{
    code::codelimit,
    lexer::Token,
    object::{ExprDesc, VarKind},
};

use super::{FuncState, ParseErr, ParseLex, Parser};

pub(super) struct ParseStmt<'s, 't> {
    fs: &'s mut FuncState,
    p: &'t mut Parser,
}

#[macro_export]
macro_rules! match_token {
    (consume: $parser: expr, $token: pat) => {
        $parser.lex_mut(|x| match &x.token {
            $token => {
                x.token_next();
                Ok(())
            }
            _ => Err(ParseErr::UnexpectedSymbol),
        })
    };
    (test_consume: $parser: expr, $token: pat) => {
        $parser.lex_mut(|x| match &x.token {
            $token => {
                x.token_next();
                true
            }
            _ => false,
        })
    };
    (test: $parser: expr, $token: pat) => {
        $parser.lex(|x| matches!(x.token, $token))
    };
}

impl<'s, 't> ParseStmt<'s, 't> {
    pub(super) fn new(fs: &'s mut FuncState, p: &'t mut Parser) -> Self {
        Self { fs, p }
    }

    pub(super) fn block_follow(&self, withuntil: bool) -> bool {
        if match_token!(test: self.p, Token::Else | Token::ElseIf | Token::End | Token::EOF) {
            true
        } else if match_token!(test: self.p, Token::Until) {
            withuntil
        } else {
            false
        }
    }

    // test_then_block ::= [ `if` | `elseif` ] cond_exp `then` block_stmt
    pub(super) fn test_then_block(&mut self, _escape: &mut usize) -> Result<(), ParseErr> {
        log::debug!("parse test_then_block_stmt");

        // parse `if` | `elseif`
        self.p.parse_lex().skip();
        // parse cond_exp
        let mut cond_expr = ExprDesc::new();
        self.p.parse_expr(self.fs, &mut cond_expr).expr_exp()?;
        // parse `then`
        match_token!(consume: self.p, Token::Then)?;
        // parse block_stmt
        self.block_stmt()?;

        Ok(())
    }

    pub(super) fn block_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse block_stmt");

        self.stmtlist()?;

        Ok(())
    }

    // if_stmt ::=  `if` cond_exp `then` block_stmt
    //              { `elseif` cond_exp `then` block_stmt }
    //              [ `else` block_stmt ]
    //              `end`
    pub(super) fn if_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse if_stmt");

        let mut escape_list = codelimit::NO_JMP;

        // parse `if` cond_exp `then` block_stmt
        self.test_then_block(&mut escape_list)?;
        // parse `elseif` cond_exp `then` block_stmt
        while match_token!(test: self.p, Token::ElseIf) {
            self.test_then_block(&mut escape_list)?;
        }
        // parse `else` block
        if match_token!(test_consume: self.p, Token::Else) {
            self.block_stmt()?;
        }
        // parse `end`
        match_token!(consume: self.p, Token::End)?;

        Ok(())
    }

    // while_stmt ::= `while` cond_exp `do` block_stmt `end`
    pub(super) fn while_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse where_stmt");

        // parse `while`
        self.p.parse_lex().skip();
        // parse cond_exp
        let _condexit = self
            .p
            .parse_expr(self.fs, &mut ExprDesc::new())
            .cond_exp()?;
        // parse `do`
        match_token!(consume: self.p, Token::Do)?;
        // parse block_stmt
        self.block_stmt()?;
        // parse `end`
        match_token!(consume: self.p, Token::End)?;

        Ok(())
    }

    // forbody_stmt ::= `do` block_stmt
    pub(super) fn forbody_stmt(
        &mut self,
        _base: u32,
        _nvars: u32,
        _isgen: bool,
    ) -> Result<(), ParseErr> {
        log::debug!("parse forbody_stmt");

        // parse `do`
        match_token!(consume: self.p, Token::Do)?;
        // parse block_stmt
        self.block_stmt()?;

        Ok(())
    }

    // fornum_stmt ::=  name `=` exp `,` exp [`,` exp] forbody_stmt
    pub(super) fn fornum_stmt(&mut self, _varname: &str) -> Result<(), ParseErr> {
        log::debug!("parse fornum_stmt");

        // parse `=`
        match_token!(consume: self.p, Token::Operator('='))?;
        // parse exp (initial value)
        self.p
            .parse_expr(self.fs, &mut ExprDesc::new())
            .expr_exp()?;
        // parse `,`
        match_token!(consume: self.p, Token::Operator(','))?;
        // parse exp (end value)
        self.p
            .parse_expr(self.fs, &mut ExprDesc::new())
            .expr_exp()?;
        // parse [ `,` exp ] (step value)
        if match_token!(test_consume: self.p, Token::Operator(',')) {
            self.p
                .parse_expr(self.fs, &mut ExprDesc::new())
                .expr_exp()?;
        }
        // parse forbody_stmt
        self.forbody_stmt(0, 1, false)?;

        Ok(())
    }

    // forlist_stmt ::= name { `,` name } `in` explist_exp forbody_stmt
    pub(super) fn forlist_stmt(&mut self, _indexname: &str) -> Result<(), ParseErr> {
        log::debug!("parse forlist_stmt");

        // parse { `,` name }
        while match_token!(test_consume: self.p, Token::Operator(',')) {
            let _name = self.p.parse_lex().name()?;
        }
        // parse `in`
        match_token!(consume: self.p, Token::In)?;
        // parse explist_exp
        let _ne = self
            .p
            .parse_expr(self.fs, &mut ExprDesc::new())
            .exprlist_exp()?;
        // parse forbody_stmt
        self.forbody_stmt(0, 0, true)?;

        Ok(())
    }

    // for_stmt ::= `for` (fornum_stmt | forlist_stmt) `end`
    pub(super) fn for_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse for_stmt");

        // parse `for`
        self.p.parse_lex().skip();
        // parse name
        let var_name = ParseLex::new(self.p).name()?;
        // parse (fornum_stmt | forlist_stmt)
        match self.p.lex(|x| x.token.clone()) {
            Token::Operator('=') => self.fornum_stmt(&var_name)?,
            Token::Operator(',') | Token::In => self.forlist_stmt(&var_name)?,
            _ => return Err(ParseErr::UnexpectedSymbol),
        }
        // parse `end`
        match_token!(consume: self.p, Token::End)?;

        Ok(())
    }

    // do_stmt ::= `do` block_stmt `end`
    pub(super) fn do_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse do_stmt");

        // parse `do`
        self.p.parse_lex().skip();
        // parse block_stmt
        self.block_stmt()?;
        // parse `end`
        match_token!(consume: self.p, Token::End)?;

        Ok(())
    }

    // repeat_stmt ::= `repeat` block_stmt `until` cond_exp
    pub(super) fn repeat_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse repeat_stmt");

        // parse `repeat`
        self.p.parse_lex().skip();
        // parse block_stmt
        self.block_stmt()?;
        // parse `until`
        match_token!(consume: self.p, Token::Until)?;
        // parse cond_exp
        let mut _condexit = self
            .p
            .parse_expr(self.fs, &mut ExprDesc::new())
            .cond_exp()?;

        Ok(())
    }

    // parlist_stmt ::= [ {name `,`} (name | `...`) ]
    pub(super) fn parlist_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse parlist_stmt");

        let mut _nparams = 0;
        let mut isvararg = false;
        if !match_token!(test: self.p, Token::Operator(')')) {
            loop {
                match self.p.lex(|x| x.token.clone()) {
                    // parse name
                    Token::Name(name) => {
                        self.p.pushloc(&self.fs, &name);
                        self.p.parse_lex().skip();
                        _nparams += 1;
                    }
                    // parse `...`
                    Token::Dots => {
                        self.p.parse_lex().skip();
                        isvararg = true;
                    }
                    _ => return Err(ParseErr::BadUsage),
                }

                // parse `,`
                if isvararg || !match_token!(test_consume: self.p, Token::Operator(',')) {
                    break;
                }
            }
        }

        Ok(())
    }

    // body_stmt ::= `(` parlist `)` block_stmt `end`
    pub(super) fn body_stmt(
        &mut self,
        _expr: &mut ExprDesc,
        ismethod: bool,
    ) -> Result<(), ParseErr> {
        log::debug!("parse body_stmt");

        let mut fs = FuncState::new();

        // parse `(`
        match_token!(consume: self.p, Token::Operator('('))?;
        if ismethod {
            self.p.pushloc(&fs, "self");
            self.p.parse_var(&mut fs).adjust_localvars(1);
        }
        // parse parlist_stmt
        self.p.parse_stmt(&mut fs).parlist_stmt()?;
        // parse `)`
        match_token!(consume: self.p, Token::Operator(')'))?;
        // parse block_stmt
        self.p.parse_stmt(&mut fs).block_stmt()?;
        // parse `end`
        match_token!(consume: self.p, Token::End)?;
        Ok(())
    }

    // fieldsel_stmt -> [`.` | `:`] name
    pub(super) fn fieldsel_stmt(&mut self, _expr: &mut ExprDesc) -> Result<(), ParseErr> {
        log::debug!("parse fieldsel_stmt");

        // parse [`.` | `:`]
        self.p.parse_lex().skip();
        // parse name
        let _name = self.p.parse_lex().name()?;

        Ok(())
    }

    // funcname_stmt -> name { fieldsel_stmt } [ `:` name ]
    pub(super) fn funcname_stmt(&mut self, expr: &mut ExprDesc) -> Result<bool, ParseErr> {
        log::debug!("parse funcname_stmt");

        // parse name
        let _name = self.p.parse_lex().name()?;
        // parse { fieldsel_stmt }
        while match_token!(test: self.p, Token::Operator('.')) {
            self.fieldsel_stmt(expr)?;
        }
        let mut ismethod = false;
        // parse [ `:` name ]
        if match_token!(test: self.p, Token::Operator(':')) {
            self.fieldsel_stmt(expr)?;
            ismethod = true;
        }

        Ok(ismethod)
    }

    // func_stmt := `function` funcname_stmt body_stmt
    pub(super) fn func_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse func_stmt");

        let mut name_expr = ExprDesc::new();
        let mut func_expr = ExprDesc::new();

        // parse `function`
        self.p.parse_lex().skip();
        // parse funcname_stmt
        let ismethod = self.funcname_stmt(&mut name_expr)?;
        // parse body_stmt
        self.body_stmt(&mut func_expr, ismethod)?;

        Ok(())
    }

    // localfunc_stmt ::= name body_stmt
    pub(super) fn localfunc_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse localfunc_stmt");

        // parse name
        let _name = self.p.parse_lex().name()?;
        // parse body_stmt
        self.body_stmt(&mut ExprDesc::new(), false)?;

        Ok(())
    }

    // attrib_stmt ::= [`<` name `>`]
    pub(super) fn attrib_stmt(&mut self) -> Result<VarKind, ParseErr> {
        log::debug!("parse attrib_stmt");

        // parse `<`
        if match_token!(test_consume: self.p, Token::Operator('<')) {
            // parse name
            let attr = self.p.parse_lex().name()?;
            // parse `>`
            match_token!(consume: self.p, Token::Operator('>'))?;

            match attr.as_str() {
                "const" => return Ok(VarKind::CONST),
                "close" => return Ok(VarKind::TOCLOSE),
                _ => return Err(ParseErr::BadUsage),
            }
        }
        Ok(VarKind::REG)
    }

    // localvar_stmt ::= name attrib_stmt { `,` name attrib_stmt }[ `=` exprlist_exp ]
    pub(super) fn localvar_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse localvar_stmt");

        loop {
            // parse name
            let _name = self.p.parse_lex().name()?;
            // parse attrib_stmt
            let _kind = self.attrib_stmt()?;
            // parse `,`
            if !match_token!(test_consume: self.p, Token::Operator(',')) {
                break;
            }
        }
        // parse `=`
        if match_token!(test_consume: self.p, Token::Operator('=')) {
            // parse exprlist_exp
            self.p
                .parse_expr(self.fs, &mut ExprDesc::new())
                .exprlist_exp()?;
        }

        Ok(())
    }

    // local_stmt ::= `local` [localfunc_stmt | localvar_stmt]
    pub(super) fn local_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse local_stmt");

        // parse `local`
        self.p.parse_lex().skip();
        // parse `function`
        if match_token!(test_consume: self.p, Token::Function) {
            // parse localfunc_stmt
            self.localfunc_stmt()
        } else {
            // parse localvar_stmt
            self.localvar_stmt()
        }
    }

    // label_stmt ::= `::` name `::`
    pub(super) fn label_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse label_stmt");

        // parse `::`
        self.p.parse_lex().skip();
        // parse name
        let _name = self.p.parse_lex().name()?;
        // parse `::`
        self.p.parse_lex().skip();

        Ok(())
    }

    // return_stmt ::= `return` [explist_exp] [`;`]
    pub(super) fn return_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse return_stmt");

        // parse `return`
        self.p.parse_lex().skip();

        // parse exprlist_exp
        if !self.block_follow(true) && !match_token!(test: self.p, Token::Operator(';')) {
            self.p
                .parse_expr(self.fs, &mut ExprDesc::new())
                .exprlist_exp()?;
        }

        // parse `;`
        match_token!(test_consume: self.p, Token::Operator(';'));

        Ok(())
    }

    // break_stmt ::= `break`
    pub(super) fn break_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse break_stmt");

        // parse `break`
        self.p.parse_lex().skip();

        Ok(())
    }

    // goto_stmt ::= `goto` name
    pub(super) fn goto_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse goto_stmt");

        // parse `goto`
        self.p.parse_lex().skip();
        // parse name
        let _name = self.p.parse_lex().name()?;

        Ok(())
    }

    // assigment_stmt ::= suffixed_exp restassign_stmt
    // restassign_stmt ::= ',' suffixed_exp restassign_stmt | `=` explist_exp
    pub(super) fn restassign_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse restassign_stmt");

        // parse `,`
        if match_token!(test_consume: self.p, Token::Operator(',')) {
            // parse suffixed_exp
            self.p
                .parse_expr(self.fs, &mut ExprDesc::new())
                .suffixed_exp()?;
            // parse restassign_stmt
            self.restassign_stmt()?;
        } else {
            // parse `=`
            match_token!(consume: self.p, Token::Operator('='))?;
            // parse exprlist_exp
            self.p
                .parse_expr(self.fs, &mut ExprDesc::new())
                .exprlist_exp()?;
        }
        Ok(())
    }

    // expr_stmt -> func_stmt | assigment_stmt
    pub(super) fn expr_stmt(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse expr_stmt");

        // parse suffixed_exp
        self.p
            .parse_expr(self.fs, &mut ExprDesc::new())
            .suffixed_exp()?;

        if match_token!(test: self.p, Token::Operator('=' | ',')) {
            self.restassign_stmt()?;
        } else {
            // func
        }

        Ok(())
    }

    pub(super) fn stmt(&mut self) -> Result<(), ParseErr> {
        match self.p.lex(|x| x.token.clone()) {
            Token::Operator(';') => self.p.parse_lex().skip(),
            Token::If => self.if_stmt()?,
            Token::While => self.while_stmt()?,
            Token::Do => self.do_stmt()?,
            Token::For => self.for_stmt()?,
            Token::Repeat => self.repeat_stmt()?,
            Token::Function => self.func_stmt()?,
            Token::Local => self.local_stmt()?,
            Token::Label => self.label_stmt()?,
            Token::Return => self.return_stmt()?,
            Token::Break => self.break_stmt()?,
            Token::Goto => self.goto_stmt()?,
            _ => self.expr_stmt()?,
        }

        Ok(())
    }

    pub(super) fn stmtlist(&mut self) -> Result<(), ParseErr> {
        log::debug!("parse stmtlist");

        while self.p.lex(|x| !x.token.block_follow(true)) {
            if self.p.lex(|x| matches!(x.token, Token::Return)) {
                self.stmt()?;
                break;
            }
            self.stmt()?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::Logger;

    use super::*;

    #[test]
    fn func_stmt() {
        log::set_logger(&Logger {}).unwrap();
        log::set_max_level(log::LevelFilter::Debug);

        let mut parser = Parser::new(
            " function abc(d, e)
            local f = g + h - k * l;
            local function ff(k) 
                return 12.3;
            end
            if 1 + 2 == 3 then
                func_call(5);
            elseif 3 + 4 >= 5 then
                a = 7;
            else
                b = func_call(7, 5);
            end
            while a == 6 do
                a = a + 1;
            end
            do
                b = b + a;
            end
            for key = 1, 10, 5 do
            end
            for key, value in 1, 20 do
                c = \"abc\";
            end
            repeat
                a = a + b;
                break;
            until c == 5;

            goto label_name;

            ::label_name::
            end",
        );

        let result = parser.parse();

        assert!(result.is_ok());
    }
}
