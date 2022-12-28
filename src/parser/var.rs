use crate::{
    check_token,
    code::{codelimit, InterCode},
    consume_token,
    lexer::Token,
    object::{ConstantValue, Expr, ExprValue, LocVar, RefValue, Upval, Var, VarKind},
};

use super::{fscope::FuncState, ParseErr, Parser};

impl Parser {
    fn get_localvar_aux(&self, fs: &FuncState, index: usize) -> Option<&Var> {
        self.actvar.get(fs.prop().first_local + index)
    }

    fn get_localvar(&self, index: usize) -> Option<&Var> {
        self.get_localvar_aux(&self.fs, index)
    }

    fn get_localvar_mut(&mut self, index: usize) -> Option<&mut Var> {
        self.actvar.get_mut(self.fs.prop().first_local + index)
    }

    pub(super) fn reglevel(&self, nvar: usize) -> usize {
        for index in (0..nvar).rev() {
            if let Some(var) = self.get_localvar(index) {
                if !matches!(var.kind, VarKind::CTC) {
                    return var.r_idx + 1;
                }
            }
        }

        0
    }

    pub(super) fn nvarstack(&self) -> usize {
        self.reglevel(self.fs.prop().nactvar)
    }

    pub(super) fn new_localvar(&mut self, var: Var) -> usize {
        self.actvar.push(var);

        self.actvar.len() - 1 - self.fs.prop().first_local
    }

    pub(super) fn removevars(&mut self, level: usize) {
        let pc = self.pc();

        while self.fs.prop().nactvar > level {
            self.fs.prop_mut().nactvar -= 1;
            let index = match self.actvar.pop() {
                Some(var) if !matches!(var.kind, VarKind::CTC) => var.p_idx,
                _ => continue,
            };

            if let Some(var) = self.fs.prop().proto.prop_mut().locvars.get_mut(index) {
                var.end_pc = pc;
            }
        }
    }

    pub(super) fn new_upvar(&mut self, upvar: Upval) -> usize {
        self.fs.prop().proto.prop_mut().upvars.push(upvar);
        self.fs.prop_mut().nups += 1;

        self.fs.prop().nups - 1
    }

    fn search_localvar(&self, fs: &FuncState, name: &str) -> Option<Expr> {
        let nvars = fs.prop().nactvar;

        for index in (0..nvars).rev() {
            if let Some(var) = self.get_localvar_aux(fs, index) {
                if var.name.ne(name) {
                    continue;
                }

                return Some(match var.kind {
                    VarKind::CTC => Expr::econst(fs.prop().first_local + index),
                    _ => Expr::local(index, var.r_idx),
                });
            }
        }
        None
    }

    fn search_upvar(&self, fs: &FuncState, name: &str) -> Option<usize> {
        let nupvars = fs.prop().nups;

        for index in 0..nupvars {
            if let Some(upvar) = fs.prop().proto.prop().upvars.get(index) {
                if upvar.name.ne(name) {
                    continue;
                }

                return Some(index);
            }
        }
        None
    }

    fn singlevar_pfs(&self, fs: &FuncState, name: &str) -> Result<Expr, ParseErr> {
        if let Some(fs) = &fs.prop().prev {
            let exp = self.singlevar_aux(fs, name, false)?;
            let nups = fs.prop().nups;
            let mut upvar = Upval::new(name);

            match exp.value {
                ExprValue::Local(varindex, regindex) => {
                    upvar.instack = true;
                    upvar.idx = regindex;
                    upvar.kind = match self.get_localvar_aux(fs, varindex) {
                        Some(var) => var.kind,
                        _ => return Err(ParseErr::BadUsage),
                    };
                }
                ExprValue::Upval(index) => {
                    upvar.instack = false;
                    upvar.idx = index;
                    upvar.kind = match fs.prop().proto.prop().upvars.get(index) {
                        Some(upvar) => upvar.kind,
                        _ => return Err(ParseErr::BadUsage),
                    };
                }
                _ => return Ok(exp),
            };

            fs.prop().proto.prop_mut().upvars.push(upvar);
            fs.prop_mut().nups = nups + 1;

            Ok(Expr::upval(nups))
        } else {
            Ok(Expr::void())
        }
    }

    fn singlevar_resetbl(&self, fs: &FuncState, exp: Expr, base: bool) -> Result<Expr, ParseErr> {
        match exp.value {
            ExprValue::Local(vidx, _) if !base => {
                let mut bl = fs.prop().block.clone();
                while bl.prop().nactvar > vidx {
                    let pbl = bl.prop().prev.clone();

                    if let Some(pbl) = pbl {
                        bl = pbl;
                    } else {
                        return Err(ParseErr::BadUsage);
                    }
                }

                bl.prop_mut().upval = true;
                bl.prop_mut().needclose = true;
            }
            _ => {}
        }

        Ok(exp)
    }

    fn singlevar_aux(&self, fs: &FuncState, name: &str, base: bool) -> Result<Expr, ParseErr> {
        if let Some(var) = self.search_localvar(fs, name) {
            self.singlevar_resetbl(fs, var, base)
        } else if let Some(index) = self.search_upvar(fs, name) {
            Ok(Expr::upval(index))
        } else {
            self.singlevar_pfs(fs, name)
        }
    }

    pub(super) fn singlevar(&mut self, name: &str) -> Result<Expr, ParseErr> {
        let exp = self.singlevar_aux(&self.fs, name, true)?;
        if matches!(exp.value, ExprValue::Void) {
            let envexp = self.singlevar_aux(&self.fs, Parser::ENV, true)?;
            let exp = self.expanyregup(envexp)?;

            self.indexed(exp, Expr::from(name))
        } else {
            Ok(exp)
        }
    }

    pub(super) fn indexed(&mut self, tab: Expr, key: Expr) -> Result<Expr, ParseErr> {
        let key = if matches!(
            key.value,
            ExprValue::String(..)
                | ExprValue::Integer(..)
                | ExprValue::Float(..)
                | ExprValue::Bool(..)
        ) {
            self.expk(key)?
        } else {
            key
        };

        let tab = if matches!(tab.value, ExprValue::Upval(..))
            && matches!(key.value, ExprValue::K(kidx) if matches!(self.constant_pool.borrow().get(kidx), Some(ConstantValue::String(..))))
        {
            self.expanyreg(tab)?
        } else {
            tab
        };

        let exp = if let ExprValue::Upval(reg) = tab.value {
            let ikey = if let ExprValue::K(kidx) = key.value {
                kidx
            } else {
                return Err(ParseErr::BadUsage);
            };

            Expr::indexup(reg, ikey)
        } else {
            let treg = match tab.value {
                ExprValue::Local(_, reg) => reg,
                ExprValue::Nonreloc(reg) => reg,
                _ => return Err(ParseErr::BadUsage),
            };

            match key.value {
                ExprValue::K(kidx)
                    if matches!(
                        self.constant_pool.borrow().get(kidx),
                        Some(ConstantValue::String(..))
                    ) =>
                {
                    Expr::indexstr(treg, kidx)
                }
                ExprValue::Integer(kval) => Expr::indexi(treg, kval),
                _ => {
                    let key = self.expanyreg(key)?;
                    let kreg = self.nonreloc(&key)?;

                    Expr::indexed(treg, kreg)
                }
            }
        };

        Ok(exp)
    }

    pub(super) fn storevar(&mut self, var: &Expr, exp: Expr) -> Result<(), ParseErr> {
        let mut exp = exp;

        match var.value {
            ExprValue::Local(_, ridx) => {
                self.freeexp(&exp)?;
                self.expreg(exp, ridx)?;
                return Ok(());
            }
            ExprValue::Upval(vreg) => {
                exp = self.expanyreg(exp.clone())?;
                let exreg = self.nonreloc(&exp)?;

                self.emit_setupval(exreg, vreg);
            }
            ExprValue::IndexUp(t, idx) => {
                exp = self.expkreg(exp)?;

                match exp.value {
                    ExprValue::Nonreloc(reg) => self.emit_settabup(t, idx, reg, false),
                    ExprValue::K(reg) => self.emit_settabup(t, idx, reg, true),
                    _ => unreachable!(),
                };
            }
            ExprValue::IndexI(t, idx) => {
                exp = self.expkreg(exp)?;

                match exp.value {
                    ExprValue::Nonreloc(reg) => self.emit_seti(t, idx, reg, false),
                    ExprValue::K(reg) => self.emit_seti(t, idx, reg, true),
                    _ => unreachable!(),
                };
            }
            ExprValue::IndexStr(t, idx) => {
                exp = self.expkreg(exp)?;

                match exp.value {
                    ExprValue::Nonreloc(reg) => self.emit_setfield(t, idx, reg, false),
                    ExprValue::K(reg) => self.emit_setfield(t, idx, reg, true),
                    _ => unreachable!(),
                };
            }
            ExprValue::Indexed(t, idx) => {
                exp = self.expkreg(exp)?;

                match exp.value {
                    ExprValue::Nonreloc(reg) => self.emit_settable(t, idx, reg, false),
                    ExprValue::K(reg) => self.emit_settable(t, idx, reg, true),
                    _ => unreachable!(),
                };
            }
            _ => return Err(ParseErr::BadUsage),
        }

        self.freeexp(&exp)?;

        Ok(())
    }

    fn need_value(&mut self, list: Option<usize>) -> bool {
        let mut list = list;
        while let Some(pc) = list {
            let (_, ins) = self.get_ctrljump(pc);
            match ins {
                Some(ins) if !matches!(ins, InterCode::TESTSET(_, _, _)) => return true,
                _ => list = self.get_jump(pc),
            }
        }

        false
    }

    pub(super) fn nonreloc(&mut self, exp: &Expr) -> Result<usize, ParseErr> {
        match exp.value {
            ExprValue::Nonreloc(reg) => Ok(reg),
            _ => Err(ParseErr::BadUsage),
        }
    }

    fn expreg(&mut self, exp: Expr, reg: usize) -> Result<Expr, ParseErr> {
        let mut exp = self.discreg(exp, reg)?;

        if let ExprValue::Jump(pc) = exp.value {
            self.jump_concatlist(&mut exp.true_jumpto, Some(pc))?;
        }

        if exp.hasjump() {
            let mut pf = None;
            let mut pt = None;
            if self.need_value(exp.true_jumpto) || self.need_value(exp.false_jumpto) {
                let fj = match exp.value {
                    ExprValue::Jump(_) => None,
                    _ => Some(self.emit_jmp()),
                };

                pf = Some(self.emit_lfalseskip(reg));
                pt = Some(self.emit_loadbool(reg, true));

                self.jump_patchtohere(fj)?;
            }

            let final_pc = self.mark_pc();

            self.jump_patchlistaux(exp.false_jumpto, Some(final_pc), Some(reg), pf)?;
            self.jump_patchlistaux(exp.true_jumpto, Some(final_pc), Some(reg), pt)?;
        }

        Ok(Expr::nonreloc(reg))
    }

    pub(super) fn expnextreg(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        let exp = self.discvar(exp)?;
        self.freeexp(&exp)?;
        self.reserve(1);

        let reg = self.fs.prop().freereg - 1;
        self.expreg(exp, reg)
    }

    pub(super) fn expanyreg(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        let exp = self.discvar(exp)?;

        match exp.value {
            ExprValue::Nonreloc(..) => Ok(exp),
            _ => self.expnextreg(exp),
        }
    }

    pub(super) fn expanyregup(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        if exp.hasjump() || !matches!(exp.value, ExprValue::Upval(..)) {
            self.expanyreg(exp)
        } else {
            Ok(exp)
        }
    }

    pub(super) fn reserve(&mut self, n: usize) {
        self.fs.prop_mut().freereg += n;
    }

    pub(super) fn freeexp(&mut self, exp: &Expr) -> Result<(), ParseErr> {
        if let ExprValue::Nonreloc(reg) = exp.value {
            self.freereg(reg)
        } else {
            Ok(())
        }
    }

    pub(super) fn freeexps(&mut self, e1: &Expr, e2: &Expr) -> Result<(), ParseErr> {
        match e1.value {
            ExprValue::Nonreloc(r1) => match e2.value {
                ExprValue::Nonreloc(r2) => {
                    if r1 < r2 {
                        self.freereg(r2)?;
                        self.freereg(r1)?;
                    } else {
                        self.freereg(r1)?;
                        self.freereg(r2)?;
                    }
                }
                _ => self.freereg(r1)?,
            },
            _ => match e2.value {
                ExprValue::Nonreloc(r2) => self.freereg(r2)?,
                _ => {}
            },
        };

        Ok(())
    }

    pub(super) fn freereg(&mut self, reg: usize) -> Result<(), ParseErr> {
        if reg < self.nvarstack() {
            Ok(())
        } else {
            self.fs.prop_mut().freereg -= 1;

            if self.fs.prop().freereg == reg {
                Ok(())
            } else {
                Err(ParseErr::BadUsage)
            }
        }
    }

    pub(super) fn setoneret(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        let exp = match exp.value {
            ExprValue::Call(pc) => match self.get_code(pc) {
                Some(InterCode::CALL(ra, _, _)) => Expr::nonreloc(ra),
                _ => return Err(ParseErr::BadUsage),
            },
            ExprValue::VarArg(pc) => {
                self.set_rc(pc, 2);

                Expr::reloc(pc)
            }
            _ => exp,
        };

        Ok(exp)
    }

    pub(super) fn setret(&mut self, exp: &Expr, nresults: usize) -> Result<(), ParseErr> {
        match exp.value {
            ExprValue::Call(pc) => {
                self.set_rc(pc, nresults + 1);
                Ok(())
            }
            ExprValue::VarArg(pc) => {
                let freereg = self.fs.prop().freereg;

                self.set_rc(pc, nresults + 1);
                self.set_ra(pc, freereg);

                self.reserve(1);

                Ok(())
            }
            _ => Err(ParseErr::BadUsage),
        }
    }

    pub(super) fn strk(&mut self, value: &str) -> Result<Expr, ParseErr> {
        let k = self.constant_pool.borrow_mut().adds(value);

        Ok(Expr::k(k))
    }
    pub(super) fn floatk(&mut self, value: f64) -> Result<Expr, ParseErr> {
        let k = self.constant_pool.borrow_mut().addf(value);

        Ok(Expr::k(k))
    }

    pub(super) fn intk(&mut self, value: i64) -> Result<Expr, ParseErr> {
        let k = self.constant_pool.borrow_mut().addi(value);

        Ok(Expr::k(k))
    }

    pub(super) fn nilk(&mut self) -> Result<Expr, ParseErr> {
        let k = self.constant_pool.borrow_mut().addn();

        Ok(Expr::k(k))
    }

    pub(super) fn boolk(&mut self, value: bool) -> Result<Expr, ParseErr> {
        let k = self.constant_pool.borrow_mut().addb(value);

        Ok(Expr::k(k))
    }

    pub(super) fn expk(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        let tj = exp.true_jumpto;
        let fj = exp.false_jumpto;

        match exp.value {
            ExprValue::Bool(v) => self.boolk(v),
            ExprValue::Nil => self.nilk(),
            ExprValue::Integer(v) => self.intk(v),
            ExprValue::Float(v) => self.floatk(v),
            ExprValue::String(v) => self.strk(v.as_str()),
            _ => Err(ParseErr::BadUsage),
        }
        .and_then(|exp| Ok(exp.tj(tj).fj(fj)))
    }

    pub(super) fn expkreg(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        let istok = matches!(
            exp.value,
            ExprValue::Bool(_)
                | ExprValue::Nil
                | ExprValue::Integer(_)
                | ExprValue::Float(_)
                | ExprValue::String(_)
        );

        if istok {
            self.expk(exp)
        } else {
            self.expanyreg(exp)
        }
    }

    pub(super) fn expvalue(&self, exp: &Expr) -> Result<RefValue, ParseErr> {
        if exp.hasjump() {
            return Err(ParseErr::BadUsage);
        }

        match &exp.value {
            ExprValue::Bool(v) => Ok(RefValue::from(*v)),
            ExprValue::Nil => Ok(RefValue::new()),
            ExprValue::String(v) => Ok(RefValue::from(v.as_str())),
            ExprValue::Integer(v) => Ok(RefValue::from(*v)),
            ExprValue::Float(v) => Ok(RefValue::from(*v)),
            ExprValue::Const(idx) => match self.actvar.get(*idx) {
                Some(v) => Ok(v.val.clone()),
                _ => Err(ParseErr::BadUsage),
            },
            _ => Err(ParseErr::BadUsage),
        }
    }

    fn discreg(&mut self, exp: Expr, reg: usize) -> Result<Expr, ParseErr> {
        let exp = self.discvar(exp)?;
        let tj = exp.true_jumpto;
        let fj = exp.false_jumpto;

        let exp = match exp.value {
            ExprValue::Nil => {
                self.emit_loadnil(reg, 1);
                Expr::nonreloc(reg)
            }
            ExprValue::Bool(value) => {
                self.emit_loadbool(reg, value);
                Expr::nonreloc(reg)
            }
            ExprValue::String(value) => {
                let exp = self.strk(&value)?;
                self.discreg(exp, reg)?
            }
            ExprValue::Float(value) => {
                if 0f64 <= value
                    && value as u32 <= codelimit::MAX_SBX
                    && value as u32 as f64 == value
                {
                    self.emit_loadfloat(reg, value);
                    Expr::nonreloc(reg)
                } else {
                    let exp = self.floatk(value)?;
                    self.discreg(exp, reg)?
                }
            }
            ExprValue::Integer(value) => {
                if 0 <= value && value as u32 <= codelimit::MAX_SBX {
                    self.emit_loadint(reg, value);
                    Expr::nonreloc(reg)
                } else {
                    let exp = self.intk(value)?;
                    self.discreg(exp, reg)?
                }
            }
            ExprValue::K(kidx) => {
                self.emit_loadk(reg, kidx);

                Expr::nonreloc(reg)
            }
            ExprValue::Reloc(pc) => {
                self.set_ra(pc, reg);
                Expr::nonreloc(reg)
            }
            ExprValue::Nonreloc(nreg) => {
                if reg != nreg {
                    self.emit_move(reg, nreg);
                }
                Expr::nonreloc(reg)
            }
            ExprValue::Jump(_) => exp,
            _ => return Err(ParseErr::BadUsage),
        };

        Ok(exp.tj(tj).fj(fj))
    }

    pub(super) fn discanyreg(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        match exp.value {
            ExprValue::Nonreloc(_) => Ok(exp),
            _ => {
                self.reserve(1);

                let reg = self.fs.prop().freereg - 1;
                self.discreg(exp, reg)
            }
        }
    }

    pub(super) fn discvar(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        let tj = exp.true_jumpto;
        let fj = exp.false_jumpto;

        let exp = match exp.value {
            ExprValue::Const(index) => match self.actvar.get(index) {
                Some(value) => Expr::from(&value.val),
                _ => return Err(ParseErr::BadUsage),
            },
            ExprValue::Local(_, reg) => Expr::nonreloc(reg),
            ExprValue::Upval(reg) => Expr::reloc(self.emit_getupval(reg)),
            ExprValue::IndexUp(t, index) => Expr::reloc(self.emit_gettabup(t, index)),
            ExprValue::IndexI(t, index) => {
                self.freereg(t)?;

                Expr::reloc(self.emit_geti(t, index))
            }
            ExprValue::IndexStr(t, index) => {
                self.freereg(t)?;

                Expr::reloc(self.emit_getfield(t, index))
            }
            ExprValue::Indexed(t, index) => {
                if t < index {
                    self.freereg(index)?;
                    self.freereg(t)?;
                } else {
                    self.freereg(t)?;
                    self.freereg(index)?;
                }

                Expr::reloc(self.emit_gettable(t, index))
            }
            ExprValue::VarArg(..) | ExprValue::Call(..) => self.setoneret(exp)?,
            _ => exp,
        };

        Ok(exp.tj(tj).fj(fj))
    }

    pub(super) fn register_localvar(&mut self, name: String) -> usize {
        let pc = self.pc();
        self.fs.prop().proto.prop_mut().locvars.push(LocVar {
            name,
            start_pc: pc,
            end_pc: 0,
        });

        self.fs.prop().proto.prop().locvars.len() - 1
    }

    pub(super) fn adjlocalvars(&mut self, nvars: usize) {
        let mut reglevel = self.nvarstack();

        for _ in 0..nvars {
            let index = self.fs.prop().nactvar;
            self.fs.prop_mut().nactvar += 1;

            if let Some(var) = self.get_localvar(index) {
                let pidx = self.register_localvar(var.name.clone());

                let mut var = self.get_localvar_mut(index).unwrap();
                var.r_idx = reglevel;
                var.p_idx = pidx;

                reglevel += 1;
            }
        }
    }

    pub(super) fn adjassign(&mut self, nv: usize, ne: usize, exp: Expr) -> Result<(), ParseErr> {
        let needed = nv as i32 - ne as i32;

        if exp.hasmultret() {
            let extra = if needed + 1 >= 0 { needed + 1 } else { 0 } as usize;
            self.setret(&exp, extra)?;
        } else {
            if !matches!(exp.value, ExprValue::Void) {
                self.expanyreg(exp)?;
            }

            if needed > 0 {
                let freereg = self.fs.prop().freereg;
                self.emit_loadnil(freereg, needed as usize);
            }
        }

        if needed > 0 {
            self.reserve(needed as usize);
        } else {
            self.fs.prop_mut().freereg -= needed.abs() as usize;
        }

        Ok(())
    }

    // attrib_stmt ::= [ `<` name `>` ]
    fn attrib_stmt(&mut self) -> Result<VarKind, ParseErr> {
        if check_token!(self, Token::Operator('<')) {
            let attrib = self.name()?;

            consume_token!(self, Token::Operator('>'))?;
            match attrib.as_str() {
                "const" => Ok(VarKind::CONST),
                "close" => Ok(VarKind::TOCLOSE),
                _ => Err(ParseErr::BadUsage),
            }
        } else {
            Ok(VarKind::REG)
        }
    }

    // localvar_stmt ::= name attrib_stmt { `,` name attrib_stmt } [ `=` explist_exp ]
    fn localvar_stmt(&mut self) -> Result<(), ParseErr> {
        let mut toclose = None;
        let mut nvar = 0;

        let index = loop {
            let name = self.name()?;
            let kind = self.attrib_stmt()?;

            if matches!(kind, VarKind::TOCLOSE) {
                toclose = match toclose {
                    None => Some(self.fs.prop().nactvar + nvar),
                    Some(_) => return Err(ParseErr::BadUsage),
                }
            }

            let index = self.new_localvar(Var::new(name, kind));
            nvar += 1;

            if !check_token!(self, Token::Operator(',')) {
                break index;
            }
        };

        let (nexp, exp) = if check_token!(self, Token::Operator('=')) {
            self.explist_exp()?
        } else {
            (0, Expr::void())
        };

        let var = self.get_localvar(index);
        let v = if nvar == nexp && matches!(var.and_then(|v| Some(v.kind)), Some(VarKind::CONST)) {
            self.expvalue(&exp)
        } else {
            Err(ParseErr::BadUsage)
        };

        if let Ok(v) = v {
            match self.get_localvar_mut(index) {
                Some(var) => {
                    var.kind = VarKind::CTC;
                    var.val = v;
                }
                _ => return Err(ParseErr::BadUsage),
            }

            self.adjlocalvars(nvar - 1);
            self.fs.prop_mut().nactvar += 1;
        } else {
            self.adjassign(nvar, nexp, exp)?;
            self.adjlocalvars(nvar);
        }

        if let Some(level) = toclose {
            self.fs.prop().block.prop_mut().upval = true;
            self.fs.prop().block.prop_mut().inside_tobeclosed = true;
            self.fs.prop().block.prop_mut().needclose = true;

            let vidx = self.reglevel(level);
            self.emit_tbc(vidx);
        }

        Ok(())
    }

    // localfunc_stmt ::= `function` name body_stmt
    fn localfunc_stmt(&mut self) -> Result<(), ParseErr> {
        self.skip()?;

        let index = self.fs.prop().nactvar;

        let name = self.name()?;
        self.new_localvar(Var::new(name, VarKind::REG));
        self.adjlocalvars(1);

        self.body_stmt(false)?;

        let pc = self.pc();
        if let Some(var) = self.fs.prop().proto.prop_mut().locvars.get_mut(index) {
            var.start_pc = pc;
        }

        Ok(())
    }

    // local_stmt ::= `local` [ localfunc_stmt | localvar_stmt ]
    pub(super) fn local_stmt(&mut self) -> Result<(), ParseErr> {
        self.skip()?;

        match self.lexer.token {
            Token::Function => self.localfunc_stmt(),
            _ => self.localvar_stmt(),
        }
    }

    // field_stmt ::=  `.` name
    pub(super) fn field_stmt(&mut self, exp: Expr) -> Result<Expr, ParseErr> {
        let texp = self.expanyregup(exp)?;

        self.skip()?;
        let name = self.name()?;

        self.indexed(texp, Expr::from(&name))
    }
}
