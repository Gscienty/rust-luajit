use crate::object::Prototype;

use super::{Block, FuncState, ParseErr, Parser};

pub(super) struct ParseFunc<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseFunc<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
    }

    pub(super) fn enterblock(&mut self, isloop: bool) {
        log::debug!("enter block, nactvar == {}", self.p.fs.prop().nactvar);

        let block = Block::new();

        block.prop_mut().is_loop = isloop;
        block.prop_mut().nactvar = self.p.fs.prop().nactvar;
        block.prop_mut().firstlabel = self.p.label.len();
        block.prop_mut().firstgoto = self.p.goto.len();
        block.prop_mut().inside_tobeclosed = self.p.fs.prop().block.prop().inside_tobeclosed;
        block.prop_mut().prev = Some(self.p.fs.prop().block.clone());

        self.p.fs.prop_mut().block = block;
    }

    pub(super) fn leaveblock(&mut self) -> Result<(), ParseErr> {
        log::debug!("leave block, nactvar == {}", self.p.fs.prop().nactvar);

        let block = self.p.fs.prop().block.clone();

        let level = self.p.pvar().reglevel(block.prop().nactvar);
        self.p.pvar().removevars(level);

        let hasclose = if block.prop().is_loop {
            self.p.pexp().createlabel("break", false)?
        } else {
            false
        };

        if !hasclose && block.prop().prev.is_some() && block.prop().upval {
            self.p.emiter().emit_close(level);
        }

        self.p.fs.prop_mut().freereg = level;
        while self.p.label.len() > block.prop().firstlabel {
            self.p.label.pop();
        }

        if let Some(prev_block) = &block.prop().prev {
            self.p.fs.prop_mut().block = prev_block.clone();
            self.p.pexp().movegotosout(&block)?;
        } else if block.prop().firstgoto < self.p.goto.len() {
            return Err(ParseErr::BadUsage);
        }

        Ok(())
    }

    pub(super) fn enterfunc(&mut self) {
        let fscope = FuncState::new(Prototype::new());

        self.p
            .fs
            .prop()
            .proto
            .prop_mut()
            .children_proto
            .push(fscope.prop().proto.clone());

        fscope.prop_mut().prev = Some(self.p.fs.clone());
        fscope.prop_mut().first_local = self.p.actvar.len();
        fscope.prop_mut().first_label = self.p.label.len();

        fscope.prop().block.prop_mut().nactvar = self.p.fs.prop().nactvar;
        fscope.prop().block.prop_mut().firstlabel = self.p.label.len();
        fscope.prop().block.prop_mut().firstgoto = self.p.goto.len();

        self.p.fs = fscope;
    }

    pub(super) fn leavefunc(&mut self) -> Result<(), ParseErr> {
        let nvars = self.p.pvar().instack_nvars();
        self.p.pexp().ret(nvars, 0);

        self.leaveblock()?;

        let prevfs = self.p.fs.prop().prev.clone();
        if let Some(fs) = prevfs {
            self.p.fs = fs;
        }

        Ok(())
    }
}
