use super::{Block, FuncState, Parser};

pub(super) struct ParseFunc<'s, 't> {
    fs: &'s mut FuncState,
    p: &'t mut Parser,
}

impl<'s, 't> ParseFunc<'s, 't> {
    pub(super) fn new(fs: &'s mut FuncState, p: &'t mut Parser) -> Self {
        Self { fs, p }
    }

    pub(super) fn enter_bl(&mut self, is_loop: bool) -> Block {
        let bl = Block::new();

        bl.prop_mut().is_loop = is_loop;
        bl.prop_mut().nactvar = self.fs.prop().nactvar;
        bl.prop_mut().firstlabel = self.p.labelcnt();
        bl.prop_mut().firstgoto = self.p.gotocnt();
        bl.prop_mut().upval = false;
        if let Some(prev_bl) = &self.fs.prop().block {
            bl.prop_mut().inside_tobeclosed = prev_bl.prop().inside_tobeclosed;
            bl.prop_mut().prev = Some(prev_bl.clone());
        }

        self.fs.prop_mut().block = Some(bl.clone());

        bl.clone()
    }

    pub(super) fn leave_bl(&mut self) {
        // TODO

        self.fs.prop_mut().block = self
            .fs
            .prop()
            .block
            .as_ref()
            .and_then(|bl| bl.prop().prev.clone());
    }
}
