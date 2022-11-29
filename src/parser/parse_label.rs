use crate::object::LabelDesc;

use super::{Block, ParseErr, Parser};

pub(super) struct ParseLabel<'s> {
    p: &'s mut Parser,
}

impl<'s> ParseLabel<'s> {
    pub(super) fn new(p: &'s mut Parser) -> Self {
        Self { p }
    }

    pub(super) fn findlabel(&self, name: &str) -> Option<LabelDesc> {
        let firstlabel = self.p.fs.prop().first_label;
        let len = self.p.label.len();

        for i in firstlabel..len {
            match self.p.label.get(i) {
                Some(l) if l.name.eq(name) => return Some(l.clone()),
                _ => {}
            }
        }
        None
    }

    pub(super) fn repeated(&self, name: &str) -> Result<(), ParseErr> {
        if matches!(self.findlabel(name), Some(..)) {
            Err(ParseErr::BadUsage)
        } else {
            Ok(())
        }
    }

    pub(super) fn slovegoto(&mut self, goto_pc: usize, label_pc: usize) -> Result<(), ParseErr> {
        self.p
            .pcode()
            .jump_patchlistaux(Some(goto_pc), Some(label_pc), None, Some(label_pc))
    }

    pub(super) fn slovegotos(&mut self, label: &LabelDesc) -> Result<bool, ParseErr> {
        let mut i = self.p.fs.prop().block.prop().firstgoto;

        let mut needsclose = false;
        while i < self.p.goto.len() {
            match self.p.goto.get(i) {
                Some(goto) if goto.name.eq(&label.name) => {
                    needsclose |= goto.close;
                    self.slovegoto(goto.pc, label.pc)?;
                    self.p.goto.remove(i);
                }
                _ => {
                    i += 1;
                }
            }
        }

        Ok(needsclose)
    }

    pub(super) fn createlabel(&mut self, name: &str, islast: bool) -> Result<bool, ParseErr> {
        let labelpc = self.p.emiter().mark_pc();
        let nactvar = if islast {
            self.p.fs.prop().block.prop().nactvar
        } else {
            self.p.fs.prop().nactvar
        };

        let label = LabelDesc::new(name, labelpc, nactvar);
        let result = if self.slovegotos(&label)? {
            let nactvar = self.p.pfscope().nvars_stack();
            self.p.emiter().emit_close(nactvar);

            true
        } else {
            false
        };
        self.p.label.push(label);

        Ok(result)
    }

    pub(super) fn creategoto(&mut self, name: &str, pc: usize) -> Result<(), ParseErr> {
        let nactvar = self.p.fs.prop().nactvar;
        let goto = LabelDesc::new(name, pc, nactvar);

        self.p.goto.push(goto);

        Ok(())
    }

    pub(super) fn movegotosout(&mut self, block: &Block) -> Result<(), ParseErr> {
        let block_level = self.p.pfscope().reglevel(block.prop().nactvar);

        for i in block.prop().firstgoto..self.p.goto.len() {
            let goto = if let Some(goto) = self.p.goto.get(i) {
                goto.clone()
            } else {
                continue;
            };
            let goto_level = self.p.pfscope().reglevel(goto.nactvar);

            if let Some(goto) = self.p.goto.get_mut(i) {
                if goto_level > block_level {
                    goto.close |= block.prop().upval;
                }
                goto.nactvar = block.prop().nactvar;
            }
        }

        Ok(())
    }
}
