use std::{cell::RefCell, rc::Rc, ops::{SubAssign, AddAssign, BitOrAssign}, borrow::Borrow};

use crate::{lex::{LexState, LuaSource, Token, VarInfo, ByteCodeInstructionLine, var_info_flags}, bytecode::{ByteOps, ByteInstruction, bytecode_range::{self, NOREG}, ByteInstructionGetter, compose_aj_instruction}};

use super::scope::{Scope, scope_flags::{self, UPVAL}};

pub(crate) struct ParseState<'s> {
    lex_state: LexState<'s>,
    free_reg: u32,
    active_var_count: u32,
    vtop: u32,
    
    scopes: Option<Rc<RefCell<Scope>>>,

    last_taget: u32,
    jpc: u32,
    pc: u32,
    varmap: [usize; 200],
}

impl<'s> ParseState<'s> {
    pub(crate) fn new(source: LuaSource<'s>) -> Self {
        ParseState {
            lex_state: LexState::new(source),
            free_reg: 0,
            active_var_count: 0,
            vtop: 0,
            scopes: None,
            last_taget: 0,
            jpc: bytecode_range::NOJMP,
            pc: 0,
            varmap: [0; 200],
        }
    }

    fn jmp_next(&self, pc: u32) -> u32 {
        if let Some(delta) = self.lex_state
            .get_ins(pc as usize)
            .and_then(|ins| {
                let delta = ins.get_j();
                if delta.ne(&bytecode_range::NOJMP) { Some(delta) } else { None }
            }) {
            pc + 1 + delta
        } else {
            bytecode_range::NOJMP
        }
    }

    fn jmp_patch_test_reg(&mut self, pc: u32, reg: u32) -> bool {
        let ilp_p = if pc.ge(&1) { pc - 1 } else { pc } as usize;
        let op = if let Some(op) = self.lex_state
            .get_ins(ilp_p)
            .and_then(|ins| ins.get_ops()) {
            op
        } else { return false; };


        let mut upgrade_next = false;

        if let Some(ins) = self.lex_state
            .get_ins_mut(if pc.ge(&1) { pc - 1 } else { pc } as usize) {

            let ops = ins.get_ops();
            if matches!(ops, Some(ByteOps::ISTC) | Some(ByteOps::ISFC)) {
                if reg.ne(&bytecode_range::NOREG) && ins.get_d().ne(&reg) {
                    ins.set_a(reg);
                } else {
                    ins.set_ops(match ops {
                        Some(ByteOps::ISTC) => ByteOps::IST,
                        Some(ByteOps::ISFC) => ByteOps::ISF,
                        _ => ByteOps::NOP,
                    });
                    ins.set_a(0);
                }
            } else if matches!(ins.get_a(), NOREG) {
                if reg.eq(&bytecode_range::NOREG) {
                    *ins= compose_aj_instruction(ByteOps::JMP, ins.get_a(), 0);
                } else {
                    ins.set_a(reg);

                    upgrade_next = true
                }
            } else {
                return false;
            }
        };

        if upgrade_next {
            if let Some(instruction) = self.lex_state
                .instructions
                .get_mut(if pc.ge(&1) { pc } else { pc + 1 } as usize)
                .and_then(|ins| Some(&mut ins.instruction)) {
                
                if reg.ge(&instruction.get_a()) {
                    instruction.set_a(reg + 1);
                }
            }
        }

        true
    }

    fn jmp_patch_instruction(&mut self, pc: u32, dest: u32) {
        let offset = dest - (pc + 1) + bytecode_range::J;
        if offset.gt(&bytecode_range::MAXD) {
            // error
            return
        }

        if let Some(jmp_instruction) = self.lex_state
            .instructions
            .get_mut(pc as usize)
            .and_then(|ins| Some(&mut ins.instruction)) {

            jmp_instruction.set_d(offset);
        }
    }

    fn jmp_append(&mut self, l1: &mut u32, l2: u32) {
        if l2.eq(&bytecode_range::NOJMP) {
            return
        } else if l1.eq(&&bytecode_range::NOJMP) {
            *l1 = l2;
        } else {
            let mut list = *l1;
            let mut next = self.jmp_next(list);
            while next.ne(&bytecode_range::NOJMP) {
                list = next;
                next = self.jmp_next(list);
            }

            self.jmp_patch_instruction(list, l2);
        }
    }

    fn jmp_to_here(&mut self, list: u32) {
        self.last_taget = self.pc;
        let mut jpc = self.jpc;
        self.jmp_append(&mut jpc, list);
        self.jpc = jpc;
    }

    fn jmp_patch(&mut self, list: u32, target: u32) {
        if target.eq(&self.pc) {
            self.jmp_to_here(list);
        } else {
            self.jmp_patch_val(list, target, bytecode_range::NOREG, target);
        }
    }

    fn jmp_patch_val(&mut self, list: u32, vtarget: u32, reg: u32, dtarget: u32) {
        let mut jmp_list = list;

        while jmp_list.ne(&bytecode_range::NOJMP) {
            let next = self.jmp_next(list);
            if self.jmp_patch_test_reg(list, reg) {
                self.jmp_patch_instruction(list, vtarget);
            } else {
                self.jmp_patch_instruction(list, dtarget);
            }
            jmp_list = next;
        }
    }

    fn emit_bytecode_instruction(&mut self, instruction: ByteInstruction) -> u32 {
        let pc = self.pc;

        self.jmp_patch_val(self.jpc, pc, bytecode_range::NOREG, pc);
        self.jpc = bytecode_range::NOJMP;

        self.lex_state.instructions.push(ByteCodeInstructionLine::new(instruction, self.lex_state.last_line));
        self.pc.add_assign(1);
        pc
    }

    fn new_goto_label(&mut self, label_name: &str, info: u8, pc: u32) -> u32 {
        let vtop = self.vtop;

        if let Some(var_info) = self.lex_state
            .var_stack
            .get_mut(vtop as usize) {

            var_info.name = String::from(label_name);
            var_info.start_pc = pc;
            var_info.slot = self.active_var_count as u8;
            var_info.info = info;
        };
        self.vtop.add_assign(1);

        vtop
    }

    fn goto_label_patch(&mut self, var_goto_idx: u32, label: &VarInfo) {
        let var_goto = if let Some(var_goto) = self.lex_state
            .var_stack
            .get_mut(var_goto_idx as usize) {
            
            var_goto
        } else {
            return;
        };

        let pc = var_goto.start_pc;
        var_goto.name.clear();

        if let Some(instruction) = self.lex_state
            .instructions
            .get_mut(pc as usize)
            .and_then(|ins| Some(&mut ins.instruction)) {
            instruction.set_a(label.slot as u32);
        }

        self.jmp_patch(pc, label.start_pc);
    }

    fn resolve_goto_label(&mut self, current_scope: &Scope, label_idx: u32) {
        let label = if let Some(label) = self.lex_state
            .var_stack
            .get(label_idx as usize) {
            label.clone()
        } else {
            return;
        };

        let mut idx = current_scope.var_start;

        while idx.lt(&label_idx) {
            idx.add_assign(1);

            let var_goto = if let Some(var_goto) = self.lex_state
                .var_stack
                .get_mut((idx - 1) as usize)
                .and_then(|var| {
                    if var.is_goto() && var.name.eq(&label.name) { Some(var) } else { None }
                }) {
                var_goto
            } else {
                continue;
            };

            if var_goto.slot.lt(&label.slot) {
                self.lex_state.line_number = if let Some(line_number) = self.lex_state
                    .instructions
                    .get(var_goto.start_pc as usize)
                    .and_then(|ins| Some(ins.line)) {

                    line_number
                } else {
                    continue;
                }
            }

            self.goto_label_patch(idx - 1, &label);
        }
    }

    fn close_goto_label(&mut self, var_goto: &VarInfo) {
        let pc = var_goto.start_pc;

        if let Some(instruction) = self.lex_state
            .instructions
            .get_mut(pc as usize)
            .and_then(|ins| Some(&mut ins.instruction)) {
            
            instruction.set_a(var_goto.slot as u32);

            if !matches!(instruction.get_ops(), Some(ByteOps::JMP)) {
                return;
            } 
            // turn into uclo
            instruction.set_ops(ByteOps::UCLO);
            instruction.set_j(bytecode_range::NOJMP);
        } else { return; };

        // jump to uclo
        let next = self.jmp_next(pc);
        if next.ne(&bytecode_range::NOJMP) {
            self.jmp_patch(next, pc);
        }
    }

    fn fixup_goto_label(&mut self, current_scope: &Scope) {
        let mut v = current_scope.var_start;
        let v_end = self.vtop;

        let mut label_collections = Vec::<(u32, VarInfo)>::new();
        let mut goto_collections = Vec::<(u32, VarInfo)>::new();

        while v.lt(&v_end) {
            v.add_assign(1);

            let var =  if let Some(var) = self.lex_state
                .var_stack
                .get_mut((v - 1) as usize)
                .and_then(|var| {
                    if !var.name.is_empty() { Some(var) } else { None }
                }) {
                var
            } else { continue; };

            if var.is_label() {
                label_collections.push((v - 1, var.clone()));
                var.name.clear();

            } else if var.is_goto() {
                goto_collections.push((v - 1, var.clone()));
                if current_scope.prev.is_some() {
                    var.slot = current_scope.active_var_count as u8;
                }
            }
        }

        label_collections.iter().for_each(|label_id| {
            let label = &label_id.1;

            let mut idx = label_id.0 + 1;
            while idx.lt(&v_end) {
                idx.add_assign(1);

                let var_goto = if let Some(var_goto) = self.lex_state
                    .var_stack
                    .get((idx - 1) as usize)
                    .and_then(|var_goto| {
                        if var_goto.name.eq(&label.name) && var_goto.is_goto() {
                            Some(var_goto)
                        } else {
                            None
                        }
                    }) {
                    var_goto.clone()
                } else { continue; };

                if (current_scope.flags & scope_flags::UPVAL).ne(&0) && var_goto.slot > label.slot {
                    self.close_goto_label(&var_goto);
                }
                self.goto_label_patch(idx - 1, label);
            }
        });

        goto_collections.iter().for_each(|goto_id| {
            let var_goto = &goto_id.1;

            if let Some(prev_scope) = current_scope.prev.clone() {
                prev_scope.as_ref().borrow_mut().flags
                    .bitor_assign(if var_goto.name.eq("break") {
                        scope_flags::BREAK
                    } else {
                        scope_flags::GOTO_LABEL
                    });
                if (current_scope.flags & scope_flags::UPVAL).ne(&0) {
                    self.close_goto_label(var_goto);
                }
            }
            // otherwise error
        });
    }

    pub(crate) fn begin_scope(&mut self, flags: u8) -> Rc<RefCell<Scope>> {
        let scope = Scope::new();

        scope.as_ref().borrow_mut().active_var_count = self.active_var_count;
        scope.as_ref().borrow_mut().var_start = self.vtop;
        scope.as_ref().borrow_mut().flags = flags;

        if let Some(prev) = self.scopes.clone() {
            scope.as_ref().borrow_mut().prev = Some(prev);
        }
        self.scopes = Some(scope.clone());

        scope
    }

    pub(crate) fn end_scope(&mut self) {
        if let Some(current_scope) = self.scopes
            .clone()
            .and_then(|current_scope| Some(current_scope.as_ref().borrow())) {

            self.scopes = if current_scope.prev.is_some() {
                current_scope.prev.clone()
            } else {
                None
            };

            self.var_remove(current_scope.active_var_count);
            self.free_reg = self.active_var_count;

            if current_scope.flags & (scope_flags::UPVAL | scope_flags::NOCLOSE) == scope_flags::UPVAL {
                _ = self.emit_bytecode_instruction(
                    compose_aj_instruction(
                        ByteOps::UCLO,
                        current_scope.borrow().active_var_count,
                        0,
                    ),
                )
            }
            if (current_scope.borrow().flags & scope_flags::BREAK).ne(&0) {
                if (current_scope.borrow().flags & scope_flags::LOOP).ne(&0) {
                    let idx = self.new_goto_label(
                        "break", 
                        var_info_flags::LABEL,
                        self.pc,
                    );
                    self.vtop = idx; // drop break label
                    self.resolve_goto_label(current_scope.borrow(), idx);
                } else {
                    self.fixup_goto_label(current_scope.borrow());
                    return;
                }
            }

            if (current_scope.borrow().flags & scope_flags::GOTO_LABEL).ne(&0) {
                // TODO
            }
        }
    }

    fn var_remove(&mut self, to_level: u32) {
        let pc = self.pc;

        while self.active_var_count.gt(&to_level) {
            self.active_var_count.sub_assign(1);

            if let Some(info) = self.get_var(self.active_var_count) {
                info.end_pc = pc;
            }
        }
    }

    fn get_var(&mut self, i: u32) -> Option<&mut VarInfo> {
        self.lex_state.var_stack.get_mut(self.varmap[i as usize] as usize)
    }

    fn parse_statement(&mut self) -> bool {
        let line = self.lex_state.line_number;

        match self.lex_state.token {
            Token::If => {

            },
            Token::While => {

            },
            Token::Do => {

            },
            Token::For => {

            },
            Token::Repeat => {

            },
            Token::Function => {

            },
            Token::Local => {

            },
            Token::Return => {

            },
            Token::Break => {

            },
            Token::Semicolon => {
                self.lex_state.next();
            },
            Token::Label => {

            },
            Token::Goto => {

            },
            _ => {

            },
        }

        false
    }

    fn parse_block(&mut self) {
        _ = self.begin_scope(0);
        self.parse_chunk();
    }

    pub(crate) fn parse_chunk(&mut self) {
        while !self.lex_state.is_end_block() {
            let last = self.parse_statement();

            _ = self.lex_state.check_and_consume(Token::Semicolon);
            self.free_reg = self.active_var_count;

            if !last {
                break;
            }
        }
    }
}