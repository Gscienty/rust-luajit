use super::RefValue;

pub(crate) struct VMContext {
    pub(crate) pc: usize,
    pub(crate) reg: Vec<RefValue>,
}

impl VMContext {
    pub(crate) fn new() -> Self {
        Self {
            pc: 0,
            reg: Vec::new(),
        }
    }

    pub(crate) fn get(&self, regidx: usize) -> RefValue {
        if let Some(stored) = self.reg.get(regidx) {
            stored.clone()
        } else {
            RefValue::new()
        }
    }

    pub(crate) fn set(&mut self, regidx: usize, value: RefValue) {
        if let Some(stored) = self.reg.get_mut(regidx) {
            *stored = value;
        } else if self.reg.len() == 0 {
            self.reg.push(value);
        } else {
            while regidx < self.reg.len() - 1 {
                self.reg.push(RefValue::new());
            }
            self.reg.push(value);
        }
    }
}
