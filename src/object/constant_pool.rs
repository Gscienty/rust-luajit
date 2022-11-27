pub(crate) enum ConstantValue {
    Float(f64),
    Integer(i64),
    String(String),
    Bool(bool),
    Nil,
}

pub(crate) struct ConstantPool {
    constants: Vec<ConstantValue>,
}

impl ConstantPool {
    pub(crate) fn new() -> Self {
        Self {
            constants: Vec::new(),
        }
    }

    pub(crate) fn addf(&mut self, v: f64) -> usize {
        self.constants.push(ConstantValue::Float(v));

        self.constants.len() - 1
    }

    pub(crate) fn addi(&mut self, v: i64) -> usize {
        self.constants.push(ConstantValue::Integer(v));

        self.constants.len() - 1
    }

    pub(crate) fn adds(&mut self, v: &str) -> usize {
        self.constants.push(ConstantValue::String(v.to_string()));

        self.constants.len() - 1
    }

    pub(crate) fn addb(&mut self, v: bool) -> usize {
        self.constants.push(ConstantValue::Bool(v));

        self.constants.len() - 1
    }

    pub(crate) fn addn(&mut self) -> usize {
        self.constants.push(ConstantValue::Nil);

        self.constants.len() - 1
    }

    pub(crate) fn get(&self, k: usize) -> Option<&ConstantValue> {
        self.constants.get(k)
    }
}
