pub(crate) enum ConstantValue {
    Float(f64),
    Integer(i64),
    String(String),
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
}
