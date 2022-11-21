use std::{
    cell::{Ref, RefCell},
    collections::BTreeMap,
    ops::Deref,
    rc::Rc,
};

use super::{RefValue, Value};

#[derive(Clone)]
struct TableNode {
    key: RefValue,
    value: RefValue,
}

#[derive(Clone)]
pub struct Table {
    hash_map: BTreeMap<u64, Vec<TableNode>>,
    array: Vec<RefValue>,
}

impl Table {
    pub fn new() -> RefValue {
        RefValue(Rc::new(RefCell::new(Value::Table(Table {
            hash_map: BTreeMap::new(),
            array: Vec::new(),
        }))))
    }

    fn hash(&self, key: &Value) -> u64 {
        match key {
            Value::Integer(value) => *value as u64,
            Value::Number(value) => *value as u64,
            Value::Boolean(value) => *value as u64,
            Value::String(value) => {
                let mut result = 0;
                let mut offset = 0;

                for chr in value.chars() {
                    result ^= (chr as u64) << (offset << 3);
                    offset = (offset + 1) % 8;
                }

                result
            }
            _ => 0,
        }
    }

    fn get_offset(&self, hash_key: u64, key: Ref<Value>) -> usize {
        if let Some(pairs) = self.hash_map.get(&hash_key) {
            let mut offset = 0usize;
            let len = pairs.len();

            loop {
                if offset == len {
                    break usize::MAX;
                }

                if let Some(pair) = pairs.get(offset) {
                    if pair.key.0.borrow().eq(key.deref()) {
                        break offset;
                    }
                } else {
                    break usize::MAX;
                }

                offset += 1;
            }
        } else {
            usize::MAX
        }
    }

    pub fn set(&mut self, key: RefValue, value: RefValue) {
        if let Value::Integer(index) = key.0.borrow().deref() {
            if let Some(array_value) = self.array.get_mut(*index as usize) {
                *array_value = value;

                return;
            }
        }

        let hash_key = self.hash(key.get().deref());

        if !self.hash_map.contains_key(&hash_key) {
            self.hash_map.insert(hash_key, Vec::new());
        }

        let offset = self.get_offset(hash_key, key.get());

        if let Some(pairs) = self.hash_map.get_mut(&hash_key) {
            if offset == usize::MAX {
                pairs.push(TableNode { key, value })
            } else if let Some(pair) = pairs.get_mut(offset) {
                pair.value = value;
            }
        }
    }

    pub fn get(&self, key: Ref<Value>) -> Option<RefValue> {
        let hash_key = self.hash(key.deref());
        let offset = self.get_offset(hash_key, key);

        self.hash_map
            .get(&hash_key)
            .and_then(|pairs| pairs.get(offset))
            .and_then(|pair| Some(pair.value.clone()))
    }
}

impl PartialEq for Table {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}
