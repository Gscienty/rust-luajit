use std::{
    cell::{Ref, RefCell},
    collections::BTreeMap,
    ops::Deref,
    rc::Rc,
};

use super::{RefValue, Value};

#[derive(Clone)]
pub struct Table {
    index_map: BTreeMap<usize, Vec<usize>>,
    pairs: Vec<(RefValue, RefValue)>,
}

impl Table {
    pub fn new() -> RefValue {
        RefValue(Rc::new(RefCell::new(Value::Table(Table {
            index_map: BTreeMap::new(),
            pairs: Vec::new(),
        }))))
    }

    fn hash(&self, key: &Value) -> usize {
        match key {
            Value::Integer(value) => *value as usize,
            Value::Number(value) => *value as usize,
            Value::Boolean(value) => *value as usize,
            Value::String(value) => {
                let mut result = 0;
                let mut offset = 0;

                for chr in value.chars() {
                    result ^= (chr as usize) << (offset << 3);
                    offset = (offset + 1) % 8;
                }

                result
            }
            _ => 0,
        }
    }

    fn pairs_off(&self, hash_key: usize, key: Ref<Value>) -> Option<usize> {
        if let Some(indexs) = self.index_map.get(&hash_key) {
            for off in indexs.iter() {
                if let Some(pair) = self.pairs.get(*off) {
                    if pair.0.get().eq(key.deref()) {
                        return Some(*off);
                    }
                } else {
                    return None;
                }
            }
        }

        None
    }

    pub fn set(&mut self, key: RefValue, value: RefValue) {
        let hash_key = self.hash(&key.get());

        if !self.index_map.contains_key(&hash_key) {
            self.index_map.insert(hash_key, Vec::new());
        }
        if let Some(off) = self.pairs_off(hash_key, key.get()) {
            if let Some(pair) = self.pairs.get_mut(off) {
                *pair = (key, value);
            } else {
                unreachable!();
            }
        } else {
            self.pairs.push((key, value));
            let off = self.pairs.len() - 1;

            if let Some(pairs) = self.index_map.get_mut(&hash_key) {
                pairs.push(off);
            }
        };
    }

    pub fn get(&self, key: Ref<Value>) -> Option<RefValue> {
        let hash_key = self.hash(key.deref());
        if let Some(off) = self.pairs_off(hash_key, key) {
            self.pairs.get(off).and_then(|v| Some(v.1.clone()))
        } else {
            None
        }
    }

    pub fn next(&self, key: Ref<Value>) -> Option<(RefValue, RefValue)> {
        if matches!(key.deref(), Value::Nil) {
            if let Some(pair) = self.pairs.get(0) {
                Some(pair.clone())
            } else {
                None
            }
        } else {
            let hash_key = self.hash(key.deref());
            if let Some(off) = self.pairs_off(hash_key, key) {
                if let Some(pair) = self.pairs.get(off + 1) {
                    Some(pair.clone())
                } else {
                    None
                }
            } else {
                None
            }
        }
    }
}

impl PartialEq for Table {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}
