use super::Object;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Env(HashMap<String, Object>);

impl Env {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn get(&self, k: String) -> Option<Object> {
        Some(self.0.get(&k)?.clone())
    }

    pub fn set(&mut self, name: String, val: Object) {
        self.0.insert(name, val);
    }
}
