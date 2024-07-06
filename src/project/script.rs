use std::path::Path;

pub struct Script {
    name: String,
    code: Box<Path>,
}

impl Script {
    pub fn new(name: String, code: Box<Path>) -> Self {
        Script { name, code }
    }
}
