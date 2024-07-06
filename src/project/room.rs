use std::path::Path;
pub struct Room {
    name: String,
    json: Box<Path>,
}

impl Room {
    pub fn new(name: String, json: Box<Path>) -> Self {
        Room { name, json }
    }
}
