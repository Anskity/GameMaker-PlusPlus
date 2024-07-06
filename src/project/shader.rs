use std::path::Path;
pub struct Shader {
    name: String,
    frag: Box<Path>,
    vertex: Box<Path>,
}

impl Shader {
    pub fn new(name: String, frag: Box<Path>, vertex: Box<Path>) -> Self {
        Shader { name, frag, vertex }
    }
}
