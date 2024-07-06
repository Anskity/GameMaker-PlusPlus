use std::path::Path;

pub struct Object {
    name: String,
    create: Option<Box<Path>>,
    step: Option<Box<Path>>,
    draw: Option<Box<Path>>,
    clean_up: Option<Box<Path>>,
}

impl Object {
    pub fn new(
        name: String,
        create: Option<Box<Path>>,
        step: Option<Box<Path>>,
        draw: Option<Box<Path>>,
        clean_up: Option<Box<Path>>,
    ) -> Self {
        Object {
            name,
            create,
            step,
            draw,
            clean_up,
        }
    }
}
