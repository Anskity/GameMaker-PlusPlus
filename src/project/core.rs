use crate::type_system::Type;
pub struct GameProject {
    custom_types: Vec<Type>,
}

impl GameProject {
    pub fn new() -> Self {
        GameProject {
            custom_types: Vec::new(),
        }
    }
    pub fn add_type(&mut self, new_type: Type) {
        self.custom_types.push(new_type);
    }
}
