use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum Type {
    String,
    Number,
    Struct(HashMap<String, Box<Type>>),
    Array(Box<Type>),
}
