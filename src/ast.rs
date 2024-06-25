#[derive(Debug, Clone)]
pub enum Node {
    Program(Vec<Box<Node>>),
    BinaryExpr(Box<Node>, char, Box<Node>),
    Identifier(String),
    NumericLiteral(u32),
    UnparsedToken(Box<crate::tokenizer::Token>),
    BinaryOperator(char),
}

impl Node {
    pub fn to_box(self) -> Box<Self> {
        Box::new(self)
    }
}
