#[derive(Debug, Clone)]
pub enum Node {
    Program(Vec<Box<Node>>),
    BinaryExpr(Box<Node>, char, Box<Node>),
    Identifier(String),
    NumericLiteral(u32),
    String(String),
    UnparsedToken(Box<crate::tokenizer::Token>),
    BinaryOperator(char),
    ArrayAccess(Box<Node>, Box<Node>),
}

impl Node {
    pub fn to_box(self) -> Box<Self> {
        Box::new(self)
    }
}
