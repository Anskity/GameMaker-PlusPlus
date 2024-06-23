#[derive(Debug, Clone)]
pub enum Node {
    Program(Vec<Box<Node>>),
    BinaryExpr(Box<Node>, char, Box<Node>),
    Identifier(String),
    NumericLiteral(u32),
    UnparsedToken(Box<crate::tokenizer::Token>),
}

impl Node {
    pub fn display(&self, indent: usize) {
        use Node::{BinaryExpr, Identifier, NumericLiteral, Program, UnparsedToken};
        let space = " ".repeat(4 * indent);
        let space_plus = " ".repeat(4 * (indent + 1));
        match self {
            Program(nodes) => {
                println!("{space}Program");
                for node in nodes {
                    node.display(indent + 1);
                }
            }
            BinaryExpr(left, operator, right) => {
                println!("{space}BinaryExpr");
                left.display(indent + 1);
                println!("{space_plus}Operator: {}", operator);
                right.display(indent + 1);
            }
            Identifier(id) => println!("{space}Identifier: {}", id),
            NumericLiteral(numb) => println!("{space}NumericLiteral: {}", numb),
            UnparsedToken(_) => panic!("UNEXPECTED NODE WHILE DISPLAYING"),
        }
    }
}
