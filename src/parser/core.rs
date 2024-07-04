use crate::ast::Node;
#[allow(unused_imports)]
use crate::parser::stmt::parse_stmt;
use crate::tokenizer::Token;

#[allow(unused_variables)]
pub fn parse(tokens: &Vec<Token>) -> Node {
    #[allow(unused_mut)]
    let mut nodes = Vec::<Box<Node>>::new();

    Node::Program(nodes)
}
