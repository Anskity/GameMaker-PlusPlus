use crate::ast::Node;
use crate::parser::stmt::parse_stmt;
use crate::tokenizer::Token;

pub fn parse(tokens: &Vec<Token>) -> Node {
    let mut nodes = Vec::<Box<Node>>::new();
    nodes.push(parse_stmt(tokens).to_box());

    Node::Program(nodes)
}
