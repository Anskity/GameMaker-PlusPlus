use crate::ast::Node;
#[allow(unused_imports)]
use crate::parser::stmt::parse_stmt;
use crate::tokenizer::Token;

pub fn parse(tokens: &Vec<Token>) -> Node {
    let mut nodes = Vec::<Box<Node>>::new();
    let mut ptr: usize = 0;

    while ptr < tokens.len() {
        let (new_node, consumed) = parse_stmt(tokens);

        assert!(consumed > 0);

        nodes.push(new_node.to_box());
        ptr += consumed;
    }

    Node::Program(nodes)
}
