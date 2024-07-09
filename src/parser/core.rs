use crate::ast::Node;
use crate::parser::stmt::parse_stmt;
use crate::parser_utils::find_pair_container;
use crate::tokenizer::Token;
use std::io::Error;

pub fn parse(tokens: &Vec<Token>) -> Result<Node, Error> {
    let mut nodes = Vec::<Box<Node>>::new();
    let mut ptr: usize = 0;

    while ptr < tokens.len() {
        if tokens[ptr] == Token::Semilicon {
            ptr += 1;
            continue;
        }

        if tokens[ptr] == Token::OpenCurly {
            let close_curly = find_pair_container(tokens, ptr)?;

            let expr = parse(&tokens[ptr + 1..close_curly].to_vec());
            if expr.is_ok() {
                nodes.push(expr?.to_box());
                ptr += close_curly - ptr + 1;

                continue;
            }
        }

        let (new_node, consumed) = parse_stmt(&tokens[ptr..])?;

        assert!(consumed > 0);

        nodes.push(new_node.to_box());
        ptr += consumed;
    }

    Ok(Node::Program(nodes))
}
