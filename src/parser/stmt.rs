use crate::ast::Node;
use crate::parser::expr::parse_expr;
use crate::parser_utils::find_free_token;
use crate::tokenizer::Token;

pub fn parse_stmt(tokens: &Vec<Token>) -> Node {
    let first = tokens.first().unwrap();

    let semilicon_idx = find_free_token(tokens, Token::Semilicon, 0);

    if let Token::Identifier(_) = first {
        if find_free_token(tokens, Token::Equals, 0).is_some() {
            return parse_variable_declaration(&tokens[0..semilicon_idx.unwrap()].to_vec());
        }

        let function_call = parse_expr(&tokens[0..semilicon_idx.unwrap()].to_vec());
        if let Node::FunctionCall(_, _) = function_call {
            return function_call;
        }
    }

    panic!("Unexpected statement: {:?}", tokens);
}

pub fn parse_variable_declaration(tokens: &Vec<Token>) -> Node {
    let equals_idx = find_free_token(tokens, Token::Equals, 0)
        .expect("Variable declaration without equals token");

    let value_id = parse_expr(&tokens[0..equals_idx].to_vec());

    match value_id {
        Node::Identifier(_)
        | Node::ArrayAccess(_, _)
        | Node::DsMapAccess(_, _)
        | Node::DsListAccess(_, _)
        | Node::DsGridAccess(_, _, _)
        | Node::StructKeyAccess(_, _)
        | Node::StructAccess(_, _) => {}
        _ => panic!("Invalid node in variable declaration: {:?}", value_id),
    }

    let expr = parse_expr(&tokens[equals_idx + 1..].to_vec());

    Node::VariableDeclaration(value_id.to_box(), expr.to_box())
}
