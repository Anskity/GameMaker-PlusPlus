use crate::ast::Node;
use crate::parser::core::parse;
use crate::parser::expr::parse_expr;
use crate::parser_utils::{find_free_token, find_pair_container};
use crate::tokenizer::Token;

pub fn parse_stmt(tokens: &Vec<Token>) -> (Node, usize) {
    let first = tokens.first().unwrap();

    let semilicon_idx = find_free_token(tokens, Token::Semilicon, 0);

    if let Token::Identifier(_) = first {
        if find_free_token(tokens, Token::Equals, 0).is_some() {
            return (
                parse_variable_declaration(
                    &tokens[0..semilicon_idx.expect("Couldnt find semilicon")].to_vec(),
                ),
                semilicon_idx.unwrap(),
            );
        }

        let function_call = parse_expr(&tokens[0..semilicon_idx.unwrap()].to_vec());
        if let Node::FunctionCall(_, _) = function_call {
            return (function_call, semilicon_idx.unwrap());
        }
    }

    let basic_statement_node = match first {
        Token::If => Some(parse_basic_statements(
            tokens,
            true,
            |condition, code, leftovers| {
                let else_node = if leftovers.len() == 0 {
                    None
                } else {
                    Some(
                        parse_basic_statements(leftovers, false, |_, code, _| {
                            Node::Else(code.to_box())
                        })
                        .to_box(),
                    )
                };
                Node::If(condition.unwrap().to_box(), code.to_box(), else_node)
            },
        )),

        Token::While => Some(parse_basic_statements(
            tokens,
            true,
            |condition, code, _| Node::While(condition.unwrap().to_box(), code.to_box()),
        )),

        Token::With => Some(parse_basic_statements(
            tokens,
            true,
            |condition, code, _| Node::With(condition.unwrap().to_box(), code.to_box()),
        )),

        _ => None,
    };

    if basic_statement_node.is_some() {
        return (basic_statement_node.unwrap(), 0);
    }

    panic!("Unexpected statement: {:?}", tokens);
}

fn parse_variable_declaration(tokens: &Vec<Token>) -> Node {
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

fn parse_basic_statements(
    tokens: &Vec<Token>,
    needs_expr: bool,
    maker: fn(Option<Node>, Node, &Vec<Token>) -> Node,
) -> Node {
    if needs_expr {
        assert_eq!(tokens[1], Token::OpenParenthesis);
    }

    let parenthesis_idx = if needs_expr {
        find_free_token(tokens, Token::CloseParenthesis, 0)
            .expect("COULDNT FIND FREE CLOSE PARENTHESIS IN IF STATEMENT")
    } else {
        0
    };

    let condition_node = if needs_expr {
        Some(parse_expr(&tokens[1..=parenthesis_idx].to_vec()))
    } else {
        None
    };

    let (program, leftovers) = if let Token::OpenCurly = &tokens[parenthesis_idx + 1] {
        let close_curly_idx: usize = find_pair_container(tokens, parenthesis_idx + 1)
            .expect("No closing curly brace when parsing an if statement");

        let program_tokens = &tokens[parenthesis_idx + 2..close_curly_idx].to_vec();
        (
            parse(program_tokens),
            &tokens[close_curly_idx + 1..].to_vec(),
        )
    } else {
        let semilicon_idx = find_free_token(tokens, Token::Semilicon, 0)
            .expect("COULD FIND SEMILICON IN IF STATEMENT WITHOUT CURLY BRACES");

        let program_tokens = &tokens[parenthesis_idx + 1..=semilicon_idx].to_vec();
        (parse(program_tokens), &tokens[semilicon_idx + 1..].to_vec())
    };

    maker(condition_node, program, leftovers)
}
