use crate::ast::DeclarationType;
use crate::ast::Node;
use crate::parser::core::parse;
use crate::parser::expr::{get_avaible_tokens_for_expr, parse_expr};
use crate::parser_utils::amount_of_tokens;
use crate::parser_utils::find_free_token;
use crate::parser_utils::find_pair_container;
use crate::parser_utils::parse_function_paremeters;
use crate::tokenizer::Token;

pub fn parse_stmt(tokens: &[Token]) -> (Node, usize) {
    if let Token::If = tokens[0] {
        return parse_if_statement(tokens);
    }

    if let Token::While = tokens[0] {
        return parse_while_statement(tokens);
    }

    if let Token::Do = tokens[0] {
        return parse_do_statement(tokens);
    }

    if let Token::Function = tokens[0] {
        return parse_function_declaration(tokens);
    }

    let semilicon_idx = find_free_token(tokens, &Token::Semilicon, 0);

    let equals_idx = find_free_token(tokens, &Token::Equals, 0);
    if equals_idx.is_some() {
        let equals_idx = equals_idx.unwrap();
        let end_idx = semilicon_idx.unwrap_or_else(|| {
            assert_eq!(tokens[equals_idx + 1], Token::Function);
            assert_eq!(tokens[equals_idx + 2], Token::OpenParenthesis);
            let close_parenthesis = find_pair_container(tokens, equals_idx + 2).unwrap();
            assert_eq!(tokens[close_parenthesis + 1], Token::OpenCurly);
            let close_curly = find_pair_container(tokens, close_parenthesis + 1).unwrap();

            close_curly
        });
        return (parse_variable_declaration(&tokens[..=end_idx]), end_idx + 1);
    }

    if semilicon_idx.is_some() {
        let semilicon_idx = semilicon_idx.unwrap();

        if let Token::Return = tokens[0] {
            return (
                parse_return_statement(&tokens[0..=semilicon_idx]),
                semilicon_idx + 1,
            );
        }

        assert!(tokens.len() > 2);

        const MODIFIER_TKS: [Token; 4] = [
            Token::IncrementBy,
            Token::DecrementBy,
            Token::MultiplyBy,
            Token::DivideBy,
        ];
        for modifier_tk in MODIFIER_TKS.iter() {
            if find_free_token(tokens, modifier_tk, 0).is_some() {
                let node = parse_modifier_by(&tokens[0..=semilicon_idx], modifier_tk);
                return (node, semilicon_idx + 1);
            }
        }
    }

    let prepared_tokens = &tokens[..semilicon_idx.unwrap_or_else(|| tokens.len())];

    (
        parse_expr(prepared_tokens),
        get_avaible_tokens_for_expr(prepared_tokens) + 1,
    )
}

fn parse_variable_declaration(tokens: &[Token]) -> Node {
    let equals_idx = find_free_token(tokens, &Token::Equals, 0);
    assert!(equals_idx.is_some());

    let equals_idx = equals_idx.unwrap();

    let variable_type = match &tokens[0] {
        Token::Var => Some(DeclarationType::Var),
        Token::Const => Some(DeclarationType::Const),
        Token::Let => Some(DeclarationType::Let),
        Token::Identifier(_) => None,
        _ => panic!("INVALID FIRST TOKEN: {:?}", tokens[0]),
    };

    let identifier_range_start: usize = if variable_type.is_some() { 1 } else { 0 };
    let identifier_range = identifier_range_start..equals_idx;
    let identifier = parse_expr(&tokens[identifier_range]);

    let end_idx = if let Token::Semilicon = tokens.last().unwrap() {
        tokens.len() - 1
    } else {
        tokens.len()
    };

    let init_value = parse_expr(&tokens[(equals_idx + 1)..end_idx]);
    println!("{:?}", init_value);

    Node::VariableDeclaration(variable_type, identifier.to_box(), init_value.to_box())
}

fn parse_if_statement(tokens: &[Token]) -> (Node, usize) {
    assert_eq!(tokens[0], Token::If);
    assert_eq!(tokens[1], Token::OpenParenthesis);
    let condition_close_idx = find_pair_container(tokens, 1).unwrap();

    let condition_node = parse_expr(&tokens[2..condition_close_idx]);

    assert_eq!(tokens[condition_close_idx + 1], Token::OpenCurly);
    let curly_close_idx = find_pair_container(tokens, condition_close_idx + 1).unwrap();

    let code_node = parse(&tokens[condition_close_idx + 2..curly_close_idx].to_vec());

    let (else_node, else_consumed) = if tokens
        .get(curly_close_idx + 1)
        .is_some_and(|tk| *tk == Token::Else)
    {
        parse_else_statement(&tokens[curly_close_idx + 1..])
    } else {
        (None, 0)
    };

    (
        Node::If(
            condition_node.to_box(),
            code_node.to_box(),
            if else_node.is_some() {
                Some(else_node.unwrap().to_box())
            } else {
                None
            },
        ),
        curly_close_idx + 1 + else_consumed,
    )
}

fn parse_else_statement(tokens: &[Token]) -> (Option<Node>, usize) {
    assert_eq!(tokens[0], Token::Else);
    assert_eq!(tokens[1], Token::OpenCurly);
    let close_idx = find_pair_container(tokens, 1).unwrap();

    let program_node = parse(&tokens[2..close_idx].to_vec());
    (Some(Node::Else(program_node.to_box())), close_idx + 1)
}

fn parse_while_statement(tokens: &[Token]) -> (Node, usize) {
    assert_eq!(tokens[0], Token::While);
    assert_eq!(tokens[1], Token::OpenParenthesis);
    let close_parenthesis = find_pair_container(tokens, 1).unwrap();

    let condition_node = parse_expr(&tokens[2..close_parenthesis]);

    assert_eq!(tokens[close_parenthesis + 1], Token::OpenCurly);
    let close_curly = find_pair_container(tokens, close_parenthesis + 1).unwrap();

    let code_node = parse(&tokens[close_parenthesis + 2..close_curly].to_vec());

    (
        Node::While(condition_node.to_box(), code_node.to_box()),
        close_curly + 1,
    )
}

fn parse_do_statement(tokens: &[Token]) -> (Node, usize) {
    assert_eq!(tokens[0], Token::Do);
    assert_eq!(tokens[1], Token::OpenCurly);
    let close_curly = find_pair_container(tokens, 1).unwrap();

    let code_node = parse(&tokens[2..close_curly].to_vec());

    assert_eq!(tokens[close_curly + 1], Token::Until);
    let semilicon_idx = find_free_token(&tokens[close_curly + 2..], &Token::Semilicon, 0).unwrap()
        + close_curly
        + 2;

    let condition_node = parse_expr(&tokens[close_curly + 2..semilicon_idx]);

    let until_node = Node::Until(condition_node.to_box());
    let do_node = Node::Do(code_node.to_box(), until_node.to_box());

    (do_node, semilicon_idx + 1)
}

fn parse_modifier_by(tokens: &[Token], modifier_tk: &Token) -> Node {
    assert_eq!(*tokens.last().unwrap(), Token::Semilicon);
    assert_eq!(amount_of_tokens(tokens, modifier_tk), 1);
    let modifier_idx: usize = find_free_token(tokens, modifier_tk, 0).unwrap();

    let identifier_node = parse_expr(&tokens[..modifier_idx]);

    match identifier_node {
        Node::Identifier(_) | Node::ArrayAccess(_, _) | Node::StructAccess(_, _) => {}
        _ => panic!(
            "INVALID IDENTIFIER NODE FOR MODIFIER BY: {:?}",
            identifier_node
        ),
    }

    let modifier = parse_expr(&tokens[modifier_idx + 1..tokens.len() - 1]);

    match *modifier_tk {
        Token::IncrementBy => Node::IncrementBy(identifier_node.to_box(), modifier.to_box()),
        Token::DecrementBy => Node::DecrementBy(identifier_node.to_box(), modifier.to_box()),
        Token::MultiplyBy => Node::MultiplyBy(identifier_node.to_box(), modifier.to_box()),
        Token::DivideBy => Node::DivideBy(identifier_node.to_box(), modifier.to_box()),
        _ => panic!("INVALID OPERATOR MODIFIER: {:?}", modifier_tk),
    }
}

fn parse_function_declaration(tokens: &[Token]) -> (Node, usize) {
    assert_eq!(tokens[0], Token::Function);

    let identifier = match &tokens[1] {
        Token::Identifier(id) => id.clone(),
        _ => panic!("TOKEN AFTER FUNCTION TOKEN ISNT AN IDENTIFIER TOKEN"),
    };

    assert_eq!(tokens[2], Token::OpenParenthesis);
    let close_parenthesis = find_pair_container(tokens, 2)
        .expect("NO PAIR PARENTHESIS WHEN PARSING FUNCTION DECLARATION");
    assert_eq!(tokens[close_parenthesis], Token::CloseParenthesis);
    let (is_constructor, curly_idx) = match tokens[close_parenthesis + 1] {
        Token::OpenCurly => (false, close_parenthesis + 1),
        Token::Constructor => {
            assert_eq!(tokens[close_parenthesis + 2], Token::OpenCurly);
            (true, close_parenthesis + 2)
        }
        _ => panic!(
            "Invalid token after parenthesis while tokenizing function declaration: {:?}",
            tokens[close_parenthesis + 1]
        ),
    };
    let close_curly = find_pair_container(tokens, curly_idx)
        .expect("NO PAIR CURLY BRACE WHEN PARSING FUNCTION DECLARATION");

    let params = parse_function_paremeters(&tokens[2..=close_parenthesis]);
    let program = parse(&tokens[curly_idx + 1..close_curly].to_vec());

    (
        if is_constructor {
            Node::FunctionConstructorDeclaration(identifier, params, program.to_box())
        } else {
            Node::FunctionDeclaration(identifier, params, program.to_box())
        },
        close_curly + 1,
    )
}

fn parse_return_statement(tokens: &[Token]) -> Node {
    assert_eq!(tokens[0], Token::Return);
    assert_eq!(*tokens.last().unwrap(), Token::Semilicon);

    if tokens.len() > 2 {
        let value = parse_expr(&tokens[1..tokens.len() - 1]);
        Node::Return(Some(value.to_box()))
    } else {
        Node::Return(None)
    }
}
