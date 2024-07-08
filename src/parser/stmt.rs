use std::io::Error;
use std::io::ErrorKind;
use std::ops::Range;

use crate::ast::DeclarationType;
use crate::ast::Node;
use crate::code_container::CodeContainerManager;
use crate::parser::core::parse;
use crate::parser::expr::{get_avaible_tokens_for_expr, parse_expr};
use crate::parser_macros::{assert_eq_or, assert_or, split_tokens, throw_err};
use crate::parser_utils::amount_of_tokens;
use crate::parser_utils::find_free_token;
use crate::parser_utils::find_pair_container;
use crate::parser_utils::parse_function_paremeters;
use crate::tokenizer::Token;

pub fn parse_stmt(tokens: &[Token]) -> Result<(Node, usize), Error> {
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

    if let Token::For = tokens[0] {
        return parse_for_loop(tokens);
    }

    let semilicon_idx = find_free_token(tokens, &Token::Semilicon, 0);

    if semilicon_idx.is_some() {
        let semilicon_idx = semilicon_idx.unwrap();

        if let Token::Return = tokens[0] {
            return Ok((
                parse_return_statement(&tokens[0..=semilicon_idx])?,
                semilicon_idx + 1,
            ));
        }

        assert_or!(tokens.len() > 2);

        const MODIFIER_TKS: [Token; 4] = [
            Token::IncrementBy,
            Token::DecrementBy,
            Token::MultiplyBy,
            Token::DivideBy,
        ];
        for modifier_tk in MODIFIER_TKS.iter() {
            if find_free_token(tokens, modifier_tk, 0).is_some() {
                let node = parse_modifier_by(&tokens[0..=semilicon_idx], modifier_tk)?;
                return Ok((node, semilicon_idx + 1));
            }
        }
    }

    let equals_idx = find_free_token(tokens, &Token::Equals, 0);
    if equals_idx.is_some() {
        let equals_idx = equals_idx.unwrap();
        let mut end_idx = semilicon_idx;

        if end_idx.is_none() {
            assert_eq_or!(tokens[equals_idx + 1], Token::Function);
            assert_eq_or!(tokens[equals_idx + 2], Token::OpenParenthesis);
            let close_parenthesis = find_pair_container(tokens, equals_idx + 2).unwrap();
            assert_eq_or!(tokens[close_parenthesis + 1], Token::OpenCurly);
            let close_curly = find_pair_container(tokens, close_parenthesis + 1).unwrap();

            end_idx = Some(close_curly);
        }
        let end_idx = end_idx.unwrap();
        return Ok((
            parse_variable_declaration(&tokens[..=end_idx])?,
            end_idx + 1,
        ));
    }

    if tokens[0] == Token::OpenCurly {
        let close_curly = find_pair_container(tokens, 0);

        if close_curly.is_some() {
            let close_curly = close_curly.unwrap();
            let program = parse(&tokens[1..close_curly].to_vec());

            if program.is_ok() {
                return Ok((program.unwrap(), close_curly + 1));
            }
        }
    }

    let prepared_tokens = &tokens[..semilicon_idx.unwrap_or_else(|| tokens.len())];

    let expr = parse_expr(prepared_tokens)?;

    match expr {
        Node::FunctionCall(_, _)
        | Node::PreIncrement(_)
        | Node::PreDecrement(_)
        | Node::PostIncrement(_)
        | Node::PostDecrement(_) => Ok((expr, get_avaible_tokens_for_expr(prepared_tokens) + 1)),
        _ => Err(Error::new(
            ErrorKind::InvalidData,
            "INVALID TOKENS WHEN PARSING STATEMENT",
        )),
    }
}

fn parse_variable_declaration(tokens: &[Token]) -> Result<Node, Error> {
    println!("{:?}", tokens);
    let equals_idx = find_free_token(tokens, &Token::Equals, 0);
    assert_or!(equals_idx.is_some());

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
    let identifier = parse_expr(&tokens[identifier_range])?;

    let end_idx = if let Token::Semilicon = tokens.last().unwrap() {
        tokens.len() - 1
    } else {
        tokens.len()
    };

    let init_value = parse_expr(&tokens[(equals_idx + 1)..end_idx])?;

    Ok(Node::VariableDeclaration(
        variable_type,
        identifier.to_box(),
        init_value.to_box(),
    ))
}

fn parse_if_statement(tokens: &[Token]) -> Result<(Node, usize), Error> {
    assert_eq_or!(tokens[0], Token::If);

    let (condition_consumed, condition_node) = if tokens[1] == Token::OpenParenthesis {
        let close_parenthesis = find_pair_container(tokens, 1);
        if close_parenthesis.is_none() {
            throw_err!("NO CLOSE PARENTHESIS");
        }
        let close_parenthesis = close_parenthesis.unwrap();
        let condition_node = parse_expr(&tokens[2..close_parenthesis])?;

        (close_parenthesis, condition_node)
    } else {
        let avaible_tokens = get_avaible_tokens_for_expr(&tokens[1..]);
        let node = parse_expr(&tokens[1..=avaible_tokens])?;
        (avaible_tokens, node)
    };

    let condition_close_idx = condition_consumed;
    let (code_node, code_consumed) = parse_stmt(&tokens[condition_close_idx + 1..])?;

    let (else_node, else_consumed) = if tokens
        .get(condition_close_idx + 1 + code_consumed)
        .is_some_and(|tk| *tk == Token::Else)
    {
        parse_else_statement(&tokens[condition_close_idx + 1 + code_consumed..])?
    } else {
        (None, 0)
    };

    Ok((
        Node::If(
            condition_node.to_box(),
            code_node.to_box(),
            if else_node.is_some() {
                Some(else_node.unwrap().to_box())
            } else {
                None
            },
        ),
        condition_close_idx + code_consumed + else_consumed + 1,
    ))
}

fn parse_else_statement(tokens: &[Token]) -> Result<(Option<Node>, usize), Error> {
    assert_eq_or!(tokens[0], Token::Else);
    assert_eq_or!(tokens[1], Token::OpenCurly);
    let close_idx = find_pair_container(tokens, 1).unwrap();

    let program_node = parse(&tokens[2..close_idx].to_vec())?;
    Ok((Some(Node::Else(program_node.to_box())), close_idx + 1))
}

fn parse_while_statement(tokens: &[Token]) -> Result<(Node, usize), Error> {
    assert_eq_or!(tokens[0], Token::While);

    let (close_parenthesis, condition_node): (usize, Node) = if tokens[1] == Token::OpenParenthesis
    {
        let idx = find_pair_container(tokens, 1);
        if idx.is_none() {
            throw_err!("COULDNT FIND PAIR PARENTHESIS");
        }
        let idx = idx.unwrap();
        let node = parse_expr(&tokens[2..idx])?;
        (idx, node)
    } else {
        let avaible_tokens = get_avaible_tokens_for_expr(&tokens[1..]);
        let node = parse_expr(&tokens[1..=avaible_tokens])?;
        (avaible_tokens, node)
    };

    let (code_node, code_consumed) = parse_stmt(&tokens[close_parenthesis + 1..])?;

    Ok((
        Node::While(condition_node.to_box(), code_node.to_box()),
        close_parenthesis + code_consumed + 1,
    ))
}

fn parse_do_statement(tokens: &[Token]) -> Result<(Node, usize), Error> {
    assert_eq_or!(tokens[0], Token::Do);

    let (code_node, code_consumed) = parse_stmt(&tokens[1..])?;

    assert_eq_or!(tokens[code_consumed + 1], Token::Until);
    let semilicon_idx = find_free_token(&tokens[code_consumed + 2..], &Token::Semilicon, 0)
        .unwrap()
        + code_consumed
        + 2;

    let condition_node = parse_expr(&tokens[code_consumed + 2..semilicon_idx])?;

    let until_node = Node::Until(condition_node.to_box());
    let do_node = Node::Do(code_node.to_box(), until_node.to_box());

    Ok((do_node, semilicon_idx + 1))
}

fn parse_modifier_by(tokens: &[Token], modifier_tk: &Token) -> Result<Node, Error> {
    assert_eq_or!(*tokens.last().unwrap(), Token::Semilicon);
    assert_eq_or!(amount_of_tokens(tokens, modifier_tk), 1);
    let modifier_idx: usize = find_free_token(tokens, modifier_tk, 0).unwrap();

    let identifier_node = parse_expr(&tokens[..modifier_idx])?;

    match identifier_node {
        Node::Identifier(_) | Node::ArrayAccess(_, _) | Node::StructAccess(_, _) => {}
        _ => {
            return Err(Error::new(
                ErrorKind::InvalidData,
                "INVALID IDENTIFIER NODE FOR MODIFIER BY: {:?}",
            ))
        }
    }

    let modifier = parse_expr(&tokens[modifier_idx + 1..tokens.len() - 1])?;

    match *modifier_tk {
        Token::IncrementBy => Ok(Node::IncrementBy(
            identifier_node.to_box(),
            modifier.to_box(),
        )),
        Token::DecrementBy => Ok(Node::DecrementBy(
            identifier_node.to_box(),
            modifier.to_box(),
        )),
        Token::MultiplyBy => Ok(Node::MultiplyBy(
            identifier_node.to_box(),
            modifier.to_box(),
        )),
        Token::DivideBy => Ok(Node::DivideBy(identifier_node.to_box(), modifier.to_box())),
        _ => Err(Error::new(
            ErrorKind::InvalidData,
            "INVALID OPERATOR MODIFIER",
        )),
    }
}

fn parse_function_declaration(tokens: &[Token]) -> Result<(Node, usize), Error> {
    assert_eq_or!(tokens[0], Token::Function);

    let identifier = match &tokens[1] {
        Token::Identifier(id) => id.clone(),
        _ => {
            return Err(Error::new(
                ErrorKind::InvalidData,
                "TOKEN AFTER FUNCTION TOKEN ISNT AN IDENTIFIER TOKEN",
            ))
        }
    };

    assert_eq_or!(tokens[2], Token::OpenParenthesis);
    let close_parenthesis = find_pair_container(tokens, 2)
        .expect("NO PAIR PARENTHESIS WHEN PARSING FUNCTION DECLARATION");
    assert_eq_or!(tokens[close_parenthesis], Token::CloseParenthesis);
    let (is_constructor, curly_idx) = match tokens[close_parenthesis + 1] {
        Token::OpenCurly => (false, close_parenthesis + 1),
        Token::Constructor => {
            assert_eq_or!(tokens[close_parenthesis + 2], Token::OpenCurly);
            (true, close_parenthesis + 2)
        }
        _ => panic!(
            "Invalid token after parenthesis while tokenizing function declaration: {:?}",
            tokens[close_parenthesis + 1]
        ),
    };
    let close_curly = find_pair_container(tokens, curly_idx)
        .expect("NO PAIR CURLY BRACE WHEN PARSING FUNCTION DECLARATION");

    let params = parse_function_paremeters(&tokens[2..=close_parenthesis])?;
    let program = parse(&tokens[curly_idx + 1..close_curly].to_vec())?;

    Ok((
        if is_constructor {
            Node::FunctionConstructorDeclaration(identifier, params, program.to_box())
        } else {
            Node::FunctionDeclaration(identifier, params, program.to_box())
        },
        close_curly + 1,
    ))
}

fn parse_return_statement(tokens: &[Token]) -> Result<Node, Error> {
    assert_eq_or!(tokens[0], Token::Return);
    assert_eq_or!(*tokens.last().unwrap(), Token::Semilicon);

    if tokens.len() > 2 {
        let value = parse_expr(&tokens[1..tokens.len() - 1])?;
        Ok(Node::Return(Some(value.to_box())))
    } else {
        Ok(Node::Return(None))
    }
}

fn parse_for_loop(tokens: &[Token]) -> Result<(Node, usize), Error> {
    assert_eq_or!(tokens[0], Token::For);
    assert_eq_or!(tokens[1], Token::OpenParenthesis);
    let close_parenthesis = find_pair_container(tokens, 1).unwrap();

    let mut last_ptr = 1;
    let mut code_container = CodeContainerManager::new();
    let mut for_attribute_ranges: Vec<Range<usize>> = Vec::new();

    let tokens_to_split = &tokens[1..=close_parenthesis];

    split_tokens!(
        tokens_to_split,
        code_container,
        Token::Semilicon,
        last_ptr,
        for_attribute_ranges
    );

    assert_eq_or!(for_attribute_ranges.len(), 3);

    let mut attribute_tokens = for_attribute_ranges
        .into_iter()
        .map(|range| tokens[range.start + 1..=range.end].to_vec())
        .collect::<Vec<Vec<Token>>>();

    attribute_tokens[0].push(Token::Semilicon);
    attribute_tokens[2].push(Token::Semilicon);

    let (start_statement, _) = parse_stmt(&attribute_tokens[0])?;
    let condition = parse_expr(&attribute_tokens[1])?;
    let (iteration_factor, _) = parse_stmt(&attribute_tokens[2])?;

    assert_eq_or!(tokens[close_parenthesis + 1], Token::OpenCurly);
    let close_curly = find_pair_container(tokens, close_parenthesis + 1)
        .expect("COULDNT FIND PAIR CURLY WHEN PARSING FOR LOOP");

    let code_node = parse(&tokens[close_parenthesis + 2..close_curly].to_vec())?;

    Ok((
        Node::For(
            start_statement.to_box(),
            condition.to_box(),
            iteration_factor.to_box(),
            code_node.to_box(),
        ),
        close_curly + 1,
    ))
}
