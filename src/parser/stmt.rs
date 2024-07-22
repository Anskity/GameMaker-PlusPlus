use std::io::Error;
use std::io::ErrorKind;

use crate::ast::DeclarationType;
use crate::ast::Node;
use crate::parser::core::parse;
use crate::parser::expr::{get_avaible_tokens_for_expr, parse_expr};
use crate::parser_macros::{assert_eq_or, assert_or, throw_err};
use crate::parser_utils::find_free_token;
use crate::parser_utils::find_pair_container;
use crate::parser_utils::parse_function_paremeters;
use crate::parser_utils::split_tokens;
use crate::tokenizer::Token;
use crate::tokenizer::TokenStruct;
use crate::verifier::types::parse_type;
use crate::verifier::types::DataType;

pub fn parse_stmt(tokens: &[TokenStruct]) -> Result<(Node, usize), Error> {
    match &tokens[0].token {
        Token::If => {
            return parse_if_statement(tokens);
        }
        Token::While => {
            return parse_while_statement(tokens);
        }
        Token::Do => {
            return parse_do_statement(tokens);
        }
        Token::Function => {
            return parse_function_declaration(tokens);
        }
        Token::For => {
            return parse_for_loop(tokens);
        }
        Token::Enum => {
            return parse_enum(tokens);
        }
        Token::With => {
            return parse_with_statement(tokens);
        }
        Token::OpenCurly => {
            let close_curly = find_pair_container(tokens, 0);

            if close_curly.is_ok() {
                let close_curly = close_curly.unwrap();
                let program = parse(&tokens[1..close_curly]);

                if program.is_ok() {
                    return Ok((program.unwrap(), close_curly + 1));
                }
            }
        }
        _ => {}
    }

    let semilicon_idx = find_free_token(tokens, &Token::Semilicon, 0);

    if semilicon_idx.is_some() {
        let semilicon_idx = semilicon_idx.unwrap();

        if let Token::Return = tokens[0].token {
            return Ok((
                parse_return_statement(&tokens[0..=semilicon_idx])?,
                semilicon_idx + 1,
            ));
        }
    }

    if let Token::Identifier(_) = &tokens[0].token {
        const MODIFIER_TKS: [Token; 6] = [
            Token::IncrementBy,
            Token::DecrementBy,
            Token::MultiplyBy,
            Token::DivideBy,
            Token::ModBy,
            Token::Equals,
        ];
        assert_or!(tokens.len() > 2);
        let semilicon_idx = semilicon_idx.unwrap();

        for modifier_tk in MODIFIER_TKS.iter() {
            if find_free_token(&tokens[0..=semilicon_idx], &modifier_tk, 0).is_some() {
                let node = parse_modifier_by(&tokens[0..=semilicon_idx], modifier_tk)?;
                return Ok((node, semilicon_idx + 1));
            }
        }
    }

    let equals_idx = find_free_token(tokens, &Token::Equals, 0);
    if [Token::Var, Token::Const, Token::Let].contains(&tokens[0].token) {
        let mut end_idx = semilicon_idx;

        if end_idx.is_none() {
            let equals_idx = equals_idx.unwrap();
            assert_eq_or!(tokens[equals_idx + 1].token, Token::Function);
            assert_eq_or!(tokens[equals_idx + 2].token, Token::OpenParenthesis);
            let close_parenthesis = find_pair_container(tokens, equals_idx + 2).unwrap();
            assert_eq_or!(tokens[close_parenthesis + 1].token, Token::OpenCurly);
            let close_curly = find_pair_container(tokens, close_parenthesis + 1).unwrap();

            end_idx = Some(close_curly);
        }
        let end_idx = end_idx.unwrap();
        return Ok((parse_variable_declaration(&tokens[..end_idx])?, end_idx + 1));
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

fn parse_variable_declaration(tokens: &[TokenStruct]) -> Result<Node, Error> {
    let variable_type = match &tokens[0].token {
        Token::Var => DeclarationType::Var,
        Token::Const => DeclarationType::Const,
        Token::Let => DeclarationType::Let,
        _ => {
            throw_err!(format!("INVALID DECLARATION TYPE: {:?}", tokens[0]));
        }
    };

    let declaration_tokens = split_tokens(&tokens[1..], Token::Comma)?;
    let mut declarations: Vec<Box<Node>> = Vec::new();

    for tks in declaration_tokens {
        let var_id = match &tks[0].token {
            Token::Identifier(id) => Node::Identifier(id.clone()),
            _ => {
                throw_err!("Identifier wasn't the first token of the declaration");
            }
        };

        let mut ptr: usize = 1;
        let mut data_type: Option<DataType> = None;

        if tks.get(1).is_some_and(|tk| tk.token == Token::Colon) {
            let equals_idx = find_free_token(tks, &Token::Equals, 0).unwrap_or_else(|| tks.len());
            let comma_idx = find_free_token(tks, &Token::Comma, 0).unwrap_or_else(|| tks.len());

            ptr = equals_idx.min(comma_idx);
            data_type = Some(parse_type(&tks[2..ptr])?);
        }

        let init_value = if tks
            .get(ptr)
            .is_some_and(|tk_struct| tk_struct.token == Token::Equals)
        {
            let expr = parse_expr(&tks[ptr + 1..])?;
            Some(expr.to_box())
        } else {
            None
        };

        declarations
            .push(Node::VariableDeclarationPart(var_id.to_box(), data_type, init_value).to_box());
    }

    let var_declaration = Node::VariableDeclaration(variable_type, declarations);
    Ok(var_declaration)
}

fn parse_if_statement(tokens: &[TokenStruct]) -> Result<(Node, usize), Error> {
    assert_eq_or!(tokens[0].token, Token::If);

    let (condition_consumed, condition_node) = if tokens[1].token == Token::OpenParenthesis {
        let close_parenthesis = find_pair_container(tokens, 1)?;
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
        .is_some_and(|tk| tk.token == Token::Else)
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

fn parse_else_statement(tokens: &[TokenStruct]) -> Result<(Option<Node>, usize), Error> {
    assert_eq_or!(tokens[0].token, Token::Else);
    let (code_node, code_consumed) = parse_stmt(&tokens[1..])?;

    let else_node = Some(Node::Else(code_node.to_box()));
    Ok((else_node, code_consumed + 1))
}

fn parse_while_statement(tokens: &[TokenStruct]) -> Result<(Node, usize), Error> {
    assert_eq_or!(tokens[0].token, Token::While);

    let (close_parenthesis, condition_node): (usize, Node) =
        if tokens[1].token == Token::OpenParenthesis {
            let idx = find_pair_container(tokens, 1)?;
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

fn parse_do_statement(tokens: &[TokenStruct]) -> Result<(Node, usize), Error> {
    assert_eq_or!(tokens[0].token, Token::Do);

    let (code_node, code_consumed) = parse_stmt(&tokens[1..])?;

    assert_eq_or!(tokens[code_consumed + 1].token, Token::Until);
    let semilicon_idx = find_free_token(&tokens[code_consumed + 2..], &Token::Semilicon, 0)
        .unwrap()
        + code_consumed
        + 2;

    let condition_node = parse_expr(&tokens[code_consumed + 2..semilicon_idx])?;

    let until_node = Node::Until(condition_node.to_box());
    let do_node = Node::Do(code_node.to_box(), until_node.to_box());

    Ok((do_node, semilicon_idx + 1))
}

fn parse_modifier_by(tokens: &[TokenStruct], modifier_tk: &Token) -> Result<Node, Error> {
    assert_eq_or!(tokens.last().unwrap().token, Token::Semilicon);
    assert_eq_or!(
        if tokens.get(1).is_some() {
            Some(&tokens[1].token)
        } else {
            None
        },
        Some(&modifier_tk)
    );
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

    let modifier_range = if let Token::Semilicon = tokens[tokens.len() - 1].token {
        modifier_idx + 1..tokens.len() - 1
    } else {
        modifier_idx + 1..tokens.len()
    };
    let modifier = parse_expr(&tokens[modifier_range])?;

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
        Token::ModBy => Ok(Node::ModBy(identifier_node.to_box(), modifier.to_box())),
        Token::DivideBy => Ok(Node::DivideBy(identifier_node.to_box(), modifier.to_box())),
        Token::Equals => Ok(Node::VariableSet(
            identifier_node.to_box(),
            modifier.to_box(),
        )),
        _ => Err(Error::new(
            ErrorKind::InvalidData,
            "INVALID OPERATOR MODIFIER",
        )),
    }
}

fn parse_function_declaration(tokens: &[TokenStruct]) -> Result<(Node, usize), Error> {
    assert_eq_or!(tokens[0].token, Token::Function);

    let identifier = match &tokens[1].token {
        Token::Identifier(id) => id.clone(),
        _ => {
            return Err(Error::new(
                ErrorKind::InvalidData,
                "TOKEN AFTER FUNCTION TOKEN ISNT AN IDENTIFIER TOKEN",
            ))
        }
    };

    assert_eq_or!(tokens[2].token, Token::OpenParenthesis);
    let close_parenthesis = find_pair_container(tokens, 2)
        .expect("NO PAIR PARENTHESIS WHEN PARSING FUNCTION DECLARATION");
    assert_eq_or!(tokens[close_parenthesis].token, Token::CloseParenthesis);
    let (is_constructor, curly_idx) = match tokens[close_parenthesis + 1].token {
        Token::OpenCurly => (false, close_parenthesis + 1),
        Token::Constructor => {
            assert_eq_or!(tokens[close_parenthesis + 2].token, Token::OpenCurly);
            (true, close_parenthesis + 2)
        }
        _ => panic!(
            "Invalid token after parenthesis while tokenizing function declaration: {:?}",
            tokens[close_parenthesis + 1]
        ),
    };
    let close_curly = find_pair_container(tokens, curly_idx)
        .expect("NO PAIR CURLY BRACE WHEN PARSING FUNCTION DECLARATION");

    let params = parse_function_paremeters(&tokens[3..close_parenthesis])?;
    let program = parse(&tokens[curly_idx + 1..close_curly])?;

    Ok((
        if is_constructor {
            Node::FunctionConstructorDeclaration(identifier, params, program.to_box())
        } else {
            Node::FunctionDeclaration(identifier, params, program.to_box())
        },
        close_curly + 1,
    ))
}

fn parse_return_statement(tokens: &[TokenStruct]) -> Result<Node, Error> {
    assert_eq_or!(tokens[0].token, Token::Return);
    assert_eq_or!(tokens.last().unwrap().token, Token::Semilicon);

    if tokens.len() > 2 {
        let value = parse_expr(&tokens[1..tokens.len() - 1])?;
        Ok(Node::Return(Some(value.to_box())))
    } else {
        Ok(Node::Return(None))
    }
}

fn parse_for_loop(tokens: &[TokenStruct]) -> Result<(Node, usize), Error> {
    assert_eq_or!(tokens[0].token, Token::For);
    assert_eq_or!(tokens[1].token, Token::OpenParenthesis);
    let close_parenthesis = find_pair_container(tokens, 1).unwrap();

    let first_semilicon = find_free_token(&tokens[2..], &Token::Semilicon, 0);
    let second_semilicon = find_free_token(&tokens[2..], &Token::Semilicon, 1);

    assert_or!(first_semilicon.is_some());
    assert_or!(second_semilicon.is_some());

    let first_semilicon = first_semilicon.unwrap() + 2;
    let second_semilicon = second_semilicon.unwrap() + 2;

    let first_attribute_tks = &tokens[2..=first_semilicon];
    let second_attribute_tks = &tokens[first_semilicon + 1..second_semilicon];
    let third_attribute_tks = &tokens[second_semilicon + 1..close_parenthesis];

    let attribute_tokens = &[
        first_attribute_tks,
        second_attribute_tks,
        third_attribute_tks,
    ];

    assert_eq_or!(attribute_tokens.len(), 3);

    let (start_statement, _) = parse_stmt(&attribute_tokens[0])?;
    let condition = parse_expr(&attribute_tokens[1])?;
    let (iteration_factor, _) = parse_stmt(&attribute_tokens[2])?;

    assert_eq_or!(tokens[close_parenthesis + 1].token, Token::OpenCurly);
    let close_curly = find_pair_container(tokens, close_parenthesis + 1)
        .expect("COULDNT FIND PAIR CURLY WHEN PARSING FOR LOOP");

    let code_node = parse(&tokens[close_parenthesis + 2..close_curly])?;

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

fn parse_enum(tokens: &[TokenStruct]) -> Result<(Node, usize), Error> {
    assert_eq_or!(tokens[0].token, Token::Enum);
    let identifier = match &tokens[1].token {
        Token::Identifier(id) => id.clone(),
        _ => {
            throw_err!("Second token in enum declaration wasn't an identifier");
        }
    };
    assert_eq_or!(tokens[2].token, Token::OpenCurly);
    let close_curly = find_pair_container(tokens, 2)?;

    let variant_tokens = split_tokens(&tokens[3..close_curly], Token::Comma)?;

    let mut variant_nodes: Vec<Box<Node>> = Vec::new();

    for tks in variant_tokens.into_iter().filter(|tks| !tks.is_empty()) {
        let id = match &tks[0].token {
            Token::Identifier(id) => id.clone(),
            _ => {
                throw_err!("First token in enum variant isn't an identifier");
            }
        };
        variant_nodes.push(Node::EnumVariant(id).to_box());
    }

    Ok((
        Node::EnumDeclaration(identifier, variant_nodes),
        close_curly + 1,
    ))
}

fn parse_with_statement(tokens: &[TokenStruct]) -> Result<(Node, usize), Error> {
    assert_eq_or!(tokens[0].token, Token::With);
    let (inst_node, inst_consumed) = if tokens[1].token == Token::OpenParenthesis {
        let close_parenthesis = find_pair_container(tokens, 1)?;
        let node = parse_expr(&tokens[2..close_parenthesis])?;
        (node, close_parenthesis)
    } else {
        let consumed = get_avaible_tokens_for_expr(&tokens[1..]);
        let node = parse_expr(&tokens[1..])?;
        (node, consumed)
    };

    let (code_node, code_consumed) = parse_stmt(&tokens[inst_consumed + 1..])?;

    let with_node = Node::With(inst_node.to_box(), code_node.to_box());
    Ok((with_node, 1 + inst_consumed + code_consumed))
}
