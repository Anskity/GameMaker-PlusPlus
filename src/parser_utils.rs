use crate::ast::{Node, TextData};
use crate::parser::expr::{parse_expr, parse_primary};
use crate::parser_macros::*;
use crate::verifier::types::{parse_type, DataType};
use crate::{
    code_container::CodeContainerManager,
    tokenizer::{Token, TokenStruct},
};
use std::io::Error;

pub fn amount_of_tokens(tokens: &[TokenStruct], search: &Token) -> usize {
    let mut container_manager = CodeContainerManager::new();
    let mut count: usize = 0;

    for tk in tokens {
        if !container_manager.is_safe(tk) {
            break;
        }
        container_manager.check(tk).unwrap();

        if !container_manager.is_free() {
            continue;
        }

        if *search == tk.token {
            count += 1;
        }
    }

    count
}

pub fn find_free_token(tokens: &[TokenStruct], search: &Token, offset: usize) -> Option<usize> {
    let mut idx: usize = 0;
    let mut container_manager = CodeContainerManager::new_ext(true, true, true);

    for (i, tk) in tokens.iter().enumerate() {
        if !container_manager.is_safe(tk) {
            break;
        }
        container_manager.check(tk).unwrap();

        if !container_manager.is_free() {
            continue;
        }

        if tk.token != *search {
            continue;
        }

        if idx == offset {
            return Some(i);
        }

        idx += 1;
    }

    None
}

pub fn find_pair_container(tokens: &[TokenStruct], idx: usize) -> Result<usize, Error> {
    let start_tk = &tokens[idx].token;
    let going_forward = match *start_tk {
        Token::OpenParenthesis | Token::OpenCurly | Token::OpenBracket => true,
        Token::CloseParenthesis | Token::CloseCurly | Token::CloseBracket => false,
        _ => {
            let msg = format!(": {:?}", start_tk);
            throw_err!(msg);
        }
    };
    let mut ptr = idx;
    let mut container = CodeContainerManager::new();

    loop {
        if going_forward {
            container.check(&tokens[ptr])?;
        } else {
            container.check_reverse(&tokens[ptr])?;
        }

        if container.is_free() {
            return Ok(ptr);
        }

        if (ptr == 0 && !going_forward) || (ptr == tokens.len() - 1 && going_forward) {
            break;
        }

        ptr = (ptr as isize + if going_forward { 1 } else { -1 }) as usize;
    }

    throw_err!("COULDNT FIND PAIR CONTAINER");
}
pub fn parse_function_paremeters(tokens: &[TokenStruct]) -> Result<Vec<Box<Node>>, Error> {
    let parameter_tks = split_tokens(tokens, Token::Comma)?;
    let mut parameter_nodes: Vec<Box<Node>> = Vec::new();

    for tks in parameter_tks.into_iter().filter(|tks| !tks.is_empty()) {
        if let Token::Identifier(_) = tks[0].token {
        } else {
            throw_parse_err!(tks[0].text_data, "Expected an identifier");
        }
        let identifier = parse_primary(&tks[0])?;
        let mut ptr: usize = 1;

        if ptr >= tks.len() {
            let node = Node::FunctionParemeter(identifier.to_box(), None, None);
            parameter_nodes.push(node.to_box());
            continue;
        }

        let mut data_type: Option<DataType> = None;

        if let Token::Colon = tks[1].token {
            let equals_idx = find_free_token(tks, &Token::Equals, 0).unwrap_or_else(|| tks.len());
            let type_tks = &tks[2..equals_idx];
            data_type = Some(parse_type(type_tks)?);

            ptr = equals_idx;
        }

        if ptr >= tks.len() {
            let node = Node::FunctionParemeter(identifier.to_box(), data_type, None);
            parameter_nodes.push(node.to_box());
            continue;
        }

        if let Token::Equals = tks[ptr].token {
        } else {
            throw_parse_err!(tks[0].text_data, "Expected an equals sign");
        }

        let expr_tks = &tks[ptr + 1..];
        let expr = parse_expr(expr_tks)?;

        let node = Node::FunctionParemeter(identifier.to_box(), data_type, Some(expr.to_box()));
        parameter_nodes.push(node.to_box());
    }

    Ok(parameter_nodes)
}

pub fn split_tokens(
    tokens: &[TokenStruct],
    separator: Token,
) -> Result<Vec<&[TokenStruct]>, Error> {
    if tokens.len() == 1 {
        return Ok(vec![tokens]);
    }

    let mut code_manager = CodeContainerManager::new();
    let mut last_ptr = 0usize;
    let mut sorted_tokens: Vec<&[TokenStruct]> = Vec::new();

    for (i, tk) in tokens.iter().enumerate() {
        code_manager.check(tk)?;
        if !code_manager.is_free() {
            continue;
        }
        if tk.token == separator || i == tokens.len() - 1 {
            if i == last_ptr && tk.token == separator {
                sorted_tokens.push(&[]);
                last_ptr = i + 1;
            } else {
                let tk_range = if i == tokens.len() - 1 {
                    last_ptr..i + 1
                } else {
                    last_ptr..i
                };
                sorted_tokens.push(&tokens[tk_range]);
                last_ptr = i + 1;
            }
        }
    }

    if tokens.last().is_some_and(|tk| tk.token == Token::Comma) {
        sorted_tokens.push(&[]);
    }

    if !code_manager.is_free() {
        let text_data = TextData::from_tokens(tokens);
        throw_parse_err!(text_data, "Unclosed container");
    }

    Ok(sorted_tokens)
}
