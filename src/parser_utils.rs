use crate::ast::Node;
use crate::parser::expr::parse_expr;
use crate::parser_macros::*;
use crate::{
    code_container::CodeContainerManager,
    tokenizer::{Token, TokenStruct},
};
use std::io::{Error, ErrorKind};

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
    assert_eq_or!(tokens[0].token, Token::OpenParenthesis);
    assert_eq_or!(tokens.last().unwrap().token, Token::CloseParenthesis);
    let parameter_tks = split_tokens(&tokens[1..tokens.len() - 1], Token::Comma);

    let mut parameter_nodes: Vec<Result<Box<Node>, Error>> = parameter_tks?
        .into_iter()
        .map(|tokens| {
            if tokens.len() == 1 {
                match &tokens[0].token {
                    Token::Identifier(id) => {
                        return Ok(Node::FunctionParemeter(
                            Node::Identifier(id.clone()).to_box(),
                            None,
                        )
                        .to_box());
                    }
                    _ => Err(Error::new(ErrorKind::InvalidData, "INVALID PAREMETER")),
                }
            } else if tokens.len() > 2 {
                match (&tokens[0].token, &tokens[1].token) {
                    (Token::Identifier(id), Token::Equals) => {
                        let identifier = Node::Identifier(id.clone());
                        assert_eq_or!(tokens[1].token, Token::Equals);
                        let default_value: Result<Node, Error> = parse_expr(&tokens[2..]);

                        if default_value.is_err() {
                            return Err(Error::new(
                                ErrorKind::InvalidData,
                                "INVALID DEFAULT VALUE FOR FUNCTION PAREMETER",
                            ));
                        }
                        let default_value = default_value.unwrap();

                        return Ok(Node::FunctionParemeter(
                            identifier.to_box(),
                            Some(default_value.to_box()),
                        )
                        .to_box());
                    }
                    _ => Err(Error::new(ErrorKind::InvalidData, "INVALID PAREMETER")),
                }
            } else {
                Err(Error::new(ErrorKind::InvalidData, "INVALID PAREMETER"))
            }
        })
        .collect();

    let err_idx = parameter_nodes.iter().position(|result| result.is_err());
    if err_idx.is_some() {
        let err_idx = err_idx.unwrap();
        let unsafe_result = parameter_nodes.remove(err_idx);
        Err(Error::new(
            ErrorKind::InvalidData,
            unsafe_result.unwrap_err(),
        ))
    } else {
        let safe_nodes: Vec<Box<Node>> = parameter_nodes
            .into_iter()
            .map(|result| result.unwrap())
            .collect();
        Ok(safe_nodes)
    }
}

pub fn split_tokens(
    tokens: &[TokenStruct],
    separator: Token,
) -> Result<Vec<&[TokenStruct]>, Error> {
    let mut code_manager = CodeContainerManager::new();
    let mut last_ptr = 0usize;
    let mut sorted_tokens: Vec<&[TokenStruct]> = Vec::new();

    for (i, tk) in tokens.iter().enumerate() {
        code_manager.check(tk)?;
        if !code_manager.is_free() {
            continue;
        }
        if tk.token == separator || i == tokens.len() - 1 {
            sorted_tokens.push(&tokens[last_ptr..i]);
            last_ptr = i + 1;
        }
    }

    Ok(sorted_tokens)
}
