use crate::ast::Node;
use crate::parser::expr::parse_expr;
use crate::parser_macros::*;
use crate::{code_container::CodeContainerManager, tokenizer::Token};
use std::io::{Error, ErrorKind};
use std::ops::Range;

pub fn amount_of_tokens(tokens: &[Token], search: &Token) -> usize {
    let mut container_manager = CodeContainerManager::new();
    let mut count: usize = 0;

    for tk in tokens {
        container_manager.check(tk);

        if !container_manager.is_free() {
            continue;
        }

        if search == tk {
            count += 1;
        }
    }

    count
}

pub fn find_free_token(tokens: &[Token], search: &Token, offset: usize) -> Option<usize> {
    let mut idx: usize = 0;
    let mut container_manager = CodeContainerManager::new_ext(true, true, true);

    for (i, tk) in tokens.iter().enumerate() {
        container_manager.check(tk);

        if !container_manager.is_free() {
            continue;
        }

        if tk != search {
            continue;
        }

        if idx == offset {
            return Some(i);
        }

        idx += 1;
    }

    None
}

pub fn find_pair_container(tokens: &[Token], idx: usize) -> Option<usize> {
    let start_tk = &tokens[idx];
    let going_forward = match *start_tk {
        Token::OpenParenthesis | Token::OpenCurly | Token::OpenBracket => true,
        Token::CloseParenthesis | Token::CloseCurly | Token::CloseBracket => false,
        _ => panic!("Unexpected token finding a pair: {:?}", start_tk),
    };
    let mut ptr = idx;
    let mut container = CodeContainerManager::new();

    loop {
        if going_forward {
            container.check(&tokens[ptr]);
        } else {
            container.check_reverse(&tokens[ptr]);
        }

        if container.is_free() {
            return Some(ptr);
        }

        if (ptr == 0 && !going_forward) || (ptr == tokens.len() - 1 && going_forward) {
            break;
        }

        ptr = (ptr as isize + if going_forward { 1 } else { -1 }) as usize;
    }

    None
}
pub fn parse_function_paremeters(tokens: &[Token]) -> Result<Vec<Box<Node>>, Error> {
    assert_eq!(tokens[0], Token::OpenParenthesis);
    assert_eq!(*tokens.last().unwrap(), Token::CloseParenthesis);
    let mut parameter_ranges: Vec<Range<usize>> = Vec::new();
    let mut code_container = CodeContainerManager::new();
    let mut last_ptr = 1usize;
    split_tokens!(
        tokens,
        code_container,
        Token::Comma,
        last_ptr,
        parameter_ranges
    );

    let mut parameter_nodes: Vec<Result<Box<Node>, Error>> = parameter_ranges
        .into_iter()
        .map(|range| {
            let tokens = &tokens[range];
            if tokens.len() == 1 {
                match &tokens[0] {
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
                match (&tokens[0], &tokens[1]) {
                    (Token::Identifier(id), Token::Equals) => {
                        let identifier = Node::Identifier(id.clone());
                        assert_eq!(tokens[1], Token::Equals);
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
