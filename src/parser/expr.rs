use crate::ast::Node;
use crate::ast::OperatorType;
use crate::code_container::CodeContainerManager;
use crate::parser::core::parse;
use crate::parser_macros::*;
use crate::parser_utils::*;
use crate::tokenizer::Token;
use std::io::{Error, ErrorKind};
use std::ops::Range;
use std::ops::RangeInclusive;

pub fn get_avaible_tokens_for_expr(tokens: &[Token]) -> usize {
    let mut numb = 0usize;
    let mut container = CodeContainerManager::new();

    if !container.is_safe(&tokens[0]) {
        return 0;
    }

    loop {
        let tk = &tokens[numb];
        let next = tokens.get(numb + 1);

        if *tk == Token::Function {
            numb += 1;
            let mut temp_container = CodeContainerManager::new();
            temp_container.check(&tokens[numb]);

            while !temp_container.is_free() && numb < tokens.len() {
                numb += 1;
                container.check(&tokens[numb]);
            }
            numb += 1;
            temp_container.check(&tokens[numb]);
            while !temp_container.is_free() && numb < tokens.len() {
                numb += 1;
                container.check(&tokens[numb]);
            }

            continue;
        }

        if !container.is_safe(tk) {
            break;
        }
        container.check(tk);

        if container.is_free() {
            if next == Some(&Token::OpenCurly) {
                match *tk {
                    Token::Identifier(_)
                    | Token::NumericLiteral(_)
                    | Token::CloseParenthesis
                    | Token::CloseBracket
                    | Token::CloseCurly => {
                        numb += 1;
                        break;
                    }
                    _ => {}
                }
            }
            if next == Some(&Token::Identifier("".to_string())) {
                match *tk {
                    Token::Identifier(_) | Token::NumericLiteral(_) | Token::CloseParenthesis => {
                        numb += 1;
                        break;
                    }
                    _ => {}
                }
            }
        }

        numb += 1;

        if numb >= tokens.len() {
            break;
        }
    }

    numb
}

pub fn parse_struct_access(struct_id: Node, tokens: &[Token]) -> Result<Node, Error> {
    assert_eq!(tokens[0], Token::Dot);

    let access_node = parse_expr(&tokens[1..])?;

    Ok(Node::StructAccess(struct_id.to_box(), access_node.to_box()))
}
pub fn parse_array_constructor(tokens: &[Token]) -> Result<Node, Error> {
    assert_eq!(*tokens.first().unwrap(), Token::OpenBracket);
    assert_eq!(*tokens.last().unwrap(), Token::CloseBracket);

    let mut container_manager = CodeContainerManager::new();
    let mut last_ptr: usize = 1;
    let mut token_ranges: Vec<Range<usize>> = Vec::new();

    split_tokens!(
        tokens,
        container_manager,
        Token::Comma,
        last_ptr,
        token_ranges
    );

    let mut element_tokens: Vec<Vec<Token>> = Vec::new();
    apply_range!(tokens, token_ranges, element_tokens);

    let mut element_nodes: Vec<Box<Node>> = Vec::new();
    for mut element in element_tokens {
        if let Token::Comma = element.last().unwrap() {
            element.pop();
        }

        let expr = parse_expr(&element)?;
        element_nodes.push(expr.to_box());
    }

    Ok(Node::ArrayConstructor(element_nodes))
}
pub fn parse_struct(tokens: &[Token]) -> Result<Node, Error> {
    assert_eq!(*tokens.first().unwrap(), Token::OpenCurly);
    assert_eq!(*tokens.last().unwrap(), Token::CloseCurly);

    let mut last_ptr: usize = 1;
    let mut attribute_ranges: Vec<std::ops::Range<usize>> = Vec::new();
    let mut container_manager = CodeContainerManager::new();

    split_tokens!(
        tokens,
        container_manager,
        Token::Comma,
        last_ptr,
        attribute_ranges
    );

    let mut attribute_tokens: Vec<Vec<Token>> = Vec::new();
    apply_range!(tokens, attribute_ranges, attribute_tokens);

    let mut attribute_nodes: Vec<Box<Node>> = Vec::new();
    for tks in attribute_tokens {
        let id = parse_primary(tks.first().unwrap())?;
        if tks.len() == 1 || (tks.len() == 2 && tks[1] == Token::Comma) {
            let node = Node::StructAttributePredefined(id.to_box());
            attribute_nodes.push(node.to_box());

            continue;
        }

        assert_eq!(tks[1], Token::Colon);

        let expr_range = if *tks.last().unwrap() == Token::Comma {
            2..(tks.len() - 1)
        } else {
            2..tks.len()
        };
        let expr = parse_expr(&tks[expr_range])?;

        let node = Node::StructAttribute(id.to_box(), expr.to_box());
        attribute_nodes.push(node.to_box());
    }

    let struct_node = Node::Struct(attribute_nodes);

    Ok(struct_node)
}
pub fn parse_array_access(arr_node: Node, tokens: &[Token]) -> Result<Node, Error> {
    assert_eq!(*tokens.first().unwrap(), Token::OpenBracket);
    assert_eq!(*tokens.last().unwrap(), Token::CloseBracket);

    static ACESSORS: [Token; 4] = [
        Token::Bar,
        Token::QuestionMark,
        Token::HashTag,
        Token::Dollar,
    ];

    let bracket_idx = find_free_token(tokens, &Token::CloseBracket, 0).unwrap();

    let current_acessor = ACESSORS.iter().find(|tk| **tk == tokens[1]);

    let idx_start: usize = if current_acessor.is_none() { 1 } else { 2 };

    let array_idx = match current_acessor {
        Some(Token::HashTag) => None,
        _ => Some(parse_expr(&tokens[idx_start..bracket_idx])?),
    };

    let access_node = match current_acessor {
        Some(Token::Bar) => Node::DsListAccess(arr_node.to_box(), array_idx.unwrap().to_box()),
        Some(Token::Dollar) => {
            Node::StructKeyAccess(arr_node.to_box(), array_idx.unwrap().to_box())
        }
        Some(Token::QuestionMark) => {
            Node::DsMapAccess(arr_node.to_box(), array_idx.unwrap().to_box())
        }
        Some(Token::HashTag) => {
            let comma_idx = find_free_token(&tokens[1..tokens.len() - 1], &Token::Comma, 0);
            assert!(comma_idx.is_some());

            let x_pos_node = parse_expr(&tokens[idx_start..comma_idx.unwrap() + 1])?;
            let y_pos_node = parse_expr(&tokens[comma_idx.unwrap() + 2..bracket_idx])?;

            Node::DsGridAccess(arr_node.to_box(), x_pos_node.to_box(), y_pos_node.to_box())
        }

        Some(_) => panic!("UNEXPECTED ACESSOR: {:?}", current_acessor.unwrap()),
        None => Node::ArrayAccess(arr_node.to_box(), array_idx.unwrap().to_box()),
    };

    if bracket_idx == tokens.len() - 1 {
        Ok(access_node)
    } else {
        let idx_tokens = &tokens[(bracket_idx + 1)..];

        parse_array_access(access_node, idx_tokens)
    }
}
pub fn parse_ternary(tokens: &[Token]) -> Result<Node, Error> {
    let question_mark_idx: usize = find_free_token(tokens, &Token::QuestionMark, 0)
        .expect("COULDNT FIND QUESTION MARK WHEN PARSING TERNARY OPERATOR");
    let colon_idx: usize = find_free_token(tokens, &Token::Colon, 0)
        .expect("COULDNT FIND COLON WHEN PARSING TERNARY OPERATOR");

    let condition_range = 0..question_mark_idx;
    let true_range = (question_mark_idx + 1)..colon_idx;
    let false_range = (colon_idx + 1)..tokens.len();

    let condition_expr = parse_expr(&tokens[condition_range])?;
    let true_expr = parse_expr(&tokens[true_range])?;
    let false_expr = parse_expr(&tokens[false_range])?;

    Ok(Node::Ternary(
        condition_expr.to_box(),
        true_expr.to_box(),
        false_expr.to_box(),
    ))
}
pub fn parse_function_call(func_node: Node, argument_tokens: &[Token]) -> Result<Node, Error> {
    assert_eq!(*argument_tokens.first().unwrap(), Token::OpenParenthesis);

    let close_parenthesis = find_pair_container(argument_tokens, 0)?;

    let mut container_manager = CodeContainerManager::new();
    //let mut arguments: Vec<Box<Node>> = Vec::new();
    let mut last_ptr: usize = 1;

    let mut argument_ranges: Vec<Range<usize>> = Vec::new();
    let fixed_argument_tokens = &argument_tokens[0..=close_parenthesis];

    split_tokens!(
        fixed_argument_tokens,
        container_manager,
        Token::Comma,
        last_ptr,
        argument_ranges
    );

    let mut arguments: Vec<Box<Node>> = Vec::new();
    for range in argument_ranges {
        let expr = parse_expr(&argument_tokens[range]);
        arguments.push(expr?.to_box());
    }

    if fixed_argument_tokens.len() == argument_tokens.len() {
        Ok(Node::FunctionCall(func_node.to_box(), arguments))
    } else {
        let call_node = Node::FunctionCall(func_node.to_box(), arguments);
        let extra_range = (fixed_argument_tokens.len())..;
        parse_function_call(call_node, &argument_tokens[extra_range])
    }
}
pub fn parse_expr(tokens: &[Token]) -> Result<Node, Error> {
    if find_free_token(tokens, &Token::QuestionMark, 0).is_some()
        && find_free_token(tokens, &Token::Colon, 0).is_some()
    {
        return parse_ternary(tokens);
    }

    let tokens = &tokens[..get_avaible_tokens_for_expr(tokens)];

    let (ranges, idxs) = parse_expr_components(tokens);

    let mut components: Vec<Node> = Vec::new();

    for range in ranges {
        let component = &tokens[range];
        components.push(parse_component(&component)?);
    }

    let mut insert_idx_buff = 0 as usize;
    for (i, idx) in idxs.iter().enumerate() {
        let operator: &Token = tokens.get(idx.clone()).unwrap();
        components.insert(i + 1 + insert_idx_buff, parse_operator(operator)?);
        insert_idx_buff += 1;
    }

    parse_operators_on_components(&mut components, &[OperatorType::Mul, OperatorType::Div]);
    parse_operators_on_components(&mut components, &[OperatorType::Add, OperatorType::Sub]);
    parse_operators_on_components(
        &mut components,
        &[
            OperatorType::BitwiseShiftRight,
            OperatorType::BitwiseShiftLeft,
        ],
    );
    parse_operators_on_components(
        &mut components,
        &[
            OperatorType::Lt,
            OperatorType::Gt,
            OperatorType::LtE,
            OperatorType::GtE,
        ],
    );
    parse_operators_on_components(
        &mut components,
        &[OperatorType::Equals, OperatorType::NotEquals],
    );
    parse_operators_on_components(
        &mut components,
        &[
            OperatorType::BitwiseAnd,
            OperatorType::BitwiseXor,
            OperatorType::BitwiseOr,
        ],
    );
    parse_operators_on_components(
        &mut components,
        &[OperatorType::And, OperatorType::Or, OperatorType::Xor],
    );

    assert_eq!(components.len(), 1);

    Ok(components.remove(0))
}

pub fn parse_expr_components(tokens: &[Token]) -> (Vec<RangeInclusive<usize>>, Vec<usize>) {
    let mut ranges = Vec::<RangeInclusive<usize>>::new();
    let mut oprts_idx = Vec::<usize>::new();
    let mut container_manager = CodeContainerManager::new();
    let mut last_ptr = 0 as isize;
    let mut just_hit_operator = false;

    for (ptr, tk) in tokens.iter().enumerate() {
        let iptr = ptr as isize;
        container_manager.check(tk);

        if let Token::BinaryOperator(_) = tk {
            if container_manager.is_free() && !just_hit_operator && ptr != 0 {
                let new_range = (last_ptr as usize)..=((iptr - 1) as usize);
                ranges.push(new_range);
                oprts_idx.push(iptr.clone() as usize);
                last_ptr = iptr + 1;
                just_hit_operator = true;
            } else {
                just_hit_operator = false;
            }
        } else {
            just_hit_operator = false;
        }
        if ptr == tokens.len() - 1 {
            let right: usize = ptr + 0;
            ranges.push((last_ptr as usize)..=right);
        }
    }

    (ranges, oprts_idx)
}

pub fn parse_component(component: &[Token]) -> Result<Node, Error> {
    let first = component.first().unwrap();
    let last = component.last().unwrap();
    if component.len() == 1 {
        return parse_primary(first);
    }

    if let Token::Exclamation = first {
        return Ok(Node::Not(parse_expr(&component[1..])?.to_box()));
    }
    if let Token::Tilde = first {
        return Ok(Node::BitwiseNot(parse_expr(&component[1..])?.to_box()));
    }

    if let Token::Function = first {
        assert_eq!(component[1], Token::OpenParenthesis);
        return parse_anonymous_function(component);
    }

    if let Token::BinaryOperator(operator_type) = first {
        return match *operator_type {
            OperatorType::Sub => Ok(Node::Neg(parse_expr(&component[1..])?.to_box())),
            _ => {
                throw_err!(format!(
                    "FIRST OPERATOR IN COMPONENT ISN'T INVALID: {:?}",
                    operator_type
                ));
            }
        };
    }

    if *first == Token::IncrementBy || *first == Token::SingleDecrement {
        let expr = parse_component(&component[1..])?;

        return match first {
            Token::SingleIncrement => Ok(Node::PreIncrement(expr.to_box())),
            Token::SingleDecrement => Ok(Node::PreDecrement(expr.to_box())),
            _ => panic!("????????????"),
        };
    }
    if *last == Token::SingleIncrement || *last == Token::SingleDecrement {
        let expr = parse_component(&component[0..component.len() - 1])?;

        return match last {
            Token::SingleIncrement => Ok(Node::PostIncrement(expr.to_box())),
            Token::SingleDecrement => Ok(Node::PostDecrement(expr.to_box())),
            _ => panic!("????????????"),
        };
    }

    let dot_idx = find_free_token(component, &Token::Dot, 0);
    if dot_idx.is_some() {
        let struct_node = parse_expr(&component[0..dot_idx.unwrap()])?;
        return parse_struct_access(struct_node, &component[dot_idx.unwrap()..]);
    }

    if let Token::OpenParenthesis = first {
        if let Token::CloseParenthesis = last {
            return parse_expr(&component[1..(component.len() - 1)]);
        } else if component.contains(&Token::CloseParenthesis) {
            let end_position = find_free_token(component, &Token::CloseParenthesis, 0).unwrap();

            match component.get(end_position + 1) {
                Some(Token::OpenBracket) => {
                    let bracket_range = (end_position + 1)..;

                    let array_id = parse_expr(&component[0..=end_position])?;
                    return parse_array_access(array_id, &component[bracket_range]);
                }

                Some(Token::OpenParenthesis) => {
                    let parenthesis_range = (end_position + 1)..;
                    let func_id = parse_expr(&component[0..=end_position])?;
                    return parse_function_call(func_id, &component[parenthesis_range]);
                }

                Some(Token::Dot) => {
                    let struct_id = parse_expr(&component[0..=end_position])?;
                    return parse_struct_access(struct_id, &component[end_position + 1..]);
                }

                None => {}

                _ => {
                    return Err(Error::new(
                        ErrorKind::InvalidData,
                        "Invalid input when parsing complex acess",
                    ))
                }
            }
        }
    }

    if let Token::Identifier(id) = first {
        match (component.get(1).unwrap(), last) {
            (Token::OpenBracket, Token::CloseBracket) => {
                return parse_array_access(
                    Node::Identifier(id.clone()),
                    &component[1..component.len()].to_vec(),
                )
            }

            (Token::OpenParenthesis, Token::CloseParenthesis) => {
                return parse_function_call(
                    Node::Identifier(id.clone()),
                    &component[1..component.len()].to_vec(),
                )
            }
            _ => {}
        };
    }

    if let (Token::OpenCurly, Token::CloseCurly) = (first, last) {
        return parse_struct(component);
    }

    let close_bracket_count = amount_of_tokens(component, &Token::CloseBracket);

    if close_bracket_count > 1 {
        let first_close_bracket = find_free_token(component, &Token::CloseBracket, 0);
        let array_constructor_node =
            parse_array_constructor(&component[0..=first_close_bracket.unwrap()])?;
        return parse_array_access(
            array_constructor_node,
            &component[first_close_bracket.unwrap() + 1..],
        );
    }

    if let (Token::OpenBracket, Token::CloseBracket) = (first, last) {
        return parse_array_constructor(component);
    }

    panic!("UNEXPECTED COMPONENT: {:?}", component);
}

pub fn parse_primary(tk: &Token) -> Result<Node, Error> {
    match tk {
        Token::Identifier(id) => Ok(Node::Identifier(id.clone())),
        Token::NumericLiteral(numb) => Ok(Node::NumericLiteral(numb.clone())),
        Token::String(txt) => Ok(Node::String(txt.clone())),
        _ => {
            throw_err!(format!("Invalid token while parsing primary: {:?}", tk));
        }
    }
}

pub fn parse_operator(tk: &Token) -> Result<Node, Error> {
    if let Token::BinaryOperator(chr) = tk {
        Ok(Node::BinaryOperator(chr.clone()))
    } else {
        Err(Error::new(
            ErrorKind::InvalidInput,
            "Invalid token when parsing operator",
        ))
    }
}

pub fn parse_operators_on_components(nodes: &mut Vec<Node>, search_operators: &[OperatorType]) {
    let mut ptr = 0 as usize;

    while ptr < nodes.len() {
        if let Node::BinaryOperator(chr) = nodes[ptr].clone() {
            if search_operators.contains(&chr) {
                let left = nodes.remove(ptr - 1);
                nodes.remove(ptr - 1);
                let right = nodes.remove(ptr - 1);
                let binary_expr = Node::BinaryExpr(left.to_box(), chr.clone(), right.to_box());
                nodes.insert(ptr - 1, binary_expr);
            } else {
                ptr += 1;
            }
        } else {
            ptr += 1;
        }
    }
}

fn parse_anonymous_function(tokens: &[Token]) -> Result<Node, Error> {
    assert_eq!(tokens[0], Token::Function);
    assert_eq!(tokens[1], Token::OpenParenthesis);
    let close_parenthesis = find_pair_container(tokens, 1)?;

    let paramethers = parse_function_paremeters(&tokens[1..=close_parenthesis])?;

    assert_eq!(tokens[close_parenthesis + 1], Token::OpenCurly);
    let close_curly = find_pair_container(tokens, close_parenthesis + 1)?;

    let code_node = parse(&tokens[close_parenthesis + 2..close_curly].to_vec())?;

    Ok(Node::AnonymousFunctionDeclaration(
        paramethers,
        code_node.to_box(),
    ))
}
