use crate::ast::Node;
use crate::code_container::CodeContainerManager;
use crate::parser_macros::*;
use crate::parser_utils::*;
use crate::tokenizer::Token;
use std::ops::Range;
use std::ops::RangeInclusive;

pub fn get_avaible_tokens_for_expr(tokens: &[Token]) -> usize {
    let mut code_manager = CodeContainerManager::new();

    let mut first_iteration = true;

    let mut length: usize = 1;
    let mut ever_contained = false;

    for (prev_tk, current_tk) in tokens.iter().zip(tokens.iter().skip(1)) {
        if first_iteration {
            code_manager.check(prev_tk);
            first_iteration = false;
        }

        if !code_manager.is_free() {
            ever_contained = true;
        }

        let is_safe = code_manager.is_safe(current_tk);

        if !is_safe {
            break;
        }

        if let (Token::Identifier(_), Token::Identifier(_)) = (prev_tk, current_tk) {
            break;
        }
        if let (Token::NumericLiteral(_), Token::NumericLiteral(_)) = (prev_tk, current_tk) {
            break;
        }

        if code_manager.is_free() && ever_contained {
            if let Token::OpenParenthesis = current_tk {
                let should_break = match *current_tk {
                    Token::BinaryOperator(_) => false,
                    _ => true,
                };
                if should_break {
                    break;
                }
            }
        }
        length += 1;
        code_manager.check(current_tk);
    }

    length
}

pub fn parse_struct_access(struct_id: Node, tokens: &Vec<Token>) -> Node {
    assert_eq!(tokens[0], Token::Dot);

    let access_node = parse_expr(&tokens[1..].to_vec());

    Node::StructAccess(struct_id.to_box(), access_node.to_box())
}
pub fn parse_array_constructor(tokens: &Vec<Token>) -> Node {
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

        let expr = parse_expr(&element);
        element_nodes.push(expr.to_box());
    }

    Node::ArrayConstructor(element_nodes)
}
pub fn parse_struct(tokens: &Vec<Token>) -> Node {
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
        let id = parse_primary(tks.first().unwrap());
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
        let expr = parse_expr(&tks[expr_range].to_vec());

        let node = Node::StructAttribute(id.to_box(), expr.to_box());
        attribute_nodes.push(node.to_box());
    }

    let struct_node = Node::Struct(attribute_nodes);

    struct_node
}
pub fn parse_array_access(arr_node: Node, tokens: &Vec<Token>) -> Node {
    assert_eq!(*tokens.first().unwrap(), Token::OpenBracket);
    assert_eq!(*tokens.last().unwrap(), Token::CloseBracket);

    static ACESSORS: [Token; 4] = [
        Token::Bar,
        Token::QuestionMark,
        Token::HashTag,
        Token::Dollar,
    ];

    let bracket_idx = find_free_token(tokens, Token::CloseBracket, 0).unwrap();

    let current_acessor = ACESSORS.iter().find(|tk| **tk == tokens[1]);

    let idx_start: usize = if current_acessor.is_none() { 1 } else { 2 };

    let array_idx = match current_acessor {
        Some(Token::HashTag) => None,
        _ => Some(parse_expr(&tokens[idx_start..bracket_idx].to_vec())),
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
            let comma_idx = find_free_token(&tokens[1..tokens.len() - 1].to_vec(), Token::Comma, 0);
            assert!(comma_idx.is_some());

            let x_pos_node = parse_expr(&tokens[idx_start..comma_idx.unwrap() + 1].to_vec());
            let y_pos_node = parse_expr(&tokens[comma_idx.unwrap() + 2..bracket_idx].to_vec());

            Node::DsGridAccess(arr_node.to_box(), x_pos_node.to_box(), y_pos_node.to_box())
        }

        Some(_) => panic!("UNEXPECTED ACESSOR: {:?}", current_acessor.unwrap()),
        None => Node::ArrayAccess(arr_node.to_box(), array_idx.unwrap().to_box()),
    };

    if bracket_idx == tokens.len() - 1 {
        access_node
    } else {
        let idx_tokens = tokens[(bracket_idx + 1)..].to_vec();

        parse_array_access(access_node, &idx_tokens)
    }
}
pub fn parse_ternary(tokens: &Vec<Token>) -> Node {
    let question_mark_idx: usize = find_free_token(tokens, Token::QuestionMark, 0)
        .expect("COULDNT FIND QUESTION MARK WHEN PARSING TERNARY OPERATOR");
    let colon_idx: usize = find_free_token(tokens, Token::Colon, 0)
        .expect("COULDNT FIND COLON WHEN PARSING TERNARY OPERATOR");

    let condition_range = 0..question_mark_idx;
    let true_range = (question_mark_idx + 1)..colon_idx;
    let false_range = (colon_idx + 1)..tokens.len();

    let condition_expr = parse_expr(&tokens[condition_range].to_vec());
    let true_expr = parse_expr(&tokens[true_range].to_vec());
    let false_expr = parse_expr(&tokens[false_range].to_vec());

    Node::Ternary(
        condition_expr.to_box(),
        true_expr.to_box(),
        false_expr.to_box(),
    )
}
pub fn parse_function_call(func_node: Node, argument_tokens: &Vec<Token>) -> Node {
    assert_eq!(*argument_tokens.first().unwrap(), Token::OpenParenthesis);
    assert_eq!(*argument_tokens.last().unwrap(), Token::CloseParenthesis);

    let mut container_manager = CodeContainerManager::new();
    //let mut arguments: Vec<Box<Node>> = Vec::new();
    let mut last_ptr: usize = 1;

    let mut argument_ranges: Vec<Range<usize>> = Vec::new();

    split_tokens!(
        argument_tokens,
        container_manager,
        Token::Comma,
        last_ptr,
        argument_ranges
    );

    let mut arguments: Vec<Box<Node>> = Vec::new();
    for range in argument_ranges {
        let expr = parse_expr(&argument_tokens[range].to_vec());
        arguments.push(expr.to_box());
    }

    Node::FunctionCall(func_node.to_box(), arguments)
}
pub fn parse_expr(tokens: &Vec<Token>) -> Node {
    if find_free_token(tokens, Token::QuestionMark, 0).is_some()
        && find_free_token(tokens, Token::Colon, 0).is_some()
    {
        return parse_ternary(tokens);
    }

    let tokens = &tokens[..get_avaible_tokens_for_expr(tokens)].to_vec();

    let (ranges, idxs) = parse_expr_components(tokens, vec!['+', '-', '*', '/']);

    let mut components: Vec<Node> = Vec::new();

    for range in ranges {
        let component = tokens[range].to_vec();
        components.push(parse_component(&component));
    }

    let mut insert_idx_buff = 0 as usize;
    for (i, idx) in idxs.iter().enumerate() {
        let operator: &Token = tokens.get(idx.clone()).unwrap();
        components.insert(i + 1 + insert_idx_buff, parse_operator(operator));
        insert_idx_buff += 1;
    }

    parse_operators_on_components(&mut components, vec!['*', '/']);
    parse_operators_on_components(&mut components, vec!['+', '-']);

    assert_eq!(components.len(), 1);

    components.remove(0)
}

pub fn parse_expr_components(
    tokens: &Vec<Token>,
    search_operators: Vec<char>,
) -> (Vec<RangeInclusive<usize>>, Vec<usize>) {
    let mut ranges = Vec::<RangeInclusive<usize>>::new();
    let mut oprts_idx = Vec::<usize>::new();
    let mut container_manager = CodeContainerManager::new();
    let mut last_ptr = 0 as isize;
    let mut just_hit_operator = false;

    for (ptr, tk) in tokens.iter().enumerate() {
        let iptr = ptr as isize;
        container_manager.check(tk);

        if let Token::BinaryOperator(chr) = *tk {
            if container_manager.is_free()
                && (search_operators.contains(&chr))
                && !just_hit_operator
            {
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

    println!("{:?}", ranges);

    (ranges, oprts_idx)
}

pub fn parse_component(component: &Vec<Token>) -> Node {
    let first = component.first();
    let last = component.last();
    if component.len() == 1 {
        return parse_primary(first.unwrap());
    }

    if let Token::BinaryOperator(chr) = first.unwrap() {
        return match *chr {
            '-' => Node::Neg(parse_expr(&component[1..].to_vec()).to_box()),
            _ => panic!("FOUND INVALID FIRST OPERATOR IN COMPONENT: {:?}", component),
        };
    }

    if component.len() == 2 {
        match (first.unwrap().to_owned(), last.unwrap().to_owned()) {
            (Token::Identifier(id), Token::SingleIncrement) => return Node::PostIncrement(id),
            (Token::Identifier(id), Token::SingleDecrement) => return Node::PostDecrement(id),
            (Token::SingleIncrement, Token::Identifier(id)) => return Node::PreIncrement(id),
            (Token::SingleDecrement, Token::Identifier(id)) => return Node::PreDecrement(id),
            _ => {}
        }
    }

    let dot_idx = find_free_token(component, Token::Dot, 0);
    if dot_idx.is_some() {
        let struct_node = parse_expr(&component[0..dot_idx.unwrap()].to_vec());
        return parse_struct_access(struct_node, &component[dot_idx.unwrap()..].to_vec());
    }

    if let Token::OpenParenthesis = first.unwrap() {
        if let Token::CloseParenthesis = last.unwrap() {
            return parse_expr(&component[1..(component.len() - 1)].to_vec());
        } else if component.contains(&Token::CloseParenthesis) {
            let end_position = find_free_token(component, Token::CloseParenthesis, 0).unwrap();

            match component.get(end_position + 1) {
                Some(Token::OpenBracket) => {
                    let bracket_range = (end_position + 1)..;

                    let array_id = parse_expr(&component[0..=end_position].to_vec());
                    return parse_array_access(array_id, &component[bracket_range].to_vec());
                }

                Some(Token::OpenParenthesis) => {
                    let parenthesis_range = (end_position + 1)..;
                    let func_id = parse_expr(&component[0..=end_position].to_vec());
                    return parse_function_call(func_id, &component[parenthesis_range].to_vec());
                }

                Some(Token::Dot) => {
                    let struct_id = parse_expr(&component[0..=end_position].to_vec());
                    return parse_struct_access(struct_id, &component[end_position + 1..].to_vec());
                }

                None => {}

                _ => panic!(
                    "Unexpecte token when parsing complex access: {:?}",
                    component[end_position + 1]
                ),
            }
        }
    }

    if let Token::Identifier(id) = first.unwrap() {
        match (component.get(1).unwrap(), last.unwrap()) {
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

    if let (Token::OpenCurly, Token::CloseCurly) = (first.unwrap(), last.unwrap()) {
        return parse_struct(component);
    }

    let close_bracket_count = amount_of_tokens(component, Token::CloseBracket);

    if close_bracket_count > 1 {
        let first_close_bracket = find_free_token(component, Token::CloseBracket, 0);
        let array_constructor_node =
            parse_array_constructor(&component[0..=first_close_bracket.unwrap()].to_vec());
        return parse_array_access(
            array_constructor_node,
            &component[first_close_bracket.unwrap() + 1..].to_vec(),
        );
    }

    if let (Token::OpenBracket, Token::CloseBracket) = (first.unwrap(), last.unwrap()) {
        return parse_array_constructor(component);
    }

    panic!("UNEXPECTED COMPONENT: {:?}", component);
}

pub fn parse_primary(tk: &Token) -> Node {
    match tk {
        Token::Identifier(id) => Node::Identifier(id.clone()),
        Token::NumericLiteral(numb) => Node::NumericLiteral(numb.clone()),
        Token::String(txt) => Node::String(txt.clone()),
        _ => panic!("ERROR PARSING PRIMARY: {:?}", tk),
    }
}

pub fn parse_operator(tk: &Token) -> Node {
    if let Token::BinaryOperator(chr) = tk {
        Node::BinaryOperator(chr.clone())
    } else {
        panic!("ERROR PARSING OPERATOR: {:?}", tk);
    }
}

pub fn parse_operators_on_components(nodes: &mut Vec<Node>, search_operators: Vec<char>) {
    let mut ptr = 0 as usize;

    while ptr < nodes.len() {
        if let Node::BinaryOperator(chr) = nodes[ptr] {
            if search_operators.contains(&chr) {
                let mut operation: Vec<Node> = nodes.drain((ptr - 1)..=(ptr + 1)).collect();

                let left = operation.remove(0);
                let right = operation.remove(1);
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
