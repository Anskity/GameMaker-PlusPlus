use crate::ast::Node;
use crate::code_container::CodeContainerManager;
use crate::tokenizer::Token;
use std::ops::RangeInclusive;

pub fn parse(tokens: &Vec<Token>) -> Node {
    let mut nodes = Vec::<Box<Node>>::new();
    nodes.push(parse_expr(tokens).to_box());

    Node::Program(nodes)
}

fn parse_expr(tokens: &Vec<Token>) -> Node {
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

fn parse_expr_components(
    tokens: &Vec<Token>,
    search_operators: Vec<char>,
) -> (Vec<RangeInclusive<usize>>, Vec<usize>) {
    let mut ranges = Vec::<RangeInclusive<usize>>::new();
    let mut oprts_idx = Vec::<usize>::new();
    let mut container_manager = CodeContainerManager::new();
    let mut last_ptr = 0 as isize;

    for (ptr, tk) in tokens.iter().enumerate() {
        let iptr = ptr as isize;
        container_manager.check(tk);

        if let Token::BinaryOperator(chr) = *tk {
            if container_manager.is_free() && (search_operators.contains(&chr)) {
                let new_range = (last_ptr as usize)..=((iptr - 1) as usize);
                ranges.push(new_range);
                oprts_idx.push(iptr.clone() as usize);
                last_ptr = iptr + 1;
            }
        }
        if ptr == tokens.len() - 1 {
            let right: usize = ptr + 0;
            ranges.push((last_ptr as usize)..=right);
        }
    }

    (ranges, oprts_idx)
}

fn parse_component(component: &Vec<Token>) -> Node {
    let first = component.first();
    let last = component.last();
    if component.len() == 1 {
        return parse_primary(first.unwrap());
    }

    if let Token::OpenParenthesis = first.unwrap() {
        if component
            .iter()
            .filter(|tk| **tk == Token::CloseParenthesis)
            .count()
            == 1
            && *last.unwrap() == Token::CloseParenthesis
        {
            return parse_expr(&component[1..(component.len() - 1)].to_vec());
        } else if component.contains(&Token::CloseParenthesis) {
            let end_position = component
                .iter()
                .position(|tk| *tk == Token::CloseParenthesis)
                .unwrap();

            match component[end_position + 1] {
                Token::OpenBracket => {
                    let bracket_range = (end_position + 1)..;

                    let array_id = parse_expr(&component[0..=end_position].to_vec());
                    return parse_array_access(array_id, &component[bracket_range].to_vec());
                }

                Token::OpenParenthesis => {
                    let parenthesis_range = (end_position + 1)..;
                    let func_id = parse_expr(&component[0..=end_position].to_vec());
                    return parse_function_call(func_id, &component[parenthesis_range].to_vec());
                }

                _ => panic!(
                    "Unexpecte token when parsing complex access: {:?}",
                    component[end_position + 1]
                ),
            }
        }
    }

    if let Token::Identifier(id) = first.unwrap() {
        return match (component.get(1).unwrap(), last.unwrap()) {
            (Token::OpenBracket, Token::CloseBracket) => parse_array_access(
                Node::Identifier(id.clone()),
                &component[1..component.len()].to_vec(),
            ),

            (Token::OpenParenthesis, Token::CloseParenthesis) => parse_function_call(
                Node::Identifier(id.clone()),
                &component[1..component.len()].to_vec(),
            ),

            _ => panic!(
                "IDENTIIER FOLLOWED BY UNEXPECTED TOKENS WHILE PARSING COMPONENT: {:?} | {:?}",
                component.get(1).unwrap(),
                component.last().unwrap()
            ),
        };
    }

    panic!("UNEXPECTED COMPONENT: {:?}", component);
}

fn parse_primary(tk: &Token) -> Node {
    match tk {
        Token::Identifier(id) => Node::Identifier(id.clone()),
        Token::NumericLiteral(numb) => Node::NumericLiteral(numb.clone()),
        Token::String(txt) => Node::String(txt.clone()),
        _ => panic!("ERROR PARSING PRIMARY: {:?}", tk),
    }
}

fn parse_operator(tk: &Token) -> Node {
    if let Token::BinaryOperator(chr) = tk {
        Node::BinaryOperator(chr.clone())
    } else {
        panic!("ERROR PARSING OPERATOR: {:?}", tk);
    }
}

fn parse_operators_on_components(nodes: &mut Vec<Node>, search_operators: Vec<char>) {
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

fn parse_array_access(arr_node: Node, tokens: &Vec<Token>) -> Node {
    assert_eq!(*tokens.first().unwrap(), Token::OpenBracket);
    assert_eq!(*tokens.last().unwrap(), Token::CloseBracket);

    let array_idx = parse_expr(&tokens[1..(tokens.len() - 1)].to_vec());

    Node::ArrayAccess(arr_node.to_box(), array_idx.to_box())
}

pub fn parse_function_call(func_node: Node, argument_tokens: &Vec<Token>) -> Node {
    assert_eq!(*argument_tokens.first().unwrap(), Token::OpenParenthesis);
    assert_eq!(*argument_tokens.last().unwrap(), Token::CloseParenthesis);

    let mut container_manager = CodeContainerManager::new();
    let mut arguments: Vec<Box<Node>> = Vec::new();
    let mut last_ptr: usize = 1;

    for (i, tk) in argument_tokens.iter().enumerate() {
        if i == 0 || i == argument_tokens.len() - 1 {
            continue;
        }
        container_manager.check(tk);

        if *tk == Token::Comma || i == argument_tokens.len() - 2 {
            let arg_range = if i == argument_tokens.len() - 2 {
                (last_ptr)..(i + 1)
            } else {
                (last_ptr)..i
            };
            println!("RANGE: {:?}", arg_range);
            println!(
                "ARG TOKENS: {:?}",
                argument_tokens[arg_range.clone()].to_vec()
            );
            let expr = parse_expr(&argument_tokens[arg_range].to_vec());

            arguments.push(expr.to_box());

            last_ptr = i + 1;
            continue;
        }
    }

    Node::FunctionCall(func_node.to_box(), arguments)
}
