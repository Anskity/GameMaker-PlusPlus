use crate::ast::Node;
use crate::tokenizer::Token;
use std::ops::RangeInclusive;

pub fn parse(tokens: &Vec<Token>) -> Node {
    let mut nodes = Vec::<Box<Node>>::new();
    nodes.push(parse_expr(tokens).to_box());

    Node::Program(nodes)
}

pub fn parse_expr(tokens: &Vec<Token>) -> Node {
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
    let mut parenthesis_index = 0 as i16;
    let mut last_ptr = 0 as isize;

    for (ptr, tk) in tokens.iter().enumerate() {
        let iptr = ptr as isize;
        match tk {
            Token::OpenParenthesis => parenthesis_index += 1,
            Token::CloseParenthesis => parenthesis_index -= 1,
            Token::BinaryOperator(chr) => {
                if parenthesis_index == 0 && (search_operators.contains(chr)) {
                    let new_range = (last_ptr as usize)..=((iptr - 1) as usize);
                    ranges.push(new_range);
                    oprts_idx.push(iptr.clone() as usize);
                    last_ptr = iptr + 1;
                }
            }
            _ => {}
        }
        if ptr == tokens.len() - 1 {
            let right: usize = ptr + 0;
            ranges.push((last_ptr as usize)..=right);
        }
    }

    (ranges, oprts_idx)
}

pub fn parse_component(component: &Vec<Token>) -> Node {
    let first = component.first();
    let last = component.last();
    if component.len() == 1 {
        return parse_primary(first.unwrap());
    }

    if let Token::OpenParenthesis = first.unwrap() {
        if let Token::CloseParenthesis = last.unwrap() {
            return parse_expr(&component[1..(component.len() - 1)].to_vec());
        }
    }

    panic!("UNEXPECTED COMPONENT: {:?}", component);
}

pub fn parse_primary(tk: &Token) -> Node {
    match tk {
        Token::Identifier(id) => Node::Identifier(id.clone()),
        Token::NumericLiteral(numb) => Node::NumericLiteral(numb.clone()),
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
