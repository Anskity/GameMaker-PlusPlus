use crate::ast::Node;
use crate::tokenizer::Token;
//
//struct NodeParseMessage(Box<Node>, usize); //(node, consumed_tokens)

//pub fn parse(tokens: Vec<Token>) -> Node {
//    let mut nodes = Vec::<Box<Node>>::new();
//    let mut ptr: usize = 0;
//
//    let boxed_tokens: Vec<Box<Token>> = tokens.iter().map(|tk| Box::from(tk.clone())).collect();
//
//    while ptr < tokens.len() {
//        let msg = parse_statement(&boxed_tokens, &ptr);
//
//        let new_node = msg.0;
//        let consumed = msg.1;
//
//        nodes.push(new_node);
//        ptr += consumed;
//    }
//
//    Node::Program(nodes)
//}
//
//fn parse_statement(tokens: &Vec<Box<Token>>, ptr: &usize) -> NodeParseMessage {
//    parse_expr(tokens, ptr)
//}

pub fn parse_expr(tokens: &Vec<Box<Token>>, ptr: &usize) -> Node {
    let mut components = Vec::<Vec<Box<Token>>>::new();
    let mut operators = Vec::<char>::new();
    let mut component_buffer = Vec::<Box<Token>>::new();
    let mut parenthesis_value: i16 = 0;
    let mut consumed = 0;

    loop {
        let tk = tokens.get(ptr + consumed).expect(
            format!(
                "TRYING TO GET TOKEN FROM {} AND TOKEN VECTOR HAS LENGTH {}",
                consumed + ptr,
                tokens.len()
            )
            .as_str(),
        );
        parenthesis_value += match **tk {
            Token::OpenParenthesis => 1,
            Token::CloseParenthesis => -1,
            _ => 0,
        };

        if parenthesis_value < 0 {
            break;
        }

        if let Token::BinaryOperator(chr) = **tk {
            if parenthesis_value == 0 {
                operators.push(chr);
                components.push(
                    component_buffer
                        .iter()
                        .map(|component_tk| component_tk.clone())
                        .collect(),
                );
                component_buffer.clear();
            } else {
                component_buffer.push(tk.clone());
            }
        } else {
            component_buffer.push(tk.clone());
        }

        consumed += 1;

        if consumed + ptr >= tokens.len() {
            components.push(
                component_buffer
                    .iter()
                    .map(|component_tk| component_tk.clone())
                    .collect(),
            );
            break;
        }
    }

    let parsed_components: Vec<Box<Node>> = components
        .iter()
        .map(|component| Box::from(parse_expr_component(component.clone())))
        .collect();

    let (multdiv_components, multdiv_operators) =
        parse_operators(&parsed_components, &operators, vec!['*', '/']);
    let (final_components, final_operators) =
        parse_operators(&multdiv_components, &multdiv_operators, vec!['+', '-']);

    assert!(final_operators.is_empty());
    assert!(final_components.len() == 1);

    *final_components[0].clone()
}

pub fn parse_expr_component(component: Vec<Box<Token>>) -> Node {
    if let Token::OpenParenthesis = **component.first().unwrap() {
        if let Token::CloseParenthesis = **component.last().unwrap() {
            let expr = component[1..component.len() - 1].to_vec();

            return parse_expr(&expr, &0);
        }
    }

    if component.len() == 1 {
        return parse_primary(component.first().unwrap().clone());
    }

    panic!("UNEXPECTED COMPONENT: {:?}", component);
}

pub fn parse_primary(tk: Box<Token>) -> Node {
    match *tk {
        Token::Identifier(id) => Node::Identifier(id),
        Token::NumericLiteral(numb) => Node::NumericLiteral(numb),
        _ => panic!("UNEXPECTED TOKEN WHILE PARSING PRIMARY: {:?}", tk),
    }
}

pub fn parse_operators(
    components: &Vec<Box<Node>>,
    operators: &Vec<char>,
    search_operators: Vec<char>,
) -> (Vec<Box<Node>>, Vec<char>) {
    assert!(components.len() == (operators.len() + 1));

    let mut new_components: Vec<Box<Node>> = Vec::new();
    let mut new_operators: Vec<char> = Vec::new();
    let mut ptr: usize = 0;

    loop {
        let operator = &operators[ptr];
        let component = &components[ptr];
        if search_operators.contains(&operator) {
            let mut left = Node::BinaryExpr(
                component.clone(),
                operator.clone(),
                components[ptr + 1].clone(),
            );

            ptr += 1;
            while ptr <= operators.len() - 1 && search_operators.contains(&operators[ptr]) {
                left = Node::BinaryExpr(
                    Box::from(left.clone()),
                    operators[ptr].clone(),
                    components[ptr + 1].clone(),
                );
                ptr += 1;
            }
            new_components.push(Box::from(left));
        } else {
            new_components.push(component.clone());
            new_operators.push(operator.clone());

            if ptr == operators.len() - 1 {}

            ptr += 1;
        }

        if ptr >= operators.len() {
            break;
        }
    }

    (new_components, new_operators)
}
