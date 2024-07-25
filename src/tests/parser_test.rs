use crate::ast::{DeclarationType, Node, OperatorType};
use crate::parser::core::parse;
use crate::tokenizer::tokenize;

#[test]
fn test_general() {
    let tokens = tokenize(
        "
        //Create
        let hsp = 0;
        let vsp = 0;
        const spd = 1;
        
        //Step
        var move_x = -keyboard_check(vk_left)+keyboard_check(vk_right),
            move_y = keyboard_check(vk_down)-keyboard_check(vk_up);
        var dir = point_direction(0, 0, move_x, move_y);

        if (move_x != 0 || abs(move_y)) {
            hsp = lengthdir_x(spd, dir);
            vsp = lengthdir_y(spd, dir);
        }
        else {
            hsp = 0;
            vsp = 0;
        }

        if (place_meeting(x+hsp, y, obj_solid)) {
            while (!place_meeting(x+sign(hsp), y, obj_solid)) x += sign(hsp);
            hsp = 0;
        }
        x += hsp;

        if place_meeting(x, y+vsp, obj_solid) {
            while !place_meeting(x, y+sign(vsp), obj_solid) {
                y += sign(vsp);
            }
            vsp = 0;
        }
        y += vsp;
    ",
    );
    let gotten_node = parse(&tokens).unwrap();

    let expected_node: Node = Node::Program(vec![
        Node::VariableDeclaration(
            DeclarationType::Let,
            vec![Node::VariableDeclarationPart(
                Node::Identifier("hsp".to_string()).to_box(),
                None,
                Some(Node::NumericLiteral(0).to_box()),
            )
            .to_box()],
        )
        .to_box(),
        Node::VariableDeclaration(
            DeclarationType::Let,
            vec![Node::VariableDeclarationPart(
                Node::Identifier("vsp".to_string()).to_box(),
                None,
                Some(Node::NumericLiteral(0).to_box()),
            )
            .to_box()],
        )
        .to_box(),
        Node::VariableDeclaration(
            DeclarationType::Const,
            vec![Node::VariableDeclarationPart(
                Node::Identifier("spd".to_string()).to_box(),
                None,
                Some(Node::NumericLiteral(1).to_box()),
            )
            .to_box()],
        )
        .to_box(),
        Node::VariableDeclaration(
            DeclarationType::Var,
            vec![
                Node::VariableDeclarationPart(
                    Node::Identifier("move_x".to_string()).to_box(),
                    None,
                    Some(
                        Node::BinaryExpr(
                            Node::Neg(
                                Node::FunctionCall(
                                    Node::Identifier("keyboard_check".to_string()).to_box(),
                                    vec![Node::Identifier("vk_left".to_string()).to_box()],
                                )
                                .to_box(),
                            )
                            .to_box(),
                            OperatorType::Add,
                            Node::FunctionCall(
                                Node::Identifier("keyboard_check".to_string()).to_box(),
                                vec![Node::Identifier("vk_right".to_string()).to_box()],
                            )
                            .to_box(),
                        )
                        .to_box(),
                    ),
                )
                .to_box(),
                Node::VariableDeclarationPart(
                    Node::Identifier("move_y".to_string()).to_box(),
                    None,
                    Some(
                        Node::BinaryExpr(
                            Node::FunctionCall(
                                Node::Identifier("keyboard_check".to_string()).to_box(),
                                vec![Node::Identifier("vk_down".to_string()).to_box()],
                            )
                            .to_box(),
                            OperatorType::Sub,
                            Node::FunctionCall(
                                Node::Identifier("keyboard_check".to_string()).to_box(),
                                vec![Node::Identifier("vk_up".to_string()).to_box()],
                            )
                            .to_box(),
                        )
                        .to_box(),
                    ),
                )
                .to_box(),
            ],
        )
        .to_box(),
        Node::VariableDeclaration(
            DeclarationType::Var,
            vec![Node::VariableDeclarationPart(
                Node::Identifier("dir".to_string()).to_box(),
                None,
                Some(
                    Node::FunctionCall(
                        Node::Identifier("point_direction".to_string()).to_box(),
                        vec![
                            Node::NumericLiteral(0).to_box(),
                            Node::NumericLiteral(0).to_box(),
                            Node::Identifier("move_x".to_string()).to_box(),
                            Node::Identifier("move_y".to_string()).to_box(),
                        ],
                    )
                    .to_box(),
                ),
            )
            .to_box()],
        )
        .to_box(),
        Node::If(
            Node::BinaryExpr(
                Node::BinaryExpr(
                    Node::Identifier("move_x".to_string()).to_box(),
                    OperatorType::NotEquals,
                    Node::NumericLiteral(0).to_box(),
                )
                .to_box(),
                OperatorType::Or,
                Node::FunctionCall(
                    Node::Identifier("abs".to_string()).to_box(),
                    vec![Node::Identifier("move_y".to_string()).to_box()],
                )
                .to_box()
                .to_box(),
            )
            .to_box(),
            Node::Program(vec![
                Node::VariableSet(
                    Node::Identifier("hsp".to_string()).to_box(),
                    Node::FunctionCall(
                        Node::Identifier("lengthdir_x".to_string()).to_box(),
                        vec![
                            Node::Identifier("spd".to_string()).to_box(),
                            Node::Identifier("dir".to_string()).to_box(),
                        ],
                    )
                    .to_box(),
                )
                .to_box(),
                Node::VariableSet(
                    Node::Identifier("vsp".to_string()).to_box(),
                    Node::FunctionCall(
                        Node::Identifier("lengthdir_y".to_string()).to_box(),
                        vec![
                            Node::Identifier("spd".to_string()).to_box(),
                            Node::Identifier("dir".to_string()).to_box(),
                        ],
                    )
                    .to_box(),
                )
                .to_box(),
            ])
            .to_box(),
            Some(
                Node::Else(
                    Node::Program(vec![
                        Node::VariableSet(
                            Node::Identifier("hsp".to_string()).to_box(),
                            Node::NumericLiteral(0).to_box(),
                        )
                        .to_box(),
                        Node::VariableSet(
                            Node::Identifier("vsp".to_string()).to_box(),
                            Node::NumericLiteral(0).to_box(),
                        )
                        .to_box(),
                    ])
                    .to_box(),
                )
                .to_box(),
            ),
        )
        .to_box(),
        Node::If(
            Node::FunctionCall(
                Node::Identifier("place_meeting".to_string()).to_box(),
                vec![
                    Node::BinaryExpr(
                        Node::Identifier("x".to_string()).to_box(),
                        OperatorType::Add,
                        Node::Identifier("hsp".to_string()).to_box(),
                    )
                    .to_box(),
                    Node::Identifier("y".to_string()).to_box(),
                    Node::Identifier("obj_solid".to_string()).to_box(),
                ],
            )
            .to_box(),
            Node::Program(vec![
                Node::While(
                    Node::Not(
                        Node::FunctionCall(
                            Node::Identifier("place_meeting".to_string()).to_box(),
                            vec![
                                Node::BinaryExpr(
                                    Node::Identifier("x".to_string()).to_box(),
                                    OperatorType::Add,
                                    Node::FunctionCall(
                                        Node::Identifier("sign".to_string()).to_box(),
                                        vec![Node::Identifier("hsp".to_string()).to_box()],
                                    )
                                    .to_box(),
                                )
                                .to_box(),
                                Node::Identifier("y".to_string()).to_box(),
                                Node::Identifier("obj_solid".to_string()).to_box(),
                            ],
                        )
                        .to_box(),
                    )
                    .to_box(),
                    Node::Program(vec![Node::IncrementBy(
                        Node::Identifier("x".to_string()).to_box(),
                        Node::FunctionCall(
                            Node::Identifier("sign".to_string()).to_box(),
                            vec![Node::Identifier("hsp".to_string()).to_box()],
                        )
                        .to_box(),
                    )
                    .to_box()])
                    .to_box(),
                )
                .to_box(),
                Node::VariableSet(
                    Node::Identifier("hsp".to_string()).to_box(),
                    Node::NumericLiteral(0).to_box(),
                )
                .to_box(),
            ])
            .to_box(),
            None,
        )
        .to_box(),
        Node::IncrementBy(
            Node::Identifier("x".to_string()).to_box(),
            Node::Identifier("hsp".to_string()).to_box(),
        )
        .to_box(),
        Node::If(
            Node::FunctionCall(
                Node::Identifier("place_meeting".to_string()).to_box(),
                vec![
                    Node::Identifier("x".to_string()).to_box(),
                    Node::BinaryExpr(
                        Node::Identifier("y".to_string()).to_box(),
                        OperatorType::Add,
                        Node::Identifier("vsp".to_string()).to_box(),
                    )
                    .to_box(),
                    Node::Identifier("obj_solid".to_string()).to_box(),
                ],
            )
            .to_box(),
            Node::Program(vec![
                Node::While(
                    Node::Not(
                        Node::FunctionCall(
                            Node::Identifier("place_meeting".to_string()).to_box(),
                            vec![
                                Node::Identifier("x".to_string()).to_box(),
                                Node::BinaryExpr(
                                    Node::Identifier("y".to_string()).to_box(),
                                    OperatorType::Add,
                                    Node::FunctionCall(
                                        Node::Identifier("sign".to_string()).to_box(),
                                        vec![Node::Identifier("vsp".to_string()).to_box()],
                                    )
                                    .to_box(),
                                )
                                .to_box(),
                                Node::Identifier("obj_solid".to_string()).to_box(),
                            ],
                        )
                        .to_box(),
                    )
                    .to_box(),
                    Node::Program(vec![Node::IncrementBy(
                        Node::Identifier("y".to_string()).to_box(),
                        Node::FunctionCall(
                            Node::Identifier("sign".to_string()).to_box(),
                            vec![Node::Identifier("vsp".to_string()).to_box()],
                        )
                        .to_box(),
                    )
                    .to_box()])
                    .to_box(),
                )
                .to_box(),
                Node::VariableSet(
                    Node::Identifier("vsp".to_string()).to_box(),
                    Node::NumericLiteral(0).to_box(),
                )
                .to_box(),
            ])
            .to_box(),
            None,
        )
        .to_box(),
        Node::IncrementBy(
            Node::Identifier("y".to_string()).to_box(),
            Node::Identifier("vsp".to_string()).to_box(),
        )
        .to_box(),
    ]);

    match gotten_node {
        Node::Program(ref code) => {
            for (i, _) in code.iter().enumerate() {
                let expected: &Box<Node> = match expected_node {
                    Node::Program(ref expected_code) => &expected_code[i],
                    _ => panic!("????????????"),
                };
                let gotten: &Box<Node> = match gotten_node {
                    Node::Program(ref gotten_code) => &gotten_code[i],
                    _ => panic!("GOT A NODE THAT ISNT A PROGRAM"),
                };
                assert_eq!(&gotten, &expected);
            }
        }
        _ => panic!("ERROR: CODE DIDNT BECOME A PROGRAME NODE"),
    }
}
