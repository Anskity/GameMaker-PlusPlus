use crate::tokenizer::{tokenize, Token};
#[test]
fn test_keywords() {
    let tokens = tokenize("let if const idk enum function construcimnottor elsefailed");

    assert_eq!(tokens[0].token, Token::Let);
    assert_eq!(tokens[1].token, Token::If);
    assert_eq!(tokens[2].token, Token::Const);
    match &tokens[3].token {
        Token::Identifier(id) => assert_eq!(id, "idk"),
        _ => panic!("TOKEN TEST FAILED FAILED"),
    }
    assert_eq!(tokens[4].token, Token::Enum);
    assert_eq!(tokens[5].token, Token::Function);
    match &tokens[6].token {
        Token::Identifier(id) => assert_eq!(id, "construcimnottor"),
        _ => panic!("TOKEN TEST FAILED"),
    }
    match &tokens[7].token {
        Token::Identifier(id) => assert_eq!(id, "elsefailed"),
        _ => panic!("TOKEN TEST FAILED"),
    }
}
