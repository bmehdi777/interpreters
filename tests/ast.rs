use r_interpreter::lexer::token::{Token, TokenType};
use r_interpreter::parser::ast::*;
use r_interpreter::parser::parser::Parser;

#[test]
fn test_string() {
    let program: Program = Program {
        statements: vec![Statement::Let(LetStatement {
            token: Token {
                token_type: TokenType::LET,
                literal: "let".to_owned(),
            },
            name: Some(Identifier {
                token: Token {
                    token_type: TokenType::IDENT,
                    literal: "myVar".to_owned(),
                },
                value: "myVar".to_owned(),
            }),
            value: Some(Expression::Identifier(Identifier {
                token: Token {
                    token_type: TokenType::IDENT,
                    literal: "anotherVar".to_owned(),
                },
                value: "anotherVar".to_owned(),
            })),
        })],
    };
    println!("{}", program);
    assert!(format!("{}", program) == "let myVar = anotherVar;")
}
