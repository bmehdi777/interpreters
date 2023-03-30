use r_interpreter::lexer::lexer;
use r_interpreter::lexer::token;

#[derive(Debug)]
struct ExpectedToken {
    expected_type: token::TokenType,
    expected_literals: String,
}

#[test]
fn test_next_token_1() -> () {
    let input: &str = "=+(){},;";
    let test: Vec<ExpectedToken> = vec![
        ExpectedToken {
            expected_type: token::TokenType::ASSIGN,
            expected_literals: String::from("="),
        },
        ExpectedToken {
            expected_type: token::TokenType::PLUS,
            expected_literals: String::from("+"),
        },
        ExpectedToken {
            expected_type: token::TokenType::LPAREN,
            expected_literals: String::from("("),
        },
        ExpectedToken {
            expected_type: token::TokenType::RPAREN,
            expected_literals: String::from(")"),
        },
        ExpectedToken {
            expected_type: token::TokenType::LBRACE,
            expected_literals: String::from("{"),
        },
        ExpectedToken {
            expected_type: token::TokenType::RBRACE,
            expected_literals: String::from("}"),
        },
        ExpectedToken {
            expected_type: token::TokenType::COMMA,
            expected_literals: String::from(","),
        },
        ExpectedToken {
            expected_type: token::TokenType::SEMICOLON,
            expected_literals: String::from(";"),
        },
        ExpectedToken {
            expected_type: token::TokenType::EOF,
            expected_literals: String::from(""),
        },
    ];

    let mut lexer: lexer::Lexer = lexer::Lexer::new(input.to_string());
    for (index, token_exp) in test.iter().enumerate() {
        let tok: token::Token = lexer.next_token();
        assert!(
            tok.token_type == token_exp.expected_type,
            "tests[{}] - token_type wrong. expected={:?}, got={:?}",
            index,
            token_exp.expected_type,
            tok.token_type
        );
        assert!(
            tok.literal == token_exp.expected_literals,
            "tests[{}] - literal wrong. expected={:?}, got={:?}",
            index,
            token_exp.expected_literals,
            tok.literal
        );
    }
}

#[test]
fn test_next_token_2() -> () {
    let input: &str = "
        let five = 5;
        let ten = 10;

        let add = fn(x,y) {
            x + y;
        }

        let result = add(five, ten); ";

    let test: Vec<ExpectedToken> = vec![
        ExpectedToken {
            expected_type: token::TokenType::LET,
            expected_literals: String::from("let"),
        },
        ExpectedToken {
            expected_type: token::TokenType::IDENT,
            expected_literals: String::from("five"),
        },
        ExpectedToken {
            expected_type: token::TokenType::ASSIGN,
            expected_literals: String::from("="),
        },
        ExpectedToken {
            expected_type: token::TokenType::INT,
            expected_literals: String::from("5"),
        },
        ExpectedToken {
            expected_type: token::TokenType::SEMICOLON,
            expected_literals: String::from(";"),
        },
        ExpectedToken {
            expected_type: token::TokenType::LET,
            expected_literals: String::from("let"),
        },
        ExpectedToken {
            expected_type: token::TokenType::IDENT,
            expected_literals: String::from("ten"),
        },
        ExpectedToken {
            expected_type: token::TokenType::ASSIGN,
            expected_literals: String::from("="),
        },
        ExpectedToken {
            expected_type: token::TokenType::INT,
            expected_literals: String::from("10"),
        },
        ExpectedToken {
            expected_type: token::TokenType::SEMICOLON,
            expected_literals: String::from(";"),
        },
        ExpectedToken {
            expected_type: token::TokenType::LET,
            expected_literals: String::from("let"),
        },
        ExpectedToken {
            expected_type: token::TokenType::IDENT,
            expected_literals: String::from("add"),
        },
        ExpectedToken {
            expected_type: token::TokenType::ASSIGN,
            expected_literals: String::from("="),
        },
        ExpectedToken {
            expected_type: token::TokenType::FUNCTION,
            expected_literals: String::from("fn"),
        },
        ExpectedToken {
            expected_type: token::TokenType::LPAREN,
            expected_literals: String::from("("),
        },
        ExpectedToken {
            expected_type: token::TokenType::IDENT,
            expected_literals: String::from("x"),
        },
        ExpectedToken {
            expected_type: token::TokenType::COMMA,
            expected_literals: String::from(","),
        },
        ExpectedToken {
            expected_type: token::TokenType::IDENT,
            expected_literals: String::from("y"),
        },
        ExpectedToken {
            expected_type: token::TokenType::RPAREN,
            expected_literals: String::from(")"),
        },
        ExpectedToken {
            expected_type: token::TokenType::LBRACE,
            expected_literals: String::from("{"),
        },
        ExpectedToken {
            expected_type: token::TokenType::IDENT,
            expected_literals: String::from("x"),
        },
        ExpectedToken {
            expected_type: token::TokenType::PLUS,
            expected_literals: String::from("+"),
        },
        ExpectedToken {
            expected_type: token::TokenType::IDENT,
            expected_literals: String::from("y"),
        },
        ExpectedToken {
            expected_type: token::TokenType::SEMICOLON,
            expected_literals: String::from(";"),
        },
        ExpectedToken {
            expected_type: token::TokenType::RBRACE,
            expected_literals: String::from("}"),
        },
        ExpectedToken {
            expected_type: token::TokenType::LET,
            expected_literals: String::from("let"),
        }, // here
        ExpectedToken {
            expected_type: token::TokenType::IDENT,
            expected_literals: String::from("result"),
        },
        ExpectedToken {
            expected_type: token::TokenType::ASSIGN,
            expected_literals: String::from("="),
        },
        ExpectedToken {
            expected_type: token::TokenType::IDENT,
            expected_literals: String::from("add"),
        },
        ExpectedToken {
            expected_type: token::TokenType::LPAREN,
            expected_literals: String::from("("),
        },
        ExpectedToken {
            expected_type: token::TokenType::IDENT,
            expected_literals: String::from("five"),
        },
        ExpectedToken {
            expected_type: token::TokenType::COMMA,
            expected_literals: String::from(","),
        },
        ExpectedToken {
            expected_type: token::TokenType::IDENT,
            expected_literals: String::from("ten"),
        },
        ExpectedToken {
            expected_type: token::TokenType::RPAREN,
            expected_literals: String::from(")"),
        },
        ExpectedToken {
            expected_type: token::TokenType::SEMICOLON,
            expected_literals: String::from(";"),
        },
        ExpectedToken {
            expected_type: token::TokenType::EOF,
            expected_literals: String::from(""),
        },
    ];

    let mut lexer: lexer::Lexer = lexer::Lexer::new(input.to_string());
    for (index, token_exp) in test.iter().enumerate() {
        let tok: token::Token = lexer.next_token();
        assert!(
            tok.token_type == token_exp.expected_type,
            "tests[{}] - token_type wrong. expected={:?}, got={:?}",
            index,
            token_exp.expected_type,
            tok.token_type
        );
        assert!(
            tok.literal == token_exp.expected_literals,
            "tests[{}] - literal wrong. expected={:?}, got={:?}",
            index,
            token_exp.expected_literals,
            tok.literal
        );
    }
}

#[test]
fn test_next_token_3() -> () {
    let input: &str = "
        !-/*5;
        5 < 10 > 5;";

    let test: Vec<ExpectedToken> = vec![
        ExpectedToken {
            expected_type: token::TokenType::BANG,
            expected_literals: String::from("!"),
        },
        ExpectedToken {
            expected_type: token::TokenType::MINUS,
            expected_literals: String::from("-"),
        },
        ExpectedToken {
            expected_type: token::TokenType::SLASH,
            expected_literals: String::from("/"),
        },
        ExpectedToken {
            expected_type: token::TokenType::ASTERISK,
            expected_literals: String::from("*"),
        },
        ExpectedToken {
            expected_type: token::TokenType::INT,
            expected_literals: String::from("5"),
        },
        ExpectedToken {
            expected_type: token::TokenType::SEMICOLON,
            expected_literals: String::from(";"),
        },
        ExpectedToken {
            expected_type: token::TokenType::INT,
            expected_literals: String::from("5"),
        },
        ExpectedToken {
            expected_type: token::TokenType::LT,
            expected_literals: String::from("<"),
        },
        ExpectedToken {
            expected_type: token::TokenType::INT,
            expected_literals: String::from("10"),
        },
        ExpectedToken {
            expected_type: token::TokenType::GT,
            expected_literals: String::from(">"),
        },
        ExpectedToken {
            expected_type: token::TokenType::INT,
            expected_literals: String::from("5"),
        },
        ExpectedToken {
            expected_type: token::TokenType::SEMICOLON,
            expected_literals: String::from(";"),
        },
    ];

    let mut lexer: lexer::Lexer = lexer::Lexer::new(input.to_string());
    for (index, token_exp) in test.iter().enumerate() {
        let tok: token::Token = lexer.next_token();
        assert!(
            tok.token_type == token_exp.expected_type,
            "tests[{}] - token_type wrong. expected={:?}, got={:?}",
            index,
            token_exp.expected_type,
            tok.token_type
        );
        assert!(
            tok.literal == token_exp.expected_literals,
            "tests[{}] - literal wrong. expected={:?}, got={:?}",
            index,
            token_exp.expected_literals,
            tok.literal
        );
    }
}

#[test]
fn test_next_token_4() -> () {
    let input: &str = "
        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        10 == 10;
        10 != 9;
        ";

    let test: Vec<ExpectedToken> = vec![
        ExpectedToken {
            expected_type: token::TokenType::IF,
            expected_literals: String::from("if"),
        },
        ExpectedToken {
            expected_type: token::TokenType::LPAREN,
            expected_literals: String::from("("),
        },
        ExpectedToken {
            expected_type: token::TokenType::INT,
            expected_literals: String::from("5"),
        },
        ExpectedToken {
            expected_type: token::TokenType::LT,
            expected_literals: String::from("<"),
        },
        ExpectedToken {
            expected_type: token::TokenType::INT,
            expected_literals: String::from("10"),
        },
        ExpectedToken {
            expected_type: token::TokenType::RPAREN,
            expected_literals: String::from(")"),
        },
        ExpectedToken {
            expected_type: token::TokenType::LBRACE,
            expected_literals: String::from("{"),
        },
        ExpectedToken {
            expected_type: token::TokenType::RETURN,
            expected_literals: String::from("return"),
        },
        ExpectedToken {
            expected_type: token::TokenType::TRUE,
            expected_literals: String::from("true"),
        },
        ExpectedToken {
            expected_type: token::TokenType::SEMICOLON,
            expected_literals: String::from(";"),
        },
        ExpectedToken {
            expected_type: token::TokenType::RBRACE,
            expected_literals: String::from("}"),
        },
        ExpectedToken {
            expected_type: token::TokenType::ELSE,
            expected_literals: String::from("else"),
        },
        ExpectedToken {
            expected_type: token::TokenType::LBRACE,
            expected_literals: String::from("{"),
        },
        ExpectedToken {
            expected_type: token::TokenType::RETURN,
            expected_literals: String::from("return"),
        },
        ExpectedToken {
            expected_type: token::TokenType::FALSE,
            expected_literals: String::from("false"),
        },
        ExpectedToken {
            expected_type: token::TokenType::SEMICOLON,
            expected_literals: String::from(";"),
        },
        ExpectedToken {
            expected_type: token::TokenType::RBRACE,
            expected_literals: String::from("}"),
        },
        ExpectedToken {
            expected_type: token::TokenType::INT,
            expected_literals: String::from("10"),
        },
        ExpectedToken {
            expected_type: token::TokenType::EQ,
            expected_literals: String::from("=="),
        },
        ExpectedToken {
            expected_type: token::TokenType::INT,
            expected_literals: String::from("10"),
        },
        ExpectedToken {
            expected_type: token::TokenType::SEMICOLON,
            expected_literals: String::from(";"),
        },
        ExpectedToken {
            expected_type: token::TokenType::INT,
            expected_literals: String::from("10"),
        },
        ExpectedToken {
            expected_type: token::TokenType::NOTEQ,
            expected_literals: String::from("!="),
        },
        ExpectedToken {
            expected_type: token::TokenType::INT,
            expected_literals: String::from("9"),
        },
        ExpectedToken {
            expected_type: token::TokenType::SEMICOLON,
            expected_literals: String::from(";"),
        },
    ];

    let mut lexer: lexer::Lexer = lexer::Lexer::new(input.to_string());
    for (index, token_exp) in test.iter().enumerate() {
        let tok: token::Token = lexer.next_token();
        assert!(
            tok.token_type == token_exp.expected_type,
            "tests[{}] - token_type wrong. expected={:?}, got={:?}",
            index,
            token_exp.expected_type,
            tok.token_type
        );
        assert!(
            tok.literal == token_exp.expected_literals,
            "tests[{}] - literal wrong. expected={:?}, got={:?}",
            index,
            token_exp.expected_literals,
            tok.literal
        );
    }
}
