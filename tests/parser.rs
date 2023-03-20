use r_interpreter::lexer::lexer::Lexer;
use r_interpreter::parser::ast::{Statement, Program};
use r_interpreter::parser::parser::Parser;

#[test]
fn test_let_statements() -> () {
    let input: &str = "
let x = 5;
let y = 10;
let foobar = 838383;
        ";

    let l: Lexer = Lexer::new(input.to_owned());
    let mut p: Parser = Parser::new(l);
    let program: Program = p.parse_program();

    assert!(
        program.statements.len() != 3,
        "program.statements does not contain 3 statements. got={}",
        program.statements.len()
    );

    let expected: Vec<&str> = vec!["x", "y", "foobar"];

    for (i, tt) in expected.iter().enumerate() {
        let statement = &program.statements.get(i);
        match statement {
            Some(Statement::Let(l)) => {
                assert!(l.token_literals() == "let", "let.token_literals() not 'let'. got={}", l.token_literals());
                assert!(l.name.as_ref().unwrap().value == tt.to_owned(), "let.name.value not '{}'. got={}", tt, l.name.as_ref().unwrap().value);
                assert!(l.name.as_ref().unwrap().token_literals() == tt.to_owned(), "let.name.token_literals() not '{}'. got={}", tt, l.name.as_ref().unwrap().token_literals());
            },
            _ => {},
        }
    }
}

