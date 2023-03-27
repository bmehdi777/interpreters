use r_interpreter::lexer::lexer::Lexer;
use r_interpreter::parser::ast::{Node, Statement, Program, ReturnStatement};
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
    check_parser_errors(p);

    assert!(
        program.statements.len() == 3,
        "program.statements does not contain 3 statements. got={}",
        program.statements.len()
    );

    let expected: Vec<&str> = vec!["x", "y", "foobar"];

    for (i, tt) in expected.iter().enumerate() {
        let statement = &program.statements.get(i).expect("Should be a statements");
        if let Statement::Let(l) = statement {
            assert!(l.token_literals() == "let", "let.token_literals() not 'let'. got={}", l.token_literals());
            assert!(l.name.as_ref().unwrap().value == tt.to_owned(), "let.name.value not '{}'. got={}", tt, l.name.as_ref().unwrap().value);
            assert!(l.name.as_ref().unwrap().token_literals() == tt.to_owned(), "let.name.token_literals() not '{}'. got={}", tt, l.name.as_ref().unwrap().token_literals());
        }
    }
}

fn check_parser_errors(prs: Parser) -> () {
    let err: Vec<String> = prs.errors();
    assert!(err.len() == 0, "parser has {} errors.", err.len());
    if err.len() != 0 {
        for e in err.iter() {
            println!("parser error : {}", e);
        }
    }
}

#[test]
fn test_return_statements() -> () {
    let input: &str = "
return 5;
return 10;
return 993322;
        ";

    let l: Lexer = Lexer::new(input.to_owned());
    let mut p: Parser = Parser::new(l);
    let program: Program = p.parse_program();
    check_parser_errors(p);

    assert!(
        program.statements.len() == 3,
        "program.statements does not contain 3 statements. got={}",
        program.statements.len()
    );

    for stmt in program.statements.iter() {
        if let Statement::Return(r) = stmt {
            assert!(r.token_literals() == "return", "return.token_literals() not 'return'. got={}", r.token_literals());
        }
    }
}
