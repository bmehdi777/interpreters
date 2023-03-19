use r_interpreter::lexer::lexer;
use r_interpreter::parser::ast;
use r_interpreter::parser::parser;

#[test]
fn test_let_statements() -> () {
    let input: &str = "
let x = 5;
let y = 10;
let foobar = 838383;
        ";

    let l: lexer::Lexer = lexer::Lexer::new(input.to_owned());
    let p: parser::Parser = parser::Parser::new(l);
    let program: ast::Program = p.parse_program();

    assert!(
        program.statements.len() != 3,
        "program.statements does not contain 3 statements. got={}",
        program.statements.len()
    );

    let expected: Vec<&str> = vec!["x", "y", "foobar"];

    for (i, tt) in expected.iter().enumerate() {
        let statement = program.statements.get(i);
        match statement {
            ast::Statement::Let(l) => {
                assert!(l.token_literals() == "let", "let.token_literals() not 'let'. got={}", l.token_literals());
                assert!(l.name.value == tt, "let.name.value not '{}'. got={}", tt, l.name.value);
                assert!(l.name.token_literals() == tt, "let.name.token_literals() not '{}'. got={}", tt, l.name.token_literals());
            },
        }
    }
}

