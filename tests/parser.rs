use r_interpreter::parser::ast;
use r_interpreter::parser::parser;
use r_interpreter::lexer::lexer;

#[test]
fn test_let_statements() -> () {
    let input: &str = "
        let x = 5;
        let y = 10;
        let foobar = 83838383;
        ";

    let l: lexer::Lexer = lexer::Lexer::new(input.to_string());
    let p: parser::Parser = parser::Parser::new(l);
    let program: ast::Program = p.parse_program();

    assert!(program.statements.len() == 3, "program.statements does not contain 3 statements. got={}", program.statements.len());

    let test: Vec<&str> = vec!["x","y","foobar"];
    for (i, tt) in test.iter().enumerate() {
        test_let_statement(program.statements[i], tt);
    }

}

fn test_let_statement(statement: &impl ast::Statement, name: String ) -> () {
    assert!(statement.token_literal() == "let", "statement.token_literals not 'let'. got={}", statement.token_literal());
}
