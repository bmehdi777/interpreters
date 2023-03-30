use r_interpreter::lexer::lexer::Lexer;
use r_interpreter::parser::ast::*;
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
            assert!(
                l.token_literals() == "let",
                "let.token_literals() not 'let'. got={}",
                l.token_literals()
            );
            assert!(
                l.name.as_ref().unwrap().value == tt.to_owned(),
                "let.name.value not '{}'. got={}",
                tt,
                l.name.as_ref().unwrap().value
            );
            assert!(
                l.name.as_ref().unwrap().token_literals() == tt.to_owned(),
                "let.name.token_literals() not '{}'. got={}",
                tt,
                l.name.as_ref().unwrap().token_literals()
            );
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
            assert!(
                r.token_literals() == "return",
                "return.token_literals() not 'return'. got={}",
                r.token_literals()
            );
        }
    }
}

#[test]
fn test_identifier_expression() -> () {
    let input: &str = "foobar;";

    let l: Lexer = Lexer::new(input.to_owned());
    let mut p: Parser = Parser::new(l);
    let program: Program = p.parse_program();
    check_parser_errors(p);

    println!("program: {}", program);
    assert!(
        program.statements.len() == 1,
        "program has not enough statements. got={}",
        program.statements.len()
    );

    let statement = program.statements.get(0).expect("shouldn't be none.");
    assert!(
        matches!(Statement::Expression, statement),
        "program.statements[0] is not an ast.ExpressionStatement. got={:?}",
        statement
    );
    let ident = if let Statement::Expression(e) = statement {
        e
    } else {
        panic!("Not an expression")
    };

    let expr: &Expression = ident.expression.as_ref().unwrap();
    if let Expression::Identifier(i) = expr {
        assert!(
            i.value == "foobar",
            "ident.value not {}. got={}",
            "foobar",
            i.value
        );
        assert!(
            i.token_literals() == "foobar",
            "ident.token_literals not {}. got={}",
            "foobar",
            i.token_literals()
        );
    }
}

#[test]
fn test_integer_expression() -> () {
    let input: &str = "5;";

    let l: Lexer = Lexer::new(input.to_owned());
    let mut p: Parser = Parser::new(l);
    let program: Program = p.parse_program();
    check_parser_errors(p);

    println!("program: {}", program);
    assert!(
        program.statements.len() == 1,
        "program has not enough statements. got={}",
        program.statements.len()
    );

    let statement = program.statements.get(0).expect("shouldn't be none.");
    assert!(
        matches!(Statement::Expression, statement),
        "program.statements[0] is not an ast.ExpressionStatement. got={:?}",
        statement
    );
    let ident = if let Statement::Expression(e) = statement {
        e
    } else {
        panic!("Not an expression")
    };

    let literal: &Expression = ident.expression.as_ref().unwrap();
    if let Expression::Integer(i) = literal {
        assert!(i.value == 5, "ident.value not {}. got={}", "5", i.value);
        assert!(
            i.token_literals() == "5",
            "ident.token_literals not '{}'. got={}",
            "5",
            i.token_literals()
        );
    }
}

#[test]
fn test_parsing_prefix_expression() -> () {
    struct Prefix<'a> {
        input: &'a str,
        operator: &'a str,
        integer_value: i64,
    }

    let prefix_tests: Vec<Prefix> = vec![
        Prefix {
            input: "!5;",
            operator: "!",
            integer_value: 5,
        },
        Prefix {
            input: "-15",
            operator: "-",
            integer_value: 15,
        },
    ];

    for prefix_test in prefix_tests.iter() {
        let l: Lexer = Lexer::new(prefix_test.input.to_owned());
        let mut p: Parser = Parser::new(l);
        let program: Program = p.parse_program();
        check_parser_errors(p);

        assert!(
            program.statements.len() == 1,
            "program has not enough statements. got={}",
            program.statements.len()
        );

        let statement = program.statements.get(0).expect("shouldn't be none.");
        assert!(
            matches!(Statement::Expression, statement),
            "program.statements[0] is not an ast.ExpressionStatement. got={:?}",
            statement
        );
        let ident = if let Statement::Expression(e) = statement {
            e
        } else {
            panic!("Not an expression")
        };

        let prfx_exp: &Expression = ident.expression.as_ref().unwrap();
        if let Expression::Prefix(p) = prfx_exp {
            assert!(
                p.operator == prefix_test.operator,
                "prfx_exp.operator is not '{}'. got={}",
                prefix_test.operator,
                p.operator
            );

            if let Expression::Integer(i) = &*p.right {
                assert!(
                    i.value == prefix_test.integer_value,
                    "i.value not {}. got={}",
                    prefix_test.integer_value,
                    i.value
                );
                assert!(
                    i.token_literals() == prefix_test.integer_value.to_string(),
                    "i.token_literals() not {}. got={}",
                    prefix_test.integer_value.to_string(),
                    i.token_literals()
                );
            } else {
                panic!("p.right should be an integer")
            }
        } else {
            panic!("Couldn't parse expression to expression::prefix")
        }
    }
}
