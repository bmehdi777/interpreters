use r_interpreter::lexer::lexer::Lexer;
use r_interpreter::parser::ast::*;
use r_interpreter::parser::parser::Parser;

struct Infix<'a, T> {
    input: &'a str,
    left_value: T,
    operator: &'a str,
    right_value: T,
}
struct PrecedenceTest<'a> {
    input: &'a str,
    expected: &'a str,
}
struct Prefix<'a, T> {
    input: &'a str,
    operator: &'a str,
    value: T,
}

#[test]
fn test_let_statements()  {
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

#[test]
fn test_return_statements()  {
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
fn test_identifier_expression()  {
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

    let _statement = program.statements.get(0).expect("shouldn't be none.");
    assert!(
        matches!(Statement::Expression, _statement),
        "program.statements[0] is not an ast.ExpressionStatement. got={:?}",
        _statement
    );
    let ident = if let Statement::Expression(e) = _statement {
        e
    } else {
        panic!("Not an expression")
    };

    util_test_identifier(ident.expression.as_ref().unwrap(), "foobar".to_owned());
}

#[test]
fn test_integer_expression()  {
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

    let _statement = program.statements.get(0).expect("shouldn't be none.");
    assert!(
        matches!(Statement::Expression, _statement),
        "program.statements[0] is not an ast.ExpressionStatement. got={:?}",
        _statement
    );
    let ident = if let Statement::Expression(e) = _statement {
        e
    } else {
        panic!("Not an expression")
    };

    util_test_integer_literal(ident.expression.as_ref().unwrap(), 5);
}
#[test]
fn test_parsing_boolean_expression()  {
    let input: &str = "true;";

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

    let _statement = program.statements.get(0).expect("shouldn't be none.");
    assert!(
        matches!(Statement::Expression, _statement),
        "program.statements[0] is not an ast.ExpressionStatement. got={:?}",
        _statement
    );
    let ident = if let Statement::Expression(e) = _statement {
        e
    } else {
        panic!("Not an expression")
    };
    util_test_boolean(ident.expression.as_ref().unwrap(), true);
}

#[test]
fn test_parsing_prefix_expression()  {
    let prefix_int_tests: Vec<Prefix<i64>> = vec![
        Prefix {
            input: "!5;",
            operator: "!",
            value: 5,
        },
        Prefix {
            input: "-15",
            operator: "-",
            value: 15,
        },
    ];
    let prefix_bool_tests: Vec<Prefix<bool>> = vec![
        Prefix {
            input: "!true;",
            operator: "!",
            value: true,
        },
        Prefix {
            input: "!false;",
            operator: "!",
            value: false,
        },
    ];

    for prefix_test in prefix_int_tests.iter() {
        let l: Lexer = Lexer::new(prefix_test.input.to_owned());
        let mut p: Parser = Parser::new(l);
        let program: Program = p.parse_program();
        check_parser_errors(p);

        assert!(
            program.statements.len() == 1,
            "program has not enough statements. got={}",
            program.statements.len()
        );

        let _statement = program.statements.get(0).expect("shouldn't be none.");
        assert!(
            matches!(Statement::Expression, _statement),
            "program.statements[0] is not an ast.ExpressionStatement. got={:?}",
            _statement
        );
        let ident = if let Statement::Expression(e) = _statement {
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

            util_test_integer_literal(&*p.right, prefix_test.value);
        } else {
            panic!("Couldn't parse expression to expression::prefix")
        }
    }

    for prefix_test in prefix_bool_tests.iter() {
        let l: Lexer = Lexer::new(prefix_test.input.to_owned());
        let mut p: Parser = Parser::new(l);
        let program: Program = p.parse_program();
        check_parser_errors(p);

        assert!(
            program.statements.len() == 1,
            "program has not enough statements. got={}",
            program.statements.len()
        );

        let _statement = program.statements.get(0).expect("shouldn't be none.");
        assert!(
            matches!(Statement::Expression, _statement),
            "program.statements[0] is not an ast.ExpressionStatement. got={:?}",
            _statement
        );
        let ident = if let Statement::Expression(e) = _statement {
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

            util_test_boolean(&*p.right, prefix_test.value);
        } else {
            panic!("Couldn't parse expression to expression::prefix")
        }
    }
}

#[test]
fn test_parsing_infix_expression()  {
    let infix_int_tests: Vec<Infix<i64>> = vec![
        Infix {
            input: "5 + 5;",
            left_value: 5,
            operator: "+",
            right_value: 5,
        },
        Infix {
            input: "5 - 5;",
            left_value: 5,
            operator: "-",
            right_value: 5,
        },
        Infix {
            input: "5 * 5;",
            left_value: 5,
            operator: "*",
            right_value: 5,
        },
        Infix {
            input: "5 / 5;",
            left_value: 5,
            operator: "/",
            right_value: 5,
        },
        Infix {
            input: "5 > 5;",
            left_value: 5,
            operator: ">",
            right_value: 5,
        },
        Infix {
            input: "5 < 5;",
            left_value: 5,
            operator: "<",
            right_value: 5,
        },
        Infix {
            input: "5 == 5;",
            left_value: 5,
            operator: "==",
            right_value: 5,
        },
        Infix {
            input: "5 != 5;",
            left_value: 5,
            operator: "!=",
            right_value: 5,
        },
    ];

    let infix_bool_tests: Vec<Infix<bool>> = vec![
        Infix {
            input: "true == true;",
            left_value: true,
            operator: "==",
            right_value: true,
        },
        Infix {
            input: "true != true;",
            left_value: true,
            operator: "!=",
            right_value: true,
        },
        Infix {
            input: "false != false;",
            left_value: false,
            operator: "!=",
            right_value: false,
        },
        Infix {
            input: "false == false;",
            left_value: false,
            operator: "==",
            right_value: false,
        },
    ];

    for infix_test in infix_int_tests.iter() {
        let l: Lexer = Lexer::new(infix_test.input.to_owned());
        let mut p: Parser = Parser::new(l);
        let program: Program = p.parse_program();
        check_parser_errors(p);

        assert!(
            program.statements.len() == 1,
            "program has not enough statements. got={}",
            program.statements.len()
        );

        let _statement = program.statements.get(0).expect("shouldn't be none.");
        assert!(
            matches!(Statement::Expression, _statement),
            "program.statements[0] is not an ast.ExpressionStatement. got={:?}",
            _statement
        );
        let ident = if let Statement::Expression(e) = _statement {
            e
        } else {
            panic!("Not an expression")
        };

        let infix_exp: &Expression = ident.expression.as_ref().unwrap();
        if let Expression::Infix(p) = infix_exp {
            assert!(
                p.operator == infix_test.operator,
                "prfx_exp.operator is not '{}'. got={}",
                infix_test.operator,
                p.operator
            );
            util_test_integer_literal(&*p.left, infix_test.left_value);
            util_test_integer_literal(&*p.right, infix_test.right_value);
        } else {
            panic!("Couldn't parse expression to expression::prefix")
        }
    }

    for infix_test in infix_bool_tests.iter() {
        let l: Lexer = Lexer::new(infix_test.input.to_owned());
        let mut p: Parser = Parser::new(l);
        let program: Program = p.parse_program();
        check_parser_errors(p);

        assert!(
            program.statements.len() == 1,
            "program has not enough statements. got={}",
            program.statements.len()
        );

        let _statement = program.statements.get(0).expect("shouldn't be none.");
        assert!(
            matches!(Statement::Expression, _statement),
            "program.statements[0] is not an ast.ExpressionStatement. got={:?}",
            _statement
        );
        let ident = if let Statement::Expression(e) = _statement {
            e
        } else {
            panic!("Not an expression")
        };

        let infix_exp: &Expression = ident.expression.as_ref().unwrap();
        if let Expression::Infix(p) = infix_exp {
            assert!(
                p.operator == infix_test.operator,
                "prfx_exp.operator is not '{}'. got={}",
                infix_test.operator,
                p.operator
            );
            util_test_boolean(&*p.left, infix_test.left_value);
            util_test_boolean(&*p.right, infix_test.right_value);
        } else {
            panic!("Couldn't parse expression to expression::prefix")
        }

    }
}

#[test]
fn test_operator_precedence_parsing()  {
    let tests: Vec<PrecedenceTest> = vec![
        PrecedenceTest {
            input: "-a * b",
            expected: "((-a) * b)",
        },
        PrecedenceTest {
            input: "!-a",
            expected: "(!(-a))",
        },
        PrecedenceTest {
            input: "a + b + c",
            expected: "((a + b) + c)",
        },
        PrecedenceTest {
            input: "a + b - c",
            expected: "((a + b) - c)",
        },
        PrecedenceTest {
            input: "a * b * c",
            expected: "((a * b) * c)",
        },
        PrecedenceTest {
            input: "a * b / c",
            expected: "((a * b) / c)",
        },
        PrecedenceTest {
            input: "a + b * c + d / e - f",
            expected: "(((a + (b * c)) + (d / e)) - f)",
        },
        PrecedenceTest {
            input: "3 + 4 - 5 * 5",
            expected: "((3 + 4) - (5 * 5))",
        },
        PrecedenceTest {
            input: "5 > 4 == 3 < 4",
            expected: "((5 > 4) == (3 < 4))",
        },
        PrecedenceTest {
            input: "5 < 4 != 3 > 4",
            expected: "((5 < 4) != (3 > 4))",
        },
        PrecedenceTest {
            input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
            expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
        PrecedenceTest {
            input: "true",
            expected: "true"
        },
        PrecedenceTest {
            input: "false",
            expected: "false",
        }, 
        PrecedenceTest {
            input: "3 > 5 == false",
            expected: "((3 > 5) == false)"
        },
        PrecedenceTest {
            input: "3 < 5 == true",
            expected: "((3 < 5) == true)"
        },
        PrecedenceTest {
            input: "1 + (2 + 3) + 4",
            expected: "((1 + (2 + 3)) + 4)"
        },
        PrecedenceTest {
            input: "(5 + 5) * 2",
            expected: "((5 + 5) * 2)"
        },
        PrecedenceTest {
            input: "2 / (5 + 5)",
            expected: "(2 / (5 + 5))"
        },
        PrecedenceTest {
            input: "-(5 + 5)",
            expected: "(-(5 + 5))"
        },
        PrecedenceTest {
            input: "!(true == true)",
            expected: "(!(true == true))"
        }
    ];

    for t in tests.iter() {
        let l: Lexer = Lexer::new(t.input.to_owned());
        let mut p: Parser = Parser::new(l);
        let program: Program = p.parse_program();
        check_parser_errors(p);

        let actual: String = format!("{}", program);
        assert!(
            actual == t.expected,
            "expected={}. got={}",
            t.expected,
            actual
        );
    }
}
#[test]
fn test_if_expression()  {
    let input: &str = "if (x < y) { x }";
    let l: Lexer = Lexer::new(input.to_owned());
    let mut p: Parser = Parser::new(l);
    let program: Program = p.parse_program();
    check_parser_errors(p);

    assert!(program.statements.len() == 1, "program.statements does not contains {} statements. got={}", 1, program.statements.len());
    let _statement = program.statements.get(0).expect("shouldn't be none.");
    assert!(
        matches!(Statement::Expression, _statement),
        "program.statements[0] is not an ast.ExpressionStatement. got={:?}",
        _statement
        );
    let ident = if let Statement::Expression(e) = _statement {
        e
    } else {
        panic!("Not an expression");
    };

    let if_exp: &Expression = ident.expression.as_ref().unwrap();
    if let Expression::If(i) = if_exp {
        assert!(i.consequence.statements.len() == 1, "consequence is not 1 statements, got={}", i.consequence.statements.len());
        let consequence: &Expression = if let Statement::Expression(e) = i.consequence.statements.get(0).expect("Shouldn't be empty") {
            &e.expression.as_ref().unwrap()
        } else {
            panic!("consequence should contain an expression statement")
        };
        util_test_identifier(consequence, "x".to_owned());
        assert!(i.alternative.as_ref().is_none(), "if_exp.alternative was not None");
        // todo : next part of the consequence.statement[0] test
    } else {
        panic!("if_exp not an IfExpression");
    }
}

#[test]
fn test_if_else_expression() {
    let input: &str = "if (x < y) { x } else { y }";
    let l: Lexer = Lexer::new(input.to_owned());
    let mut p: Parser = Parser::new(l);
    let program: Program = p.parse_program();
    check_parser_errors(p);

    assert!(program.statements.len() == 1, "program.statements does not contains {} statements. got={}", 1, program.statements.len());
    let _statement = program.statements.get(0).expect("shouldn't be none.");
    assert!(
        matches!(Statement::Expression, _statement),
        "program.statements[0] is not an ast.ExpressionStatement. got={:?}",
        _statement
        );
    let ident = if let Statement::Expression(e) = _statement {
        e
    } else {
        panic!("Not an expression");
    };

    let if_exp: &Expression = ident.expression.as_ref().unwrap();
    if let Expression::If(i) = if_exp {
        assert!(i.consequence.statements.len() == 1, "consequence is not 1 statements, got={}", i.consequence.statements.len());
        let consequence: &Expression = if let Statement::Expression(e) = i.consequence.statements.get(0).expect("Shouldn't be empty") {
            &e.expression.as_ref().unwrap()
        } else {
            panic!("consequence should contain an expression statement")
        };
        util_test_identifier(consequence, "x".to_owned());
        assert!(i.alternative.as_ref().is_some(), "if_exp.alternative was not None");
        // todo : next part of the consequence.statement[0] test
    } else {
        panic!("if_exp not an IfExpression");
    }
}

fn util_test_integer_literal(exp: &Expression, value: i64)  {
    if let Expression::Integer(intg) = exp {
        assert!(
            intg.value == value,
            "intg.value not {}. got={}",
            value,
            intg.value
        );
        assert!(
            intg.token_literals() == format!("{}", value),
            "intg.token_literals() not {}. got={}",
            value,
            intg.token_literals()
        );
    } else {
        panic!("exp should be an Integer");
    };
}
fn util_test_identifier(exp: &Expression, value: String)  {
    if let Expression::Identifier(ident) = exp {
        assert!(
            ident.value == value,
            "ident.value not {}. got={}",
            value,
            ident.value
        );
        assert!(
            ident.token_literals() == value,
            "ident.token_literals not {}. got={}",
            value,
            ident.token_literals()
        );
    } else {
        panic!("exp should be a Identifier");
    }
}
fn util_test_boolean(exp: &Expression, value: bool) {
    if let Expression::Boolean(b) = exp {
        assert!(b.value == value, "b.value not {}. got={}", value, b.value);
        assert!(
            b.token_literals() == format!("{}", value),
            "b.token_literals() not {}. got={}",
            value,
            b.token_literals()
        );
    }
}
fn check_parser_errors(prs: Parser)  {
    let err: Vec<String> = prs.errors();
    for e in err.iter() {
        println!("parser error : {}", e);
    }
    assert!(err.len() == 0, "parser has {} errors.", err.len());
}
