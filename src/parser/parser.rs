use crate::lexer::lexer::Lexer;
use crate::lexer::token::{TokenType, Token};
use crate::parser::ast::{Statement, Expression,ExpressionStatement, LetStatement, ReturnStatement, Program, Identifier};

pub enum Precedence {
    LOWEST,
    EQUALS, // ==
    LESSGREATER, // > or <
    SUM,    // +
    PRODUCT, // *
    PREFIX, // -X or !X
    CALL // myFunction(X)
}

type PrefixParseFn = fn(&mut Parser)->Expression;
type InfixParseFn = fn(&mut Parser, Expression)->Expression;

pub struct Parser {
    l: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Parser {
        let cur_tok: Token = lexer.next_token();
        let peek_tok: Token = lexer.next_token();
        let mut cur_parser: Parser = Parser {
            l: lexer,
            current_token: cur_tok,
            peek_token: peek_tok,
            errors: vec![],
        };

        cur_parser
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.to_owned()
    }

    pub fn next_token(&mut self) -> () {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program: Program = Program { statements: vec![] };
        while self.current_token.token_type != TokenType::EOF  {
            let stmt: Option<Statement> = self.parse_statement();
            if let Some(st) = stmt {
                program.statements.push(st);
            }
            self.next_token();
        }
        program
    }
    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token.token_type {
            TokenType::LET => return self.parse_let_statement(),
            TokenType::RETURN => return self.parse_return_statement(),
            _ => return self.parse_expression_statement(),
        }
    }
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix = self.prefix_call(&self.current_token.token_type).unwrap();
        let left_expression = prefix(self);
        
        Some(left_expression)
    }


    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let stmt: Statement = Statement::Expression(ExpressionStatement {token: self.current_token.clone(), expression: self.parse_expression(Precedence::LOWEST) }) ;

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(stmt)
    }
    fn parse_identifier(&mut self) -> Expression {
        Expression::Identifier(Identifier { token: self.current_token.clone(), value: self.current_token.clone().literal})
    }
    fn parse_let_statement(&mut self) -> Option<Statement> {
        let mut stmt: Statement = Statement::Let(LetStatement {token: self.current_token.clone(), name: None, value: None});

        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }

        if let Statement::Let(ref mut st) = stmt {
            (*st).name = Some(Identifier { token: self.current_token.clone(), value: self.current_token.literal.clone() });
        }

        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }
        
        while !self.current_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(stmt)
    }
    fn parse_return_statement(&mut self) -> Option<Statement> {
        let stmt: Statement = Statement::Return(ReturnStatement {token: self.current_token.clone(), return_value: None});

        self.next_token();

        while !self.current_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(stmt)
    }
    fn parse_integer_literal(&mut self) -> () {
        // todo
    }

    fn prefix_call(&self, token_type: &TokenType) -> Option<PrefixParseFn> {
        match token_type {
            TokenType::IDENT => Some(Parser::parse_identifier),
            _ => None
        }
    }
    fn register_call(&mut self, token_type: TokenType) -> () {
        match token_type {
            _ => ()
        }
    }

    fn current_token_is(&self, tok: TokenType) -> bool {
        self.current_token.token_type == tok
    }
    fn peek_token_is(&self, tok: TokenType) -> bool {
        self.peek_token.token_type == tok
    }
    fn peek_errors(&mut self, tok: TokenType) -> () {
        let err: String = format!("Next token should be {:?} instead of {:?}.", tok, self.peek_token.token_type);
        println!("{}", &err);
        self.errors.push(err);
    }
    fn expect_peek(&mut self, tok: TokenType) -> bool {
        if self.peek_token_is(tok.to_owned()) {
            self.next_token();
            true
        } else {
            self.peek_errors(tok);
            false
        }
    }
}
