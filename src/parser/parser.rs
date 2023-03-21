use crate::lexer::lexer::Lexer;
use crate::lexer::token::{TokenType, Token};
use crate::parser::ast::{Statement, Let, Return, Program, Identifier};

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
        let cur_parser: Parser = Parser {
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
            _ => None,
        }
    }
    fn parse_let_statement(&mut self) -> Option<Statement> {
        let mut stmt: Statement = Statement::Let(Let {token: self.current_token.clone(), name: None, value: None});

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
        let stmt: Statement = Statement::Return(Return {token: self.current_token.clone(), return_value: None});

        self.next_token();

        while !self.current_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(stmt)
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
