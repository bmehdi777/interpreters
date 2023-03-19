use crate::lexer::lexer;
use crate::lexer::token;
use crate::parser::ast;

pub struct Parser {
    l: lexer::Lexer,
    current_token: token::Token,
    peek_token: token::Token,
}

impl Parser {
    pub fn new(mut lexer: lexer::Lexer) -> Parser {
        let cur_tok: token::Token = lexer.next_token();
        let peek_tok: token::Token = lexer.next_token();
        let cur_parser: Parser = Parser {
            l: lexer,
            current_token: cur_tok,
            peek_token: peek_tok,
        };

        cur_parser
    }

    pub fn next_token(&mut self) -> () {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let program: ast::Program = ast::Program { statements: vec![] };
        while self.current_token.token_type != token::TokenType::EOF  {
            let stmt: ast::Statement = self.parse_statement();
            program.statements.push(stmt);
            self.next_token();
        }
        program
    }
    fn parse_statement(&self) -> ast::Statement {
        match self.current_token.token_type {
            token::TokenType::LET => return self.parse_let_statement(),
        }
    }
    fn parse_let_statement(&self) -> ast::Statement {
        let stmt: ast::Statement = ast::Statement::Let( ast::Let {token: self.current_token, name: Box::new(ast::Identifier {token: self.current_token, value: self.current_token.literal}), value: ast::Expression::Identifier {token: self.current_token, value: self.current_token.literal}} );
        
        while !self.current_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }
        stmt
    }

    fn current_token_is(&self, tok: token::TokenType) -> bool {
        self.current_token.token_type == tok
    }
    fn peek_token_is(&self, tok: token::TokenType) -> bool {
        self.peek_token.token_type == tok
    }
    fn expect_peek(&self, tok: token::TokenType) -> bool {
        if self.peek_token_is(tok) {
            self.next_token();
            true
        } else {
            false
        }
    }
}
