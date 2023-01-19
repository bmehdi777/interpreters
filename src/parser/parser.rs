use crate::parser::ast;
use crate::lexer::lexer;
use crate::lexer::token;

pub struct Parser {
    l: lexer::Lexer,
    current_token: token::Token,
    peek_token: token::Token,
}

impl Parser {
    pub fn new(mut lexer: lexer::Lexer) -> Parser {
        let cur_tok: token::Token = lexer.next_token();
        let peek_tok: token::Token = lexer.next_token();
        let cur_parser: Parser = Parser { l: lexer, current_token: cur_tok, peek_token: peek_tok };

        cur_parser
    } 

    pub fn next_token(&mut self) -> () {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> () {}
}
