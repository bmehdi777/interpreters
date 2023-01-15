use std::io::{self, Write};
use crate::lexer;
use crate::token;

const PROMPT: &str = ">> ";

pub fn start() {
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        let mut line: String = String::new();
        io::stdin().read_line(&mut line).expect("An error occured.");
        
        let mut lex: lexer::Lexer = lexer::Lexer::new(line);
        let mut tok: token::Token = lex.next_token();
        while tok.token_type != token::TokenType::EOF {
            println!("{:?}", tok);
            tok = lex.next_token();
        }
        println!("{:?}", tok);
    }
}
