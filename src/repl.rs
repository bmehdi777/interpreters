use crate::lexer::{lexer::*, token::*};
use crate::parser::{parser::*, ast::*};
use std::io::{self, Write};

const PROMPT: &str = ">> ";

pub fn start_RLPL() {
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        let mut line: String = String::new();
        io::stdin().read_line(&mut line).expect("An error occured.");

        let mut lex: Lexer = Lexer::new(line);
        let mut tok: Token = lex.next_token();
        while tok.token_type != TokenType::EOF {
            println!("{:?}", tok);
            tok = lex.next_token();
        }
        println!("{:?}", tok);
    }
}

pub fn start() {
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        let mut line: String = String::new();
        io::stdin().read_line(&mut line).expect("An error occured.");

        let lex: Lexer = Lexer::new(line);
        let mut parser: Parser = Parser::new(lex);
        let program: Program = parser.parse_program();
        if parser.errors().len() != 0 {
            print_parse_error(parser.errors());
            continue;
        }
        println!("{}\n", program);

    }
}

fn print_parse_error(errors: Vec<String>) {
    println!("Errors :");
    errors.iter().for_each(|err| println!("\t{}\n", err));
}
