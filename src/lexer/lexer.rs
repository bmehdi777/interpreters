use crate::lexer::token;
use crate::utils;

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lexer: Lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };
        lexer.read_char();
        lexer
    }
    pub fn read_char(&mut self) -> () {
        self.ch = if self.read_position >= self.input.len() {
            0
        } else {
            self.input.bytes().nth(self.read_position).unwrap()
        };
        self.position = self.read_position;
        self.read_position += 1;
    }
    pub fn next_token(&mut self) -> token::Token {
        let tok: token::Token;

        self.skip_whitespace();

        match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    let ch: String = (self.ch as char).to_string();
                    self.read_char();
                    tok =
                        token::Token::new(token::TokenType::EQ, ch + &(self.ch as char).to_string())
                } else {
                    tok = token::Token::new(token::TokenType::ASSIGN, String::from("="))
                }
            }
            b';' => tok = token::Token::new(token::TokenType::SEMICOLON, String::from(";")),
            b'(' => tok = token::Token::new(token::TokenType::LPAREN, String::from("(")),
            b')' => tok = token::Token::new(token::TokenType::RPAREN, String::from(")")),
            b'{' => tok = token::Token::new(token::TokenType::LBRACE, String::from("{")),
            b'}' => tok = token::Token::new(token::TokenType::RBRACE, String::from("}")),
            b',' => tok = token::Token::new(token::TokenType::COMMA, String::from(",")),
            b'+' => tok = token::Token::new(token::TokenType::PLUS, String::from("+")),
            b'-' => tok = token::Token::new(token::TokenType::MINUS, String::from("-")),
            b'!' => {
                if self.peek_char() == b'=' {
                    let ch: String = (self.ch as char).to_string();
                    self.read_char();
                    tok = token::Token::new(
                        token::TokenType::NOTEQ,
                        ch + &(self.ch as char).to_string(),
                    )
                } else {
                    tok = token::Token::new(token::TokenType::BANG, String::from("!"))
                }
            }
            b'/' => tok = token::Token::new(token::TokenType::SLASH, String::from("/")),
            b'*' => tok = token::Token::new(token::TokenType::ASTERISK, String::from("*")),
            b'<' => tok = token::Token::new(token::TokenType::LT, String::from("<")),
            b'>' => tok = token::Token::new(token::TokenType::GT, String::from(">")),
            0 => tok = token::Token::new(token::TokenType::EOF, String::from("")),
            _ => {
                if utils::is_letter(self.ch) {
                    let ident: &str = &self.read_identifier();
                    tok = token::Token::new(token::Token::lookup_ident(&ident), ident.to_string());
                    return tok;
                } else if utils::is_digit(self.ch) {
                    tok = token::Token::new(token::TokenType::INT, self.read_number());
                    return tok;
                }

                return token::Token::new(token::TokenType::ILLEGAL, self.ch.to_string());
            }
        }
        self.read_char();
        tok
    }

    fn read_identifier(&mut self) -> String {
        let position: usize = self.position;
        while utils::is_letter(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }
    fn read_number(&mut self) -> String {
        let position: usize = self.position;
        while utils::is_digit(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.bytes().nth(self.read_position).unwrap()
        }
    }

    fn skip_whitespace(&mut self) -> () {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char();
        }
    }
}
