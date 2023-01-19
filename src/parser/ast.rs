use crate::lexer::token;

pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression)
}

pub enum Statement {
    Let(Let)
}
pub enum Expression {}

pub struct Program {
    pub statements: Vec<Statement>,
}

pub struct Identifier {
    pub token: token::Token,
    pub value: String,
}

pub struct Let {
    pub token: token::Token,
    pub name: Identifier,
    pub value: Expression,
}

impl Program {
    pub fn token_literals(&mut self) -> () {}
}
