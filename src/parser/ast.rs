use crate::lexer::token::Token;

pub trait Node {
    fn token_literals(&self) -> String;
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
}
#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}
#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}
#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Option<Identifier>,
    pub value: Option<Expression>,
}
#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Expression>,
}


impl Node for Program {
    fn token_literals(&self) -> String {
        if self.statements.len() > 0 {
            match &self.statements[0] {
                Statement::Let(a) => return a.token_literals(),
                _ => {},
            }
        }
        "".to_owned()
    }
}
impl Node for Identifier {
    fn token_literals(&self) -> String {
        self.token.literal.to_owned()
    }
}
impl Identifier {
    pub fn expression_node(&self) -> () {}
}

impl Node for LetStatement {
    fn token_literals(&self) -> String {
        self.token.literal.to_owned()
    }

}
impl LetStatement {
    pub fn statement_node(&self) -> () {}
}

impl Node for ReturnStatement {
    fn token_literals(&self) -> String {
        self.token.literal.to_owned()
    }
}
impl ReturnStatement {
    pub fn statement_node(&self) -> () {}
}
