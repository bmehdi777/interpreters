use crate::lexer::token::Token;

pub trait Node {
    fn token_literals(&self) -> String;
}

#[derive(Debug)]
pub enum Statement {
    Let(Let),
    Return(Return),
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
pub struct Program {
    pub statements: Vec<Statement>,
}
#[derive(Debug)]
pub struct Let {
    pub token: Token,
    pub name: Option<Identifier>,
    pub value: Option<Expression>,
}
#[derive(Debug)]
pub struct Return {
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

impl Node for Let {
    fn token_literals(&self) -> String {
        self.token.literal.to_owned()
    }

}
impl Let {
    pub fn statement_node(&self) -> () {}
}

impl Node for Return {
    fn token_literals(&self) -> String {
        self.token.literal.to_owned()
    }
}
impl Return {
    pub fn statement_node(&self) -> () {}
}
