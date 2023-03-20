use crate::lexer::token;

pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

pub enum Statement {
    Let(Let),
}
pub enum Expression {
    Identifier(Identifier),
}

pub struct Program {
    pub statements: Vec<Statement>,
}

pub struct Let {
    pub token: token::Token,
    pub name: Option<Identifier>,
    pub value: Option<Expression>,
}
pub struct Identifier {
    pub token: token::Token,
    pub value: String,
}

impl Program {
    fn token_literals(&self) -> String {
        if self.statements.len() > 0 {
            match &self.statements[0] {
                Statement::Let(a) => return a.token_literals(),
            }
        }
        "".to_owned()
    }
}

impl Let {
    pub fn token_literals(&self) -> String {
        self.token.literal.to_owned()
    }
    pub fn statement_node(&self) -> () {}
}
impl Identifier {
    pub fn token_literals(&self) -> String {
        self.token.literal.to_owned()
    }
    pub fn expression_node(&self) -> () {}
}
