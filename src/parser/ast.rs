use crate::lexer::token::Token;
use std::fmt;

pub trait Node {
    fn token_literals(&self) -> String;
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}
#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    Integer(IntegerLiteral),
    Boolean(Boolean),

    Prefix(Prefix),
    Infix(Infix),
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}
#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Expression>,
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
#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}
#[derive(Debug)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}
#[derive(Debug)]
pub struct Prefix {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}
#[derive(Debug)]
pub struct Infix {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for Program {
    fn token_literals(&self) -> String {
        if self.statements.len() > 0 {
            match &self.statements[0] {
                Statement::Let(a) => return a.token_literals(),
                _ => {}
            }
        }
        "".to_owned()
    }
}
impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in self.statements.iter() {
            match statement {
                Statement::Let(l) => return l.fmt(f),
                Statement::Return(r) => return r.fmt(f),
                Statement::Expression(e) => return e.fmt(f),
            }
        }
        Ok(())
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(i) => i.fmt(f),
            Expression::Integer(i) => i.fmt(f),
            Expression::Boolean(b) => b.fmt(f),
            Expression::Prefix(p) => p.fmt(f),
            Expression::Infix(i) => i.fmt(f),
        }
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
impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for ExpressionStatement {
    fn token_literals(&self) -> String {
        self.token.literal.to_owned()
    }
}
impl ExpressionStatement {
    pub fn statement_node(&self) -> () {}
}
impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.expression
            .as_ref()
            .expect("Expression should not be empty")
            .fmt(f)
    }
}

impl Node for LetStatement {
    fn token_literals(&self) -> String {
        self.token.literal.to_owned()
    }
}
impl LetStatement {
    pub fn statement_node(&self) -> () {}
}
impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {} = {};",
            self.token_literals(),
            self.name.as_ref().expect("name shouldn't be empty.").value,
            self.value.as_ref().expect("value shouldn't be empty.")
        )
    }
}

impl Node for ReturnStatement {
    fn token_literals(&self) -> String {
        self.token.literal.to_owned()
    }
}
impl ReturnStatement {
    pub fn statement_node(&self) -> () {}
}
impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {};",
            self.token_literals(),
            self.return_value
                .as_ref()
                .expect("return_value shouldn't be empty.")
        )
    }
}

impl Node for IntegerLiteral {
    fn token_literals(&self) -> String {
        self.token.literal.to_owned()
    }
}
impl IntegerLiteral {
    pub fn expression_node(&self) -> () {}
}
impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_literals())
    }
}

impl Node for Boolean {
    fn token_literals(&self) -> String {
        self.token.literal.to_owned()
    }
}
impl Boolean {
    pub fn expression_node(&self) -> () {}
}
impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_literals())
    }
}

impl Node for Prefix {
    fn token_literals(&self) -> String {
        self.token.literal.to_owned()
    }
}
impl Prefix {
    pub fn expression_node(&self) -> () {}
}
impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

impl Node for Infix {
    fn token_literals(&self) -> String {
        self.token.literal.to_owned()
    }
}
impl Infix {
    pub fn expression_node(&self) -> () {}
}
impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}
