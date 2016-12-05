#[derive(Debug, Eq, PartialEq)]
pub struct Rule {
    pub name: String,
    pub ty:   RuleType,
    pub body: Body
}

#[derive(Debug, Eq, PartialEq)]
pub enum RuleType {
    Normal,
    Silent,
    Atomic,
    NonAtomic
}

#[derive(Debug, Eq, PartialEq)]
pub enum Body {
    Normal(Expr),
    Infix(Expr,Vec<(Rule, bool)>)
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Char(String),
    Str(String),
    Insens(String),
    Range(String, String),
    Ident(String),
    Seq(Vec<Expr>),
    Choice(Vec<Expr>),
    RepZero(Box<Expr>),
    RepOne(Box<Expr>),
    Opt(Box<Expr>),
    PosLhd(Box<Expr>),
    NegLhd(Box<Expr>),
    Push(Box<Expr>)
}