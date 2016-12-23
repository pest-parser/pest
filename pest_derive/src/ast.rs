use quote::Ident;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Rule {
    pub name: Ident,
    pub ty:   RuleType,
    pub body: Body
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RuleType {
    Normal,
    Silent,
    Atomic,
    NonAtomic
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Body {
    Normal(Expr),
    Infix(Expr, Vec<(Rule, bool)>)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
    Char(String),
    Str(String),
    Insens(String),
    Range(String, String),
    Ident(Ident),
    Seq(Vec<Expr>),
    Choice(Vec<Expr>),
    RepZero(Box<Expr>),
    RepOne(Box<Expr>),
    Opt(Box<Expr>),
    PosLhd(Box<Expr>),
    NegLhd(Box<Expr>),
    Push(Box<Expr>)
}