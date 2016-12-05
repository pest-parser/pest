use std::iter::Peekable;
use std::str::Chars;

use super::ast::*;

macro_rules! err {
    ($slf:ident, $message:expr) => {
        Err(format!("{} {}", $message, $slf.pos()))
    }
}

struct PestParser<'a> {
    chars: Peekable<Chars<'a>>,
    prev:  Option<char>,
    line:  usize,
    col:   usize
}

impl<'a> PestParser<'a> {
    pub fn new(chars: Peekable<Chars>) -> PestParser {
        PestParser {
            chars: chars,
            prev:  None,
            line:  1,
            col:   1
        }
    }

    fn pos(&self) -> String {
        format!("line {}, column {}", self.line, self.col)
    }

    fn adv(&mut self, c: Option<char>) {
        if let Some(prev) = self.prev {
            if prev == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        }

        self.prev = c;
    }

    fn next(&mut self) -> Result<char, String> {
        match self.chars.next() {
            Some(c) => {
                self.adv(Some(c));
                Ok(c)
            },
            None => {
                self.adv(None);
                err!(self, "Unexpected end of input at")
            }
        }
    }

    fn whitespace(&mut self) {
        loop {
            if let Some(c) = self.peek() {
                if !c.is_whitespace() {
                    break;
                }

                self.next().unwrap();
            } else {
                break;
            }
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|c| *c)
    }

    pub fn ident(&mut self) -> Result<Expr, String> {
        let mut ident = vec![];

        let c = self.next()?;

        if c.is_alphabetic() || c == '_' {
            ident.push(c);

            loop {
                let c = match self.chars.peek() {
                    Some(c) => *c,
                    None    => break
                };

                if c.is_alphabetic() || c.is_digit(10) || c == '_' {
                    ident.push(self.next()?)
                } else {
                    break;
                }
            }
        } else {
            if c.is_digit(10) {
                return err!(self, "Rules can only start with alphabetic characters or '_' at");
            } else {
                return err!(self, "Expected rule at");
            }
        }

        Ok(Expr::Ident(ident.into_iter().collect()))
    }

    pub fn char(&mut self) -> Result<Expr, String> {
        let mut char = vec![];

        let c = self.next()?;

        if c == '\'' {
            char.push(c);
            let mut prev = c;
            let mut c = self.next()?;

            while c != '\'' || prev == '\\' {
                char.push(c);
                prev = c;
                c = self.next()?;
            }

            char.push(c);
        } else {
            return err!(self, "Expected char at");
        }

        Ok(Expr::Char(char.into_iter().collect()))
    }

    pub fn string(&mut self) -> Result<Expr, String> {
        let mut string = vec![];

        let c = self.next()?;

        if c == '"' {
            string.push(c);
            let mut prev = c;
            let mut c = self.next()?;

            while c != '"' || prev == '\\' {
                string.push(c);
                prev = c;
                c = self.next()?;
            }

            string.push(c);
        } else {
            return err!(self, "Expected string at");
        }

        Ok(Expr::Str(string.into_iter().collect()))
    }

    pub fn insens(&mut self) -> Result<Expr, String> {
        let c = self.next()?;

        if c == '^' || self.peek() != Some('"') {
            let expr = self.string()?;

            if let Expr::Str(string) = expr {
                Ok(Expr::Insens(string))
            } else {
                unreachable!()
            }
        } else {
            err!(self, "Expected case-insensitive string at")
        }
    }

    pub fn range(&mut self) -> Result<Expr, String> {
        let start = self.char()?;

        for _ in 0..2 {
            self.whitespace();

            let c = self.next()?;
            if c != '.' {
                return err!(self, "Expected '.' at");
            }
        }

        self.whitespace();

        let end = self.char()?;

        if let (Expr::Char(start), Expr::Char(end)) = (start, end) {
            Ok(Expr::Range(start, end))
        } else {
            unreachable!()
        }
    }

    pub fn term(&mut self) -> Result<Expr, String> {
        let mut term = match self.peek() {
            Some(c) => match c {
                '"'  => self.string(),
                '^'  => self.insens(),
                '\'' => self.range(),
                c if c.is_alphabetic() || c == '_' => self.ident(),
                '&'  => {
                    self.next()?;
                    self.whitespace();

                    Ok(Expr::PosLhd(Box::new(self.term()?)))
                },
                '!'  => {
                    self.next()?;
                    self.whitespace();

                    Ok(Expr::NegLhd(Box::new(self.term()?)))
                },
                '('  => self.paren(),
                _    => return err!(self, "Expecting grammar expression term at")
            },
            None => return err!(self, "Unexpected end of input at")
        };

        self.whitespace();

        while let Some(c) = self.peek() {
            match c {
                '?' => {
                    self.next()?;
                    self.whitespace();

                    term = Ok(Expr::Opt(Box::new(term?)));
                },
                '*' => {
                    self.next()?;
                    self.whitespace();

                    term = Ok(Expr::RepZero(Box::new(term?)));
                },
                '+' => {
                    self.next()?;
                    self.whitespace();

                    term = Ok(Expr::RepOne(Box::new(term?)));
                },
                _   => break
            }
        }

        term
    }

    pub fn paren(&mut self) -> Result<Expr, String> {
        if let Ok('(') = self.next() {
            self.whitespace();

            let expr = self.expr();

            if let Ok(')') = self.next() {
                expr
            } else {
                err!(self, "Expected ')' at")
            }
        } else {
            err!(self, "Expected '(' at")
        }
    }

    pub fn expr(&mut self) -> Result<Expr, String> {
        let mut sequences = vec![self.seq()?];

        while let Some('|') = self.peek() {
            self.next()?;
            self.whitespace();

            sequences.push(self.seq()?);
        }

        if sequences.len() == 1 {
            Ok(sequences.pop().unwrap())
        } else {
            Ok(Expr::Choice(sequences))
        }
    }

    fn seq(&mut self) -> Result<Expr, String> {
        let mut terms = vec![self.term()?];

        while let Some('~') = self.peek() {
            self.next()?;
            self.whitespace();

            terms.push(self.term()?);
        }

        if terms.len() == 1 {
            Ok(terms.pop().unwrap())
        } else {
            Ok(Expr::Seq(terms))
        }
    }

    pub fn rule(&mut self) -> Result<Rule, String> {
        if let Expr::Ident(name) = self.ident()? {
            self.whitespace();

            if self.next()? == '=' {
                self.whitespace();

                let (ty, body) = match self.peek() {
                    Some(c) => match c {
                        '{' => (RuleType::Normal, self.body()?),
                        '_' => {
                            self.next()?;
                            self.whitespace();

                            (RuleType::Silent, self.body()?)
                        },
                        '@' => {
                            self.next()?;
                            self.whitespace();

                            (RuleType::Atomic, self.body()?)
                        },
                        '!' => {
                            self.next()?;

                            if self.next()? != '@' {
                                return err!(self, "Expecting '@' at");
                            }

                            self.whitespace();

                            (RuleType::NonAtomic, self.body()?)
                        },
                        _ => return err!(self, "Expected modifier ('_', '@', or '!@') or '{' at")
                    },
                    None => return err!(self, "Unexpected end of input at")
                };

                Ok(Rule {
                    name: name,
                    ty:   ty,
                    body: body
                })
            } else {
                return err!(self, "Expected '=' after rule name at");
            }
        } else {
            unreachable!()
        }
    }

    fn body(&mut self) -> Result<Body, String> {
        if let Ok('{') = self.next() {
            self.whitespace();

            if let Some('{') = self.peek() {
                self.next()?;
                self.whitespace();

                let term = self.expr()?;

                if self.next() != Ok('}') {
                    return err!(self, "Expected '}' at");
                }

                self.whitespace();

                let mut rules = vec![];

                while let Some(c) = self.peek() {
                    if c.is_alphabetic() || c == '_' {
                        rules.push(self.rule_infix()?);

                        self.whitespace();
                    } else {
                        break;
                    }
                }

                if let Ok('}') = self.next() {
                    Ok(Body::Infix(term, rules))
                } else {
                    err!(self, "Expected '}' at")
                }
            } else {
                let expr = self.expr()?;

                if let Ok('}') = self.next() {
                    Ok(Body::Normal(expr))
                } else {
                    err!(self, "Expected '}' at")
                }
            }
        } else {
            err!(self, "Rule definitions start with '{' at")
        }
    }

    fn rule_infix(&mut self) -> Result<(Rule, bool), String> {
        if let Expr::Ident(name) = self.ident()? {
            self.whitespace();

            if self.next()? == '=' {
                self.whitespace();

                let (ty, (body, right_assoc)) = match self.peek() {
                    Some(c) => match c {
                        '{' => (RuleType::Normal, self.body_infix()?),
                        '_' => {
                            self.next()?;
                            self.whitespace();

                            (RuleType::Silent, self.body_infix()?)
                        },
                        '@' => {
                            self.next()?;
                            self.whitespace();

                            (RuleType::Atomic, self.body_infix()?)
                        },
                        '!' => {
                            self.next()?;

                            if self.next()? != '@' {
                                return err!(self, "Expecting '@' at");
                            }

                            self.whitespace();

                            (RuleType::NonAtomic, self.body_infix()?)
                        },
                        _ => return err!(self, "Expected modifier ('_', '@', or '!@') or '{' at")
                    },
                    None => return err!(self, "Unexpected end of input at")
                };

                let rule = Rule {
                    name: name,
                    ty:   ty,
                    body: body
                };

                Ok((rule, right_assoc))
            } else {
                return err!(self, "Expected '=' after rule name at");
            }
        } else {
            unreachable!()
        }
    }

    fn body_infix(&mut self) -> Result<(Body, bool), String> {
        if let Ok('{') = self.next() {
            self.whitespace();

            let right_assoc = if let Some(c) = self.peek() {
                let right_assoc = c == '<';

                if right_assoc {
                    self.next()?;
                }

                right_assoc
            } else {
                return err!(self, "Unexpected end of input at");
            };

            self.whitespace();

            let expr = self.expr();

            if let Ok('}') = self.next() {
                Ok((Body::Normal(expr?), right_assoc))
            } else {
                err!(self, "Expected '}' at")
            }
        } else {
            err!(self, "Rule definitions start with '{' at")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn peek() {
        let mut parser = PestParser::new("abc".chars().peekable());

        assert_eq!(parser.peek(), Some('a'));
        parser.ident().unwrap();
        assert_eq!(parser.peek(), None);
    }

    #[test]
    fn ident() {
        let mut parser = PestParser::new("abc".chars().peekable());

        assert_eq!(parser.ident(), Ok(Expr::Ident("abc".to_owned())));
        assert_eq!(parser.ident(), Err("Unexpected end of input at line 1, column 4".to_owned()));
    }

    #[test]
    fn ident_digit() {
        let mut parser = PestParser::new("1".chars().peekable());

        assert_eq!(parser.ident(), Err("Rules can only start with alphabetic characters or '_' at \
                                        line 1, column 1".to_owned()));
    }

    #[test]
    fn ident_symbol() {
        let mut parser = PestParser::new("=".chars().peekable());

        assert_eq!(parser.ident(), Err("Expected rule at line 1, column 1".to_owned()));
    }

    #[test]
    fn string() {
        let mut parser = PestParser::new("\"a\\\"\"".chars().peekable());

        assert_eq!(parser.string(), Ok(Expr::Str("\"a\\\"\"".to_owned())));
    }

    #[test]
    fn string_no_end() {
        let mut parser = PestParser::new("\"a\\\"".chars().peekable());

        assert_eq!(parser.string(), Err("Unexpected end of input at line 1, column 5".to_owned()));
    }

    #[test]
    fn char() {
        let mut parser = PestParser::new("'\\''".chars().peekable());

        assert_eq!(parser.char(), Ok(Expr::Char("'\\''".to_owned())));
    }

    #[test]
    fn char_no_end() {
        let mut parser = PestParser::new("'\\'".chars().peekable());

        assert_eq!(parser.char(), Err("Unexpected end of input at line 1, column 4".to_owned()));
    }

    #[test]
    fn insens() {
        let mut parser = PestParser::new("^\"a\\\"\"".chars().peekable());

        assert_eq!(parser.insens(), Ok(Expr::Insens("\"a\\\"\"".to_owned())));
    }

    #[test]
    fn range() {
        let mut parser = PestParser::new("'a' .\n. 'z'".chars().peekable());

        assert_eq!(parser.range(), Ok(Expr::Range("'a'".to_owned(), "'z'".to_owned())));
    }

    #[test]
    fn range_missing_dot() {
        let mut parser = PestParser::new("'a'.'z'".chars().peekable());

        assert_eq!(parser.range(), Err("Expected \'.\' at line 1, column 5".to_owned()));
    }

    #[test]
    fn term_string() {
        let mut parser = PestParser::new("\"hi\"".chars().peekable());

        assert_eq!(parser.term(), Ok(Expr::Str("\"hi\"".to_owned())));
    }

    #[test]
    fn term_insens() {
        let mut parser = PestParser::new("^\"hi\"".chars().peekable());

        assert_eq!(parser.term(), Ok(Expr::Insens("\"hi\"".to_owned())));
    }

    #[test]
    fn term_range() {
        let mut parser = PestParser::new("'a'..'z'".chars().peekable());

        assert_eq!(parser.term(), Ok(Expr::Range("'a'".to_owned(), "'z'".to_owned())));
    }

    #[test]
    fn term_underscore_ident() {
        let mut parser = PestParser::new("_hi".chars().peekable());

        assert_eq!(parser.term(), Ok(Expr::Ident("_hi".to_owned())));
    }

    #[test]
    fn term_ident() {
        let mut parser = PestParser::new("hi".chars().peekable());

        assert_eq!(parser.term(), Ok(Expr::Ident("hi".to_owned())));
    }

    #[test]
    fn term_prefix_postfix() {
        let mut parser = PestParser::new("& ! hi ? * +".chars().peekable());

        assert_eq!(parser.term(), Ok(
            Expr::PosLhd(Box::new(
                Expr::NegLhd(Box::new(
                    Expr::RepOne(Box::new(
                        Expr::RepZero(Box::new(
                            Expr::Opt(Box::new(
                                Expr::Ident("hi".to_owned())
                            ))
                        ))
                    ))
                ))
            ))
        ));
    }

    #[test]
    fn term_paren() {
        let mut parser = PestParser::new("( ( ( \"hi\" ) ) )".chars().peekable());

        assert_eq!(parser.term(), Ok(Expr::Str("\"hi\"".to_owned())));
    }

    #[test]
    fn term_not_closed() {
        let mut parser = PestParser::new("(((\"hi\"))".chars().peekable());

        assert_eq!(parser.term(), Err("Expected ')' at line 1, column 10".to_owned()));
    }

    #[test]
    fn precedence1() {
        let mut parser = PestParser::new("a ~ b | c ~ d".chars().peekable());

        assert_eq!(parser.expr(), Ok(
            Expr::Choice(vec![
                Expr::Seq(vec![
                    Expr::Ident("a".to_owned()),
                    Expr::Ident("b".to_owned())
                ]),
                Expr::Seq(vec![
                    Expr::Ident("c".to_owned()),
                    Expr::Ident("d".to_owned())
                ])
            ])
        ));
    }

    #[test]
    fn precedence2() {
        let mut parser = PestParser::new("a | b ~ c ~ d | e | f".chars().peekable());

        assert_eq!(parser.expr(), Ok(
            Expr::Choice(vec![
                Expr::Ident("a".to_owned()),
                Expr::Seq(vec![
                    Expr::Ident("b".to_owned()),
                    Expr::Ident("c".to_owned()),
                    Expr::Ident("d".to_owned())
                ]),
                Expr::Ident("e".to_owned()),
                Expr::Ident("f".to_owned())
            ])
        ));
    }

    #[test]
    fn complex() {
        let mut parser = PestParser::new("_a | 'a'..'b' ~ !^\"abc\" ~ (d | e)*?".chars().peekable());

        assert_eq!(parser.expr(), Ok(
            Expr::Choice(vec![
                Expr::Ident("_a".to_owned()),
                Expr::Seq(vec![
                    Expr::Range("'a'".to_owned(), "'b'".to_owned()),
                    Expr::NegLhd(Box::new(
                        Expr::Insens("\"abc\"".to_owned())
                    )),
                    Expr::Opt(Box::new(
                        Expr::RepZero(Box::new(
                            Expr::Choice(vec![
                                Expr::Ident("d".to_owned()),
                                Expr::Ident("e".to_owned())
                            ])
                        ))
                    ))
                ])
            ])
        ));
    }

    #[test]
    fn rule() {
        let mut parser = PestParser::new("rule = { a }".chars().peekable());

        assert_eq!(parser.rule(), Ok(
            Rule {
                name: "rule".to_owned(),
                ty:   RuleType::Normal,
                body: Body::Normal(Expr::Ident("a".to_owned()))
            }
        ));
    }

    #[test]
    fn rule_silent() {
        let mut parser = PestParser::new("rule = _{ a }".chars().peekable());

        assert_eq!(parser.rule(), Ok(
            Rule {
                name: "rule".to_owned(),
                ty:   RuleType::Silent,
                body: Body::Normal(Expr::Ident("a".to_owned()))
            }
        ));
    }

    #[test]
    fn rule_atomic() {
        let mut parser = PestParser::new("rule = @{ a }".chars().peekable());

        assert_eq!(parser.rule(), Ok(
            Rule {
                name: "rule".to_owned(),
                ty:   RuleType::Atomic,
                body: Body::Normal(Expr::Ident("a".to_owned()))
            }
        ));
    }

    #[test]
    fn rule_non_atomic() {
        let mut parser = PestParser::new("rule = !@{ a }".chars().peekable());

        assert_eq!(parser.rule(), Ok(
            Rule {
                name: "rule".to_owned(),
                ty:   RuleType::NonAtomic,
                body: Body::Normal(Expr::Ident("a".to_owned()))
            }
        ));
    }

    #[test]
    fn rule_infix() {
        let mut parser = PestParser::new("rule = _{ { a } b = @{ c } d = {< e } }".chars().peekable());

        assert_eq!(parser.rule(), Ok(
            Rule {
                name: "rule".to_owned(),
                ty:   RuleType::Silent,
                body: Body::Infix(
                    Expr::Ident("a".to_owned()),
                    vec![
                        (Rule {
                            name: "b".to_owned(),
                            ty:   RuleType::Atomic,
                            body: Body::Normal(Expr::Ident("c".to_owned()))
                        }, false),
                        (Rule {
                            name: "d".to_owned(),
                            ty:   RuleType::Normal,
                            body: Body::Normal(Expr::Ident("e".to_owned()))
                        }, true)
                    ]
                )
            }
        ));
    }
}