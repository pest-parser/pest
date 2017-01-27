use super::ast::*;

//pub fn optimize(rules: Vec<Rule>) -> Vec<Rule> {
//
//}

fn push_string(option: Option<String>, string: String) -> Option<String> {
    match option {
        Some(mut last) => {
            last.push_str(&string);
            Some(last)
        },
        None => Some(string)
    }
}

fn concat_string_sequences(rules: Vec<Rule>) -> Vec<Rule> {
    map_all_exprs(rules, |expr| {
        match expr {
            Expr::Seq(exprs) => {
                let mut last_string = None;
                let mut concatenated = vec![];

                for expr in exprs {
                    match expr {
                        Expr::Str(string) => last_string = push_string(last_string, string),
                        expr => {
                            if let Some(string) = last_string {
                                concatenated.push(Expr::Str(string));
                                last_string = None;
                            }

                            concatenated.push(expr);
                        }
                    };
                }

                if let Some(string) = last_string {
                    concatenated.push(Expr::Str(string));
                }

                Expr::Seq(concatenated)
            }
            expr => expr
        }
    })
}

fn concat_insensitive_sequences(rules: Vec<Rule>) -> Vec<Rule> {
    map_all_exprs(rules, |expr| {
        match expr {
            Expr::Seq(exprs) => {
                let mut last_string = None;
                let mut concatenated = vec![];

                for expr in exprs {
                    match expr {
                        Expr::Insens(string) => last_string = push_string(last_string, string),
                        expr => {
                            if let Some(string) = last_string {
                                concatenated.push(Expr::Insens(string));
                                last_string = None;
                            }

                            concatenated.push(expr);
                        }
                    };
                }

                if let Some(string) = last_string {
                    concatenated.push(Expr::Insens(string));
                }

                Expr::Seq(concatenated)
            }
            expr => expr
        }
    })
}

#[cfg(test)]
mod tests {
    use quote::Ident;

    use super::*;

    #[test]
    fn concat_strings() {
        let rules = vec![
            Rule {
                name: Ident::new("rule"),
                ty:   RuleType::Silent,
                body: Body::Normal(
                    Expr::Choice(vec![
                        Expr::Str("a".to_owned()),
                        Expr::Seq(vec![
                            Expr::Str("b".to_owned()),
                            Expr::Str("c".to_owned()),
                            Expr::Insens("d".to_owned()),
                            Expr::Str("e".to_owned()),
                            Expr::Str("f".to_owned())
                        ])
                    ])
                )
            }
        ];
        let concatenated = vec![
            Rule {
                name: Ident::new("rule"),
                ty:   RuleType::Silent,
                body: Body::Normal(
                    Expr::Choice(vec![
                        Expr::Str("a".to_owned()),
                        Expr::Seq(vec![
                            Expr::Str("bc".to_owned()),
                            Expr::Insens("d".to_owned()),
                            Expr::Str("ef".to_owned())
                        ])
                    ])
                )
            }
        ];

        assert_eq!(concat_string_sequences(rules), concatenated);
    }

    #[test]
    fn concat_insensitive() {
        let rules = vec![
            Rule {
                name: Ident::new("rule"),
                ty:   RuleType::Silent,
                body: Body::Normal(
                    Expr::Choice(vec![
                        Expr::Str("a".to_owned()),
                        Expr::Seq(vec![
                            Expr::Insens("b".to_owned()),
                            Expr::Insens("c".to_owned()),
                            Expr::Str("d".to_owned()),
                            Expr::Insens("e".to_owned()),
                            Expr::Insens("f".to_owned())
                        ])
                    ])
                )
            }
        ];
        let concatenated = vec![
            Rule {
                name: Ident::new("rule"),
                ty:   RuleType::Silent,
                body: Body::Normal(
                    Expr::Choice(vec![
                        Expr::Str("a".to_owned()),
                        Expr::Seq(vec![
                            Expr::Insens("bc".to_owned()),
                            Expr::Str("d".to_owned()),
                            Expr::Insens("ef".to_owned())
                        ])
                    ])
                )
            }
        ];

        assert_eq!(concat_insensitive_sequences(rules), concatenated);
    }
}
