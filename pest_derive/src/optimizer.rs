use super::ast::*;

pub fn optimize(rules: Vec<Rule>) -> Vec<Rule> {
    let optimized = concat_string_sequences(rules);
    let optimized = concat_insensitive_sequences(optimized);
    let optimized = extract_common_choice_sequences(optimized);

    optimized
}

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

fn extract_common_choice_sequences(rules: Vec<Rule>) -> Vec<Rule> {
    map_all_exprs(rules, |expr| {
        match expr {
            Expr::Choice(exprs) => {
                let choice = Expr::Choice(extract_common_sequences(exprs));

                map_expr(choice, &mut |expr| {
                    match expr {
                        Expr::Choice(exprs) => {
                            if exprs.len() == 1 {
                                exprs[0].clone()
                            } else {
                                Expr::Choice(exprs)
                            }
                        },
                        expr => expr
                    }
                })
            },
            expr => expr
        }
    })
}

fn extract_common_sequences(choices: Vec<Expr>) -> Vec<Expr> {
    fn skip(expr: Expr, choices: Vec<Expr>) -> Vec<Expr> {
        choices.into_iter().skip_while(|other_expr| {
            match other_expr.clone() {
                Expr::Seq(exprs) => {
                    !exprs.is_empty() && expr == exprs[0]
                },
                other_expr => expr == other_expr
            }
        }).collect()
    }

    if choices.len() <= 1 {
        return choices;
    }

    match choices[0].clone() {
        Expr::Seq(exprs) => {
            let mut i = 0;
            let common = choices.iter().cloned().take_while(|expr| {
                let matches = match expr.clone() {
                    Expr::Seq(other_exprs) => exprs[0] == other_exprs[0],
                    _ => false
                };

                if matches {
                    i += 1;
                }

                matches
            }).map(|expr| {
                match expr.clone() {
                    Expr::Seq(mut exprs) => {
                        match exprs.len() {
                            2 => {
                                exprs[1].clone()
                            },
                            _ => {
                                exprs.remove(0);
                                Expr::Seq(exprs)
                            }
                        }
                    }
                    _ => unreachable!()
                }
            }).collect();

            let common = extract_common_sequences(common);

            let mut skipped = skip(exprs[0].clone(), extract_common_sequences(choices.clone().split_off(i)));

            let mut extracted = if common.len() == 1 {
                match common[0].clone() {
                    Expr::Seq(mut other_exprs) => {
                        other_exprs.insert(0, exprs[0].clone());
                        vec![Expr::Seq(other_exprs)]
                    },
                    _ => unreachable!()
                }
            } else {
                vec![
                    Expr::Seq(vec![
                        exprs[0].clone(),
                        Expr::Choice(common)
                    ])
                ]
            };
            extracted.append(&mut skipped);

            extracted
        },
        expr => {
            let mut skipped = skip(expr.clone(), choices[1..].iter().cloned().collect());
            skipped.insert(0, expr);

            skipped
        }
    }
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

    #[test]
    fn extract_empty() {
        assert_eq!(extract_common_sequences(vec![]), vec![]);
    }

    #[test]
    fn simple_common_sequence() {
        let choices = vec![
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Str("b".to_owned())
            ]),
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Str("c".to_owned())
            ])
        ];
        let common = vec![
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Choice(vec![
                    Expr::Str("b".to_owned()),
                    Expr::Str("c".to_owned())
                ])
            ])
        ];

        assert_eq!(extract_common_sequences(choices), common);
    }

    #[test]
    fn long_common_sequence() {
        let choices = vec![
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Str("b".to_owned()),
                Expr::Str("c".to_owned())
            ]),
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Str("b".to_owned()),
                Expr::Str("d".to_owned())
            ])
        ];
        let common = vec![
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Str("b".to_owned()),
                Expr::Choice(vec![
                    Expr::Str("c".to_owned()),
                    Expr::Str("d".to_owned())
                ])
            ])
        ];

        assert_eq!(extract_common_sequences(choices), common);
    }

    #[test]
    fn complex_common_sequences() {
        let choices = vec![
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Str("b".to_owned()),
                Expr::Str("c".to_owned())
            ]),
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Str("b".to_owned()),
                Expr::Str("d".to_owned())
            ]),
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Str("e".to_owned())
            ])
        ];
        let common = vec![
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Choice(vec![
                    Expr::Seq(vec![
                        Expr::Str("b".to_owned()),
                        Expr::Choice(vec![
                            Expr::Str("c".to_owned()),
                            Expr::Str("d".to_owned())
                        ])
                    ]),
                    Expr::Str("e".to_owned())
                ])
            ])
        ];

        assert_eq!(extract_common_sequences(choices), common);
    }

    #[test]
    fn two_common_sequences() {
        let choices = vec![
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Str("b".to_owned()),
            ]),
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Str("c".to_owned()),
            ]),
            Expr::Seq(vec![
                Expr::Str("d".to_owned()),
                Expr::Str("b".to_owned()),
            ]),
            Expr::Seq(vec![
                Expr::Str("d".to_owned()),
                Expr::Str("c".to_owned()),
            ]),
        ];
        let common = vec![
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Choice(vec![
                    Expr::Str("b".to_owned()),
                    Expr::Str("c".to_owned())
                ])
            ]),
            Expr::Seq(vec![
                Expr::Str("d".to_owned()),
                Expr::Choice(vec![
                    Expr::Str("b".to_owned()),
                    Expr::Str("c".to_owned())
                ])
            ])
        ];

        assert_eq!(extract_common_sequences(choices), common);
    }

    #[test]
    fn skip_same_expr() {
        let choices = vec![
            Expr::Str("a".to_owned()),
            Expr::Str("a".to_owned())
        ];
        let skipped = vec![
            Expr::Str("a".to_owned())
        ];

        assert_eq!(extract_common_sequences(choices), skipped);
    }

    #[test]
    fn skip_seuqnce() {
        let choices = vec![
            Expr::Str("a".to_owned()),
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Str("b".to_owned())
            ])
        ];
        let skipped = vec![
            Expr::Str("a".to_owned())
        ];

        assert_eq!(extract_common_sequences(choices), skipped);
    }

    #[test]
    fn skip_after_extraction() {
        let choices = vec![
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Str("b".to_owned())
            ]),
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Str("c".to_owned())
            ]),
            Expr::Str("a".to_owned()),
            Expr::Str("a".to_owned())
        ];
        let common = vec![
            Expr::Seq(vec![
                Expr::Str("a".to_owned()),
                Expr::Choice(vec![
                    Expr::Str("b".to_owned()),
                    Expr::Str("c".to_owned())
                ])
            ])
        ];

        assert_eq!(extract_common_sequences(choices), common);
    }

    #[test]
    fn extract_common_choice() {
        let rules = vec![
            Rule {
                name: Ident::new("rule"),
                ty:   RuleType::Silent,
                body: Body::Normal(
                    Expr::Choice(vec![
                        Expr::Seq(vec![
                            Expr::Str("a".to_owned()),
                            Expr::Str("b".to_owned())
                        ]),
                        Expr::Seq(vec![
                            Expr::Str("a".to_owned()),
                            Expr::Str("c".to_owned())
                        ])
                    ])
                )
            }
        ];
        let optimized = vec![
            Rule {
                name: Ident::new("rule"),
                ty:   RuleType::Silent,
                body: Body::Normal(
                    Expr::Seq(vec![
                        Expr::Str("a".to_owned()),
                        Expr::Choice(vec![
                            Expr::Str("b".to_owned()),
                            Expr::Str("c".to_owned())
                        ])
                    ])
                )
            }
        ];
    }
}
