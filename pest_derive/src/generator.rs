// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use proc_macro::TokenStream;
use quote::{Ident, Tokens};

use super::ast::*;

pub fn generate(name: &str, rules: Vec<Rule>) -> Tokens {
    rule_enum(rules.iter().collect())
}

fn rule_enum(rules: Vec<&Rule>) -> Tokens {
    let rules = rule_names(rules);

    quote! {
        #[allow(dead_code, non_camel_case_types)]
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        pub enum Rule {
            any,
            soi,
            eoi,
            #( #rules ),*
        }
    }
}

fn rule_names(rules: Vec<&Rule>) -> Vec<Tokens> {
    rules.iter().flat_map(|rule| {
        match rule.body {
            Body::Normal(_) => {
                if rule.ty != RuleType::Silent {
                    let name = Ident::new(rule.name.clone());
                    vec![quote! { #name }]
                } else {
                    vec![]
                }
            },
            Body::Infix(_, ref rules) => {
                rule_names(rules.iter().map(|&(ref rule, _)| rule).collect())
            }
        }
    }).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rule_enum_complex() {
        let rules = vec![
            Rule {
                name: Ident::new("rule"),
                ty:   RuleType::Silent,
                body: Body::Infix(
                    Expr::Ident(Ident::new("a")),
                    vec![
                        (Rule {
                            name: Ident::new("b"),
                            ty:   RuleType::Atomic,
                            body: Body::Normal(Expr::Ident(Ident::new("c")))
                        }, false),
                        (Rule {
                            name: Ident::new("d"),
                            ty:   RuleType::Normal,
                            body: Body::Normal(Expr::Ident(Ident::new("e")))
                        }, true)
                    ]
                )
            },
            Rule {
                name: Ident::new("f"),
                ty: RuleType::Normal,
                body: Body::Normal(Expr::Ident(Ident::new("g")))
            }
        ];

        assert_eq!(rule_enum(rules.iter().collect()), quote! {
            #[allow(dead_code, non_camel_case_types)]
            #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
            pub enum Rule {
                any,
                soi,
                eoi,
                b,
                d,
                f
            }
        });
    }
}
