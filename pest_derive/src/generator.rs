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
    let rules: Vec<_> = rules.into_iter()
                             .filter(|rule| rule.ty != RuleType::Silent)
                             .map(|rule| {
                                 let name = Ident::new(rule.name.clone());
                                 quote! { #name }
                             })
                             .collect();

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rule_enum_complex() {
        let rules = vec![
            Rule {
                name: Ident::new("rule"),
                ty:   RuleType::Silent,
                expr: Expr::Ident(Ident::new("a"))
            },
            Rule {
                name: Ident::new("f"),
                ty: RuleType::Normal,
                expr: Expr::Ident(Ident::new("g"))
            }
        ];

        assert_eq!(rule_enum(rules.iter().collect()), quote! {
            #[allow(dead_code, non_camel_case_types)]
            #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
            pub enum Rule {
                any,
                soi,
                eoi,
                f
            }
        });
    }
}
