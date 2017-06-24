// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use proc_macro::TokenStream;
use quote::{Ident, Tokens};

use super::ast::*;

pub fn generate(name: &str, rules: Vec<Rule>, defaults: Vec<Ident>) -> Tokens {
    rule_enum(&rules, &defaults)
}

fn rule_enum(rules: &Vec<Rule>, defaults: &Vec<Ident>) -> Tokens {
    let rules = rules.iter().map(|rule| &rule.name).chain(defaults.iter());

    quote! {
        #[allow(dead_code, non_camel_case_types)]
        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum Rule {
            #( #rules ),*
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rule_enum_simple() {
        let rules = vec![
            Rule {
                name: Ident::new("f"),
                ty: RuleType::Normal,
                expr: Expr::Ident(Ident::new("g"))
            }
        ];

        assert_eq!(rule_enum(&rules, &vec![Ident::new("any")]), quote! {
            #[allow(dead_code, non_camel_case_types)]
            #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            pub enum Rule {
                f,
                any
            }
        });
    }
}
