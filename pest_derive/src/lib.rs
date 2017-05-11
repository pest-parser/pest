// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![doc(html_root_url = "https://docs.rs/pest_derive/1.0.0")]

extern crate proc_macro;
#[macro_use]
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use quote::{Tokens, ToTokens};
use syn::{Attribute, Lit, MetaItem, NestedMetaItem};

mod ast;
mod generator;
mod optimizer;
mod parser;
mod validator;

#[proc_macro_derive(Parser)]
pub fn derive_parser(input: TokenStream) -> TokenStream {
    let source = input.to_string();

    let (tokens, filename) = parse_derive(source);

    tokens.parse().unwrap()
}

fn parse_derive(source: String) -> (Tokens, String) {
    let ast = syn::parse_macro_input(&source).unwrap();

    let (grammar, attrs): (Vec<_>, Vec<_>) = ast.attrs.iter().partition(|attr| {
        match attr.value {
            MetaItem::List(ref ident, _) => format!("{}", ident) == "grammar",
            _ => false
        }
    });

    let filename = if grammar.len() == 1 {
        get_filename(grammar[0])
    } else {
        panic!("You can only supply 1 grammar file");
    };

    let mut tokens = Tokens::new();

    let mut ast = ast.clone();
    ast.attrs = attrs.into_iter().cloned().collect::<Vec<_>>();
    ast.to_tokens(&mut tokens);

    (tokens, filename)
}

fn get_filename(attr: &Attribute) -> String {
    if let MetaItem::List(_, ref items) = attr.value {
        if items.len() == 1 {
            let item = &items[0];

            if let &NestedMetaItem::Literal(ref lit) = item {
                if let &Lit::Str(ref str, _) = lit {
                    str.to_owned()
                } else {
                    panic!("grammar attribute takes as argument a string");
                }
            } else {
                panic!("grammar attribute takes as argument a string");
            }
        } else {
            panic!("grammar attribute takes only 1 argument");
        }
    } else {
        unreachable!();
    }
}

#[cfg(test)]
mod tests {
    use super::parse_derive;

    #[test]
    fn derive_ok() {
        let (tokens, filename) = parse_derive("
            #[other_attr]
            #[grammar(\"myfile.pest\")]
            pub struct MyParser<'a, T>;
        ".to_owned());

        assert_eq!(tokens.as_str(), "# [ other_attr ] pub struct MyParser < \'a , T > ;");
        assert_eq!(filename, "myfile.pest");
    }

    #[test]
    #[should_panic(expected = "You can only supply 1 grammar file")]
    fn derive_multiple_grammars() {
        let (tokens, filename) = parse_derive("
            #[other_attr]
            #[grammar(\"myfile1.pest\")]
            #[grammar(\"myfile2.pest\")]
            pub struct MyParser<'a, T>;
        ".to_owned());
    }

    #[test]
    #[should_panic(expected = "grammar attribute takes only 1 argument")]
    fn derive_multiple_args() {
        let (tokens, filename) = parse_derive("
            #[other_attr]
            #[grammar(\"myfile1.pest\", \"myfile2.pest\")]
            pub struct MyParser<'a, T>;
        ".to_owned());
    }

    #[test]
    #[should_panic(expected = "grammar attribute takes as argument a string")]
    fn derive_wrong_arg() {
        let (tokens, filename) = parse_derive("
            #[other_attr]
            #[grammar(1)]
            pub struct MyParser<'a, T>;
        ".to_owned());
    }
}
