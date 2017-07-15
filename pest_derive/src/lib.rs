// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![doc(html_root_url = "https://docs.rs/pest_derive/1.0.0")]
#![recursion_limit="256"]

#[macro_use]
extern crate pest;
extern crate proc_macro;
#[macro_use]
extern crate quote;
extern crate syn;

use std::env;
use std::path::Path;
use std::rc::Rc;

use pest::inputs::FileInput;
use pest::Parser;
use proc_macro::TokenStream;
use quote::Ident;
use syn::{Attribute, Lit, MetaItem};

mod ast;
mod generator;
mod optimizer;
mod parser;
mod validator;

use parser::{GrammarParser, GrammarRule};

#[proc_macro_derive(Parser, attributes(grammar))]
pub fn derive_parser(input: TokenStream) -> TokenStream {
    let source = input.to_string();

    let (name, path) = parse_derive(source);

    let root = env::var("CARGO_MANIFEST_DIR").unwrap_or(".".into());
    let path = Path::new(&root).join("src/").join(&path);
    let file_name = match path.file_name() {
        Some(file_name) => file_name,
        None => panic!("grammar attribute should point to a file")
    };

    let input = match FileInput::new(&path) {
        Ok(input) => Rc::new(input),
        Err(error) => panic!("error opening {:?}: {}", file_name, error)
    };
    let pairs = match GrammarParser::parse(GrammarRule::grammar_rules, input) {
        Ok(pairs) => pairs,
        Err(error) => panic!("error parsing {:?}\n\n{}", file_name, error.renamed_rules(|rule| {
            match *rule {
                GrammarRule::eoi => "end-of-input".to_owned(),
                GrammarRule::assignment_operator => "`=`".to_owned(),
                GrammarRule::silent_modifier => "`_`".to_owned(),
                GrammarRule::atomic_modifier => "`@`".to_owned(),
                GrammarRule::compound_atomic_modifier => "`$`".to_owned(),
                GrammarRule::non_atomic_modifier => "`!`".to_owned(),
                GrammarRule::opening_brace => "`{`".to_owned(),
                GrammarRule::closing_brace => "`}`".to_owned(),
                GrammarRule::opening_paren => "`(`".to_owned(),
                GrammarRule::positive_predicate_operator => "`&`".to_owned(),
                GrammarRule::negative_predicate_operator => "`!`".to_owned(),
                GrammarRule::sequence_operator => "`&`".to_owned(),
                GrammarRule::choice_operator => "`|`".to_owned(),
                GrammarRule::optional_operator => "`?`".to_owned(),
                GrammarRule::repeat_operator => "`*`".to_owned(),
                GrammarRule::repeat_once_operator => "`+`".to_owned(),
                GrammarRule::closing_paren => "`)`".to_owned(),
                GrammarRule::quote => "`\"`".to_owned(),
                GrammarRule::insensitive_string => "`^`".to_owned(),
                GrammarRule::range_operator => "`..`".to_owned(),
                GrammarRule::single_quote => "`'`".to_owned(),
                other_rule => format!("{:?}", other_rule)
            }
        }))
    };

    let (ast, defaults) = parser::consume_rules(pairs);
    let optimized = optimizer::optimize(ast);
    let generated = generator::generate(name, optimized, defaults);

    generated.as_ref().parse().unwrap()
}

fn parse_derive(source: String) -> (Ident, String) {
    let ast = syn::parse_macro_input(&source).unwrap();
    let name = Ident::new(ast.ident.as_ref());

    let grammar: Vec<_> = ast.attrs.iter().filter(|attr| {
        match attr.value {
            MetaItem::NameValue(ref ident, _) => format!("{}", ident) == "grammar",
            _ => false
        }
    }).collect();

    let filename = match grammar.len() {
        0 => panic!("a grammar file needs to be provided with the #[grammar(\"...\")] attribute"),
        1 => get_filename(grammar[0]),
        _ => panic!("only 1 grammar file can be provided")
    };

    (name, filename)
}

fn get_filename(attr: &Attribute) -> String {
    if let MetaItem::NameValue(_, ref lit) = attr.value {
        if let &Lit::Str(ref string, _) = lit {
            string.clone()
        } else {
            panic!("grammar attribute must be a string")
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
        let (_, filename) = parse_derive("
            #[other_attr]
            #[grammar = \"myfile.pest\"]
            pub struct MyParser<'a, T>;
        ".to_owned());

        assert_eq!(filename, "myfile.pest");
    }

    #[test]
    #[should_panic(expected = "only 1 grammar file can be provided")]
    fn derive_multiple_grammars() {
        parse_derive("
            #[other_attr]
            #[grammar = \"myfile1.pest\"]
            #[grammar = \"myfile2.pest\"]
            pub struct MyParser<'a, T>;
        ".to_owned());
    }

    #[test]
    #[should_panic(expected = "grammar attribute must be a string")]
    fn derive_wrong_arg() {
        parse_derive("
            #[other_attr]
            #[grammar = 1]
            pub struct MyParser<'a, T>;
        ".to_owned());
    }
}
