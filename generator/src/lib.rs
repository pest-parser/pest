// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#![doc(html_root_url = "https://docs.rs/pest_derive")]
#![recursion_limit = "256"]

extern crate pest;
extern crate pest_meta;

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate quote;
extern crate syn;

use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

use proc_macro2::TokenStream;
use syn::{Attribute, DeriveInput, Generics, Ident, Lit, Meta};

#[macro_use]
mod macros;
mod generator;

use pest_meta::parser::{self, Rule};
use pest_meta::{optimizer, unwrap_or_report, validator};

pub fn derive_parser(input: TokenStream, include_grammar: bool) -> TokenStream {
    let ast: DeriveInput = syn::parse2(input).unwrap();
    let (name, generics, content) = parse_derive(ast);

    let (data, path) = match content {
        GrammarSource::File(ref path) => {
            let root = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
            let path = Path::new(&root).join("src/").join(&path);
            let file_name = match path.file_name() {
                Some(file_name) => file_name,
                None => panic!("grammar attribute should point to a file"),
            };

            let data = match read_file(&path) {
                Ok(data) => data,
                Err(error) => panic!("error opening {:?}: {}", file_name, error),
            };
            (data, Some(path.clone()))
        }
        GrammarSource::Inline(content) => (content, None),
    };

    let pairs = match parser::parse(Rule::grammar_rules, &data) {
        Ok(pairs) => pairs,
        Err(error) => panic!(
            "error parsing \n{}",
            error.renamed_rules(|rule| match *rule {
                Rule::grammar_rule => "rule".to_owned(),
                Rule::_push => "PUSH".to_owned(),
                Rule::assignment_operator => "`=`".to_owned(),
                Rule::silent_modifier => "`_`".to_owned(),
                Rule::atomic_modifier => "`@`".to_owned(),
                Rule::compound_atomic_modifier => "`$`".to_owned(),
                Rule::non_atomic_modifier => "`!`".to_owned(),
                Rule::opening_brace => "`{`".to_owned(),
                Rule::closing_brace => "`}`".to_owned(),
                Rule::opening_brack => "`[`".to_owned(),
                Rule::closing_brack => "`]`".to_owned(),
                Rule::opening_paren => "`(`".to_owned(),
                Rule::positive_predicate_operator => "`&`".to_owned(),
                Rule::negative_predicate_operator => "`!`".to_owned(),
                Rule::sequence_operator => "`&`".to_owned(),
                Rule::choice_operator => "`|`".to_owned(),
                Rule::optional_operator => "`?`".to_owned(),
                Rule::repeat_operator => "`*`".to_owned(),
                Rule::repeat_once_operator => "`+`".to_owned(),
                Rule::comma => "`,`".to_owned(),
                Rule::closing_paren => "`)`".to_owned(),
                Rule::quote => "`\"`".to_owned(),
                Rule::insensitive_string => "`^`".to_owned(),
                Rule::range_operator => "`..`".to_owned(),
                Rule::single_quote => "`'`".to_owned(),
                other_rule => format!("{:?}", other_rule),
            })
        ),
    };

    let defaults = unwrap_or_report(validator::validate_pairs(pairs.clone()));
    let ast = unwrap_or_report(parser::consume_rules(pairs));
    let optimized = optimizer::optimize(ast);

    generator::generate(name, &generics, path, optimized, defaults, include_grammar)
}

fn read_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let mut file = File::open(path.as_ref())?;
    let mut string = String::new();
    file.read_to_string(&mut string)?;
    Ok(string)
}

#[derive(Debug, PartialEq)]
enum GrammarSource {
    File(String),
    Inline(String),
}

fn parse_derive(ast: DeriveInput) -> (Ident, Generics, GrammarSource) {
    let name = ast.ident;
    let generics = ast.generics;

    let grammar: Vec<&Attribute> = ast
        .attrs
        .iter()
        .filter(|attr| match attr.parse_meta() {
            Ok(Meta::NameValue(name_value)) => {
                name_value.path.is_ident("grammar") || name_value.path.is_ident("grammar_inline")
            }
            _ => false,
        })
        .collect();

    let argument = match grammar.len() {
        0 => panic!("a grammar file needs to be provided with the #[grammar = \"PATH\"] or #[grammar_inline = \"GRAMMAR CONTENTS\"] attribute"),
        1 => get_attribute(grammar[0]),
        _ => panic!("only 1 grammar file can be provided"),
    };

    (name, generics, argument)
}

fn get_attribute(attr: &Attribute) -> GrammarSource {
    match attr.parse_meta() {
        Ok(Meta::NameValue(name_value)) => match name_value.lit {
            Lit::Str(string) => {
                if name_value.path.is_ident("grammar") {
                    GrammarSource::File(string.value())
                } else {
                    GrammarSource::Inline(string.value())
                }
            }
            _ => panic!("grammar attribute must be a string"),
        },
        _ => panic!("grammar attribute must be of the form `grammar = \"...\"`"),
    }
}

#[cfg(test)]
mod tests {
    use super::parse_derive;
    use super::GrammarSource;
    use syn;

    #[test]
    fn derive_inline_file() {
        let definition = "
            #[other_attr]
            #[grammar_inline = \"GRAMMAR\"]
            pub struct MyParser<'a, T>;
        ";
        let ast = syn::parse_str(definition).unwrap();
        let (_, _, filename) = parse_derive(ast);
        assert_eq!(filename, GrammarSource::Inline("GRAMMAR".to_string()));
    }

    #[test]
    fn derive_ok() {
        let definition = "
            #[other_attr]
            #[grammar = \"myfile.pest\"]
            pub struct MyParser<'a, T>;
        ";
        let ast = syn::parse_str(definition).unwrap();
        let (_, _, filename) = parse_derive(ast);
        assert_eq!(filename, GrammarSource::File("myfile.pest".to_string()));
    }

    #[test]
    #[should_panic(expected = "only 1 grammar file can be provided")]
    fn derive_multiple_grammars() {
        let definition = "
            #[other_attr]
            #[grammar = \"myfile1.pest\"]
            #[grammar = \"myfile2.pest\"]
            pub struct MyParser<'a, T>;
        ";
        let ast = syn::parse_str(definition).unwrap();
        parse_derive(ast);
    }

    #[test]
    #[should_panic(expected = "grammar attribute must be a string")]
    fn derive_wrong_arg() {
        let definition = "
            #[other_attr]
            #[grammar = 1]
            pub struct MyParser<'a, T>;
        ";
        let ast = syn::parse_str(definition).unwrap();
        parse_derive(ast);
    }
}
