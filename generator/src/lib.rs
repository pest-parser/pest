// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#![doc(
    html_root_url = "https://docs.rs/pest_derive",
    html_logo_url = "https://raw.githubusercontent.com/pest-parser/pest/master/pest-logo.svg",
    html_favicon_url = "https://raw.githubusercontent.com/pest-parser/pest/master/pest-logo.svg"
)]
#![warn(missing_docs, rust_2018_idioms, unused_qualifications)]
#![recursion_limit = "256"]
//! # pest generator
//!
//! This crate generates code from ASTs (which is used in the `pest_derive` crate).

#[macro_use]
extern crate quote;

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

use pest::iterators::Pairs;
use proc_macro2::TokenStream;
use syn::{Attribute, DeriveInput, Generics, Ident, Lit, Meta};

#[macro_use]
mod macros;
mod generator;

use pest_meta::parser::{self, rename_meta_rule, Rule};
use pest_meta::{optimizer, unwrap_or_report, validator};

use generator::DocComment;

/// Processes the derive/proc macro input and generates the corresponding parser based
/// on the parsed grammar. If `include_grammar` is set to true, it'll generate an explicit
/// "include_str" statement (done in pest_derive, but turned off in the local bootstrap).
pub fn derive_parser(input: TokenStream, include_grammar: bool) -> TokenStream {
    let ast: DeriveInput = syn::parse2(input).unwrap();
    let (name, generics, contents) = parse_derive(ast);

    let mut data = String::new();
    let mut path = None;

    for content in contents {
        let (_data, _path) = match content {
            GrammarSource::File(ref path) => {
                let root = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());

                // Check whether we can find a file at the path relative to the CARGO_MANIFEST_DIR
                // first.
                //
                // If we cannot find the expected file over there, fallback to the
                // `CARGO_MANIFEST_DIR/src`, which is the old default and kept for convenience
                // reasons.
                // TODO: This could be refactored once `std::path::absolute()` get's stabilized.
                // https://doc.rust-lang.org/std/path/fn.absolute.html
                let path = if Path::new(&root).join(path).exists() {
                    Path::new(&root).join(path)
                } else {
                    Path::new(&root).join("src/").join(path)
                };

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

        data.push_str(&_data);
        if _path.is_some() {
            path = _path;
        }
    }

    let pairs = match parser::parse(Rule::grammar_rules, &data) {
        Ok(pairs) => pairs,
        Err(error) => panic!("error parsing \n{}", error.renamed_rules(rename_meta_rule)),
    };

    let grammar_doc = consume_grammar_doc(pairs.clone());
    let line_docs = consume_line_docs(pairs.clone());

    let defaults = unwrap_or_report(validator::validate_pairs(pairs.clone()));
    let ast = unwrap_or_report(parser::consume_rules(pairs));
    let optimized = optimizer::optimize(ast);

    let doc_comment = &DocComment::new(grammar_doc, line_docs);

    generator::generate(
        name,
        &generics,
        path,
        optimized,
        defaults,
        doc_comment,
        include_grammar,
    )
}

/// Consume grammar doc into String, multi-line joined with `\n`
fn consume_grammar_doc(pairs: Pairs<'_, Rule>) -> String {
    let mut docs = vec![];
    for pair in pairs {
        if pair.as_rule() == Rule::grammar_doc {
            let inner_doc = pair.into_inner().next().unwrap();
            docs.push(inner_doc.as_str());
        }
    }

    docs.join("\n")
}

/// Consume line docs into HashMap<rule_name, doc>
///
/// Example a `test.pest`:
///
/// ```ignore
/// /// Line doc 1
/// foo = {}
///
/// /// Line doc 2
/// /// Line doc 3
/// bar = {}
/// ```
///
/// Will returns `{ "foo":  "This is line comment", "bar": "Line doc 2\n/// Line doc 3" }`
fn consume_line_docs(pairs: Pairs<'_, Rule>) -> HashMap<String, String> {
    let mut docs: HashMap<String, String> = HashMap::new();
    let mut comments = vec![];

    for pair in pairs {
        let rule = pair.as_rule();

        if rule == Rule::grammar_rule {
            if let Some(inner) = pair.into_inner().next() {
                // grammar_rule > line_doc | identifier
                match inner.as_rule() {
                    Rule::line_doc => {
                        if let Some(inner_doc) = inner.into_inner().next() {
                            comments.push(inner_doc.as_str())
                        }
                    }
                    Rule::identifier => {
                        if !comments.is_empty() {
                            let rule_name = inner.as_str().to_owned();
                            docs.insert(rule_name, comments.join("\n"));
                            comments = vec![];
                        }
                    }
                    _ => (),
                }
            }
        }
    }

    docs
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

fn parse_derive(ast: DeriveInput) -> (Ident, Generics, Vec<GrammarSource>) {
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

    if grammar.is_empty() {
        panic!("a grammar file needs to be provided with the #[grammar = \"PATH\"] or #[grammar_inline = \"GRAMMAR CONTENTS\"] attribute");
    }

    let mut grammar_sources = Vec::with_capacity(grammar.len());
    for attr in grammar {
        grammar_sources.push(get_attribute(attr))
    }

    (name, generics, grammar_sources)
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
    use std::collections::HashMap;

    use super::consume_line_docs;
    use super::parse_derive;
    use super::GrammarSource;

    use pest_meta::parser::{self, Rule};

    #[test]
    fn derive_inline_file() {
        let definition = "
            #[other_attr]
            #[grammar_inline = \"GRAMMAR\"]
            pub struct MyParser<'a, T>;
        ";
        let ast = syn::parse_str(definition).unwrap();
        let (_, _, filenames) = parse_derive(ast);
        assert_eq!(filenames, [GrammarSource::Inline("GRAMMAR".to_string())]);
    }

    #[test]
    fn derive_ok() {
        let definition = "
            #[other_attr]
            #[grammar = \"myfile.pest\"]
            pub struct MyParser<'a, T>;
        ";
        let ast = syn::parse_str(definition).unwrap();
        let (_, _, filenames) = parse_derive(ast);
        assert_eq!(filenames, [GrammarSource::File("myfile.pest".to_string())]);
    }

    #[test]
    fn derive_multiple_grammars() {
        let definition = "
            #[other_attr]
            #[grammar = \"myfile1.pest\"]
            #[grammar = \"myfile2.pest\"]
            pub struct MyParser<'a, T>;
        ";
        let ast = syn::parse_str(definition).unwrap();
        let (_, _, filenames) = parse_derive(ast);
        assert_eq!(
            filenames,
            [
                GrammarSource::File("myfile1.pest".to_string()),
                GrammarSource::File("myfile2.pest".to_string())
            ]
        );
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

    #[test]
    #[should_panic(
        expected = "a grammar file needs to be provided with the #[grammar = \"PATH\"] or #[grammar_inline = \"GRAMMAR CONTENTS\"] attribute"
    )]
    fn derive_no_grammar() {
        let definition = "
            #[other_attr]
            pub struct MyParser<'a, T>;
        ";
        let ast = syn::parse_str(definition).unwrap();
        parse_derive(ast);
    }

    #[test]
    fn test_consume_line_docs() {
        let pairs = match parser::parse(Rule::grammar_rules, include_str!("../tests/test.pest")) {
            Ok(pairs) => pairs,
            Err(_) => panic!("error parsing tests/test.pest"),
        };

        let line_docs = consume_line_docs(pairs);

        let mut expected = HashMap::new();
        expected.insert("foo".to_owned(), "Matches foo str, e.g.: `foo`".to_owned());
        expected.insert(
            "bar".to_owned(),
            "Matches bar str,\n  Indent 2, e.g: `bar` or `foobar`".to_owned(),
        );
        expected.insert(
            "dar".to_owned(),
            "Matches dar\nMatch dar description".to_owned(),
        );
        assert_eq!(expected, line_docs);
    }

    #[test]
    fn test_generate_doc() {
        let input = quote! {
            #[derive(Parser)]
            #[grammar = "../tests/test.pest"]
            pub struct TestParser;
        };

        let token = super::derive_parser(input, true);

        let expected = quote! {
            #[doc = "A parser for JSON file.\nAnd this is a example for JSON parser.\n\n    indent-4-space"]
            #[allow(dead_code, non_camel_case_types, clippy::upper_case_acronyms)]
            #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]

            pub enum Rule {
                #[doc = "Matches foo str, e.g.: `foo`"]
                r#foo,
                #[doc = "Matches bar str,\n  Indent 2, e.g: `bar` or `foobar`"]
                r#bar,
                r#bar1,
                #[doc = "Matches dar\nMatch dar description"]
                r#dar
            }
        };

        assert!(
            token.to_string().contains(expected.to_string().as_str()),
            "{}\n\nExpected to contains:\n{}",
            token,
            expected
        );
    }
}
