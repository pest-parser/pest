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

use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

use proc_macro2::TokenStream;
use syn::{Attribute, DeriveInput, Generics, Ident, Lit, Meta};

#[macro_use]
mod macros;
mod generator;

use pest_meta::parser::{self, rename_meta_rule, Rule};
use pest_meta::{optimizer, unwrap_or_report, validator};

fn join_path(path: &str) -> PathBuf {
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

    path
}

/// Get path relative to `path` dir, or relative to root path
fn partial_path(path: Option<&PathBuf>, filename: &str) -> PathBuf {
    let root = match path {
        Some(path) => path.parent().unwrap().to_path_buf(),
        None => join_path("./"),
    };

    // Add .pest suffix if not exist
    let mut filename = filename.to_string();
    if !filename.to_lowercase().ends_with(".pest") {
        filename.push_str(".pest");
    }

    root.join(filename)
}

/// Processes the derive/proc macro input and generates the corresponding parser based
/// on the parsed grammar. If `include_grammar` is set to true, it'll generate an explicit
/// "include_str" statement (done in pest_derive, but turned off in the local bootstrap).
pub fn derive_parser(input: TokenStream, include_grammar: bool) -> TokenStream {
    let ast: DeriveInput = syn::parse2(input).unwrap();
    let (name, generics, contents) = parse_derive(ast);

    let mut data = String::new();
    let mut path = None;
    let mut has_use = false;

    for content in contents {
        let (_data, _path) = match content {
            GrammarSource::File(ref path) => {
                let path = join_path(path);

                let data = match read_file(&path) {
                    Ok(data) => data,
                    Err(error) => panic!("error opening {:?}: {}", path, error),
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

    let raw_data = data.clone();
    let mut pairs = match parser::parse(Rule::grammar_rules, &raw_data) {
        Ok(pairs) => pairs,
        Err(error) => panic!("error parsing \n{}", error.renamed_rules(rename_meta_rule)),
    };

    // parse `include!("file.pest")` to load partial and replace data
    // TODO: Try to avoid parse twice as much as possible
    let mut partial_pairs = pairs.clone().flatten().peekable();
    while let Some(pair) = partial_pairs.next() {
        if pair.as_rule() == Rule::include {
            if let Some(filename) = partial_pairs.peek() {
                let filepath = partial_path(path.as_ref(), filename.as_str());
                let partial_data = match read_file(&filepath) {
                    Ok(data) => data,
                    Err(error) => panic!("error opening {:?}: {}", filepath, error),
                };

                let (start, end) = (pair.as_span().start(), pair.as_span().end());

                data.replace_range(start..end, &partial_data);
                has_use = true;
            }
        }
    }

    if has_use {
        // Re-parse the data again, after replacing the `use` statement
        pairs = match parser::parse(Rule::grammar_rules, &data) {
            Ok(pairs) => pairs,
            Err(error) => panic!("error parsing \n{}", error.renamed_rules(rename_meta_rule)),
        };
    }

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
    use super::parse_derive;
    use super::GrammarSource;
    use std::path::PathBuf;

    use crate::partial_path;

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
    fn test_partial_path() {
        assert_eq!(
            PathBuf::from("tests/grammars/base.pest".to_owned()),
            partial_path(Some(&PathBuf::from("tests/grammars/foo.pest")), "base")
        );

        assert_eq!(
            PathBuf::from("tests/grammars/base.pest".to_owned()),
            partial_path(Some(&PathBuf::from("tests/grammars/foo.pest")), "base.pest")
        );

        let root = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
        assert_eq!(
            std::path::Path::new(&root).join("base.pest"),
            partial_path(None, "base.pest")
        );
    }
}
