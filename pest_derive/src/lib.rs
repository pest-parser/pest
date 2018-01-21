// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! # pest. The Elegant Parser
//!
//! pest is a [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar) parser built with
//! *simplicity* and *speed* in mind.
//!
//! This crate works in conjunction with the [`pest` crate](https://docs.rs/pest) by
//! deriving a grammar implementation based on a provided grammar.
//!
//! ## `.pest` files
//!
//! Grammar definitions reside in custom `.pest` files located in the `src` directory. Their path is
//! relative to `src` and is specified between the `derive` attribute and empty `struct` that
//! `Parser` will be derived on.
//!
//! Because of a limitation in procedural macros, there is no way for Cargo to know that a module
//! needs to be recompiled based on the file that the procedural macro is opening. This leads to the
//! case where modifying a `.pest` file without touching the file where the `derive` is does not
//! recompile it if it already has a working binary in the cache. To avoid this issue, the grammar
//! file can be included in a dummy `const` definition while debugging.
//!
//! ```ignore
//! #[cfg(debug_assertions)]
//! const _GRAMMAR: &'static str = include_str!("path/to/my_grammar.pest"); // relative to this file
//!
//! #[derive(Parser)]
//! #[grammar = "path/to/my_grammar.pest"] // relative to src
//! struct MyParser;
//! ```
//!
//! ## Grammar
//!
//! A grammar is a series of rules separated by whitespace, possibly containing comments.
//!
//! ### Comments
//!
//! Comments start with `//` and end at the end of the line.
//!
//! ```ignore
//! // a comment
//! ```
//!
//! ### Rules
//!
//! Rules have the following form:
//!
//! ```ignore
//! name = optional_modifier { expression }
//! ```
//!
//! The name of the rule is formed from alphanumeric characters or `_` with the condition that the
//! first character is not a digit and is used to create token pairs. When the rule starts being
//! parsed, the starting part of the token is being produced, with the ending part being produced
//! when the rule finishes parsing.
//!
//! The following token pair notation `a(b(), c())` denotes the tokens: start `a`, start `b`, end
//! `b`, start `c`, end `c`, end `a`.
//!
//! #### Modifiers
//!
//! Modifiers are optional and can be one of `_`, `@`, `$`, or `!`. These modifiers change the
//! behavior of the rules.
//!
//! 1. Silent (`_`)
//!
//!     Silent rules do not create token pairs during parsing, nor are they error-reported.
//!
//!     ```ignore
//!     a = _{ "a" }
//!     b =  { a ~ "b" }
//!     ```
//!
//!     Parsing `"ab"` produces the token pair `b()`.
//!
//! 2. Atomic (`@`)
//!
//!     Atomic rules do not accept whitespace or comments within their expressions and have a
//!     cascading effect on any rule they call. I.e. rules that are not atomic but are called by atomic
//!     rules behave atomically.
//!
//!     Any rules called by atomic rules do not generate token pairs.
//!
//!     ```ignore
//!     a =  { "a" }
//!     b = @{ a ~ "b" }
//!
//!     whitespace = _{ " " }
//!     ```
//!
//!     Parsing `"ab"` produces the token pair `b()`, while `"a   b"` produces an error.
//!
//! 3. Compound-atomic (`$`)
//!
//!     Compound-atomic are identical to atomic rules with the exception that rules called by them are
//!     not forbidden from generating token pairs.
//!
//!     ```ignore
//!     a =  { "a" }
//!     b = ${ a ~ "b" }
//!
//!     whitespace = _{ " " }
//!     ```
//!
//!     Parsing `"ab"` produces the token pairs `b(a())`, while `"a   b"` produces an error.
//!
//! 4. Non-atomic (`!`)
//!
//!     Non-atomic are identical to normal rules with the exception that they stop the cascading effect
//!     of atomic and compound-atomic rules.
//!
//!     ```ignore
//!     a =  { "a" }
//!     b = !{ a ~ "b" }
//!     c = @{ b }
//!
//!     whitespace = _{ " " }
//!     ```
//!
//!     Parsing both `"ab"` and `"a   b"` produce the token pairs `c(a())`.
//!
//! #### Expressions
//!
//! Expressions can be either terminals or non-terminals.
//!
//! 1. Terminals
//!
//!     | Terminal   | Usage                                                          |
//!     |------------|----------------------------------------------------------------|
//!     | `"a"`      | matches the exact string `"a"`                                 |
//!     | `^"a"`     | matches the exact string `"a"` case insensitively (ASCII only) |
//!     | `'a'..'z'` | matches one character between `'a'` and `'z'`                  |
//!     | `a`        | matches rule `a`                                               |
//!
//! Strings and characters follow
//! [Rust's escape mechanisms](https://doc.rust-lang.org/reference/tokens.html#byte-escapes), while
//! identifiers can contain alpha-numeric characters and underscores (`_`), as long as they do not
//! start with a digit.
//!
//! 2. Non-terminals
//!
//!     | Non-terminal | Usage                                                      |
//!     |--------------|------------------------------------------------------------|
//!     | `(e)`        | matches `e`                                                |
//!     | `e1 ~ e2`    | matches the sequence `e1` `e2`                             |
//!     | `e1 | e2`    | matches either `e1` or `e2`                                |
//!     | `e*`         | matches `e` zero or more times                             |
//!     | `e+`         | matches `e` one or more times                              |
//!     | `e{n}`       | matches `e` exactly `n` times                              |
//!     | `e{, n}`     | matches `e` at most `n` times                              |
//!     | `e{n,} `     | matches `e` at least `n` times                             |
//!     | `e{m, n}`    | matches `e` between `m` and `n` times inclusively          |
//!     | `e?`         | optionally matches `e`                                     |
//!     | `&e`         | matches `e` without making progress                        |
//!     | `!e`         | matches if `e` doesn't match without making progress       |
//!     | `push(e)`    | matches `e` and pushes it's captured string down the stack |
//!
//!     where `e`, `e1`, and `e2` are expressions.
//!
//! ## Special rules
//!
//! Special rules can be called within the grammar. They are:
//!
//! * `whitespace` - gets run between rules and sub-rules
//! * `comment` - gets run between rules and sub-rules
//! * `any` - matches exactly one `char`
//! * `soi` - (start-of-input) matches only when a `Parser` is still at the starting position
//! * `eoi` - (end-of-input) matches only when a `Parser` has reached its end
//! * `pop` - pops a string from the stack and matches it
//! * `peek` - peeks a string from the stack and matches it
//!
//! `whitespace` and `comment` should be defined manually if needed. All other rules cannot be
//! overridden.
//!
//! ## `whitespace` and `comment`
//!
//! When defined, these rules get matched automatically in sequences (`~`) and repetitions
//! (`*`, `+`) between expressions. Atomic rules and those rules called by atomic rules are exempt
//! from this behavior.
//!
//! These rules should be defined so as to match one whitespace character and one comment only since
//! they are run in repetitions.
//!
//! If both `whitespace` and `comment` are defined, this grammar:
//!
//! ```ignore
//! a = { b ~ c }
//! ```
//!
//! is effectively transformed into this one behind the scenes:
//!
//! ```ignore
//! a = { b ~ whitespace* ~ (comment ~ whitespace*)* ~ c }
//! ```
//!
//! ## `push`, `pop`, and `peek`
//!
//! `push(e)` simply pushes the captured string of the expression `e` down a stack. This stack can
//! then later be used to match grammar based on its content with `pop` and `peek`.
//!
//! `peek` always matches the string at the top of stack. So, if the stack contains `["a", "b"]`,
//! the this grammar:
//!
//! ```ignore
//! a = { peek }
//! ```
//!
//! is effectively transformed into at parse time:
//!
//! ```ignore
//! a = { "a" }
//! ```
//!
//! `pop` works the same way with the exception that it pops the string off of the stack if the
//! the match worked. With the stack from above, if `pop` matches `"a"`, the stack will be mutated
//! to `["b"]`.
//!
//! ## `Rule`
//!
//! All rules defined or used in the grammar populate a generated `enum` called `Rule`. This
//! implements `pest`'s `RuleType` and can be used throughout the API.

#![doc(html_root_url = "https://docs.rs/pest_derive")]
#![recursion_limit = "256"]

#[cfg(test)]
#[macro_use]
extern crate pest;
#[cfg(not(test))]
extern crate pest;

extern crate proc_macro;
#[macro_use]
extern crate quote;
extern crate syn;

use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

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

    let data = match read_file(&path) {
        Ok(data) => data,
        Err(error) => panic!("error opening {:?}: {}", file_name, error)
    };

    let input = &data;
    let pairs = match GrammarParser::parse(GrammarRule::grammar_rules, input) {
        Ok(pairs) => pairs,
        Err(error) => panic!(
            "error parsing {:?}\n\n{}",
            file_name,
            error.renamed_rules(|rule| match *rule {
                GrammarRule::grammar_rule => "rule".to_owned(),
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
                GrammarRule::comma => "`,`".to_owned(),
                GrammarRule::closing_paren => "`)`".to_owned(),
                GrammarRule::quote => "`\"`".to_owned(),
                GrammarRule::insensitive_string => "`^`".to_owned(),
                GrammarRule::range_operator => "`..`".to_owned(),
                GrammarRule::single_quote => "`'`".to_owned(),
                other_rule => format!("{:?}", other_rule)
            })
        )
    };

    let (ast, defaults) = parser::consume_rules(pairs);
    let optimized = optimizer::optimize(ast);
    let generated = generator::generate(name, optimized, defaults);

    generated.as_ref().parse().unwrap()
}

fn read_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let mut file = File::open(path.as_ref())?;
    let mut string = String::new();
    file.read_to_string(&mut string)?;
    Ok(string)
}

fn parse_derive(source: String) -> (Ident, String) {
    let ast = syn::parse_derive_input(&source).unwrap();
    let name = Ident::new(ast.ident.as_ref());

    let grammar: Vec<_> = ast.attrs
        .iter()
        .filter(|attr| match attr.value {
            MetaItem::NameValue(ref ident, _) => format!("{}", ident) == "grammar",
            _ => false
        })
        .collect();

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
        let definition = "
            #[other_attr]
            #[grammar = \"myfile.pest\"]
            pub struct MyParser<'a, T>;
        ";
        let (_, filename) = parse_derive(definition.to_owned());

        assert_eq!(filename, "myfile.pest");
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
        parse_derive(definition.to_owned());
    }

    #[test]
    #[should_panic(expected = "grammar attribute must be a string")]
    fn derive_wrong_arg() {
        let definition = "
            #[other_attr]
            #[grammar = 1]
            pub struct MyParser<'a, T>;
        ";
        parse_derive(definition.to_owned());
    }
}
