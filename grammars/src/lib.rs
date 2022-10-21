// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! # pest grammars
//!
//! Contains a series of default grammars.

#![doc(html_root_url = "https://docs.rs/pest_grammars")]

extern crate pest;
#[macro_use]
extern crate pest_derive;

pub use pest::Parser;

pub mod http {
    /// HTTP parser.
    #[derive(Parser)]
    #[grammar = "grammars/http.pest"]
    pub struct HttpParser;
}

pub mod json {
    /// JSON parser.
    #[derive(Parser)]
    #[grammar = "grammars/json.pest"]
    pub struct JsonParser;
}

pub mod toml {
    /// TOML parser.
    #[derive(Parser)]
    #[grammar = "grammars/toml.pest"]
    pub struct TomlParser;
}

#[cfg(test)]
mod tests {
    use std::convert::TryInto;

    use pest::Parser;

    use crate::{json, toml};

    fn test_toml_deep_nesting(input: &str) {
        const ERROR: &str = "call limit reached";
        pest::set_call_limit(Some(5_000usize.try_into().unwrap()));
        let s = toml::TomlParser::parse(toml::Rule::toml, input);
        assert!(s.is_err());
        assert_eq!(s.unwrap_err().variant.message(), ERROR);
    }

    #[test]
    fn toml_handles_deep_nesting() {
        let sample1 = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/resources/test/tomlfuzzsample1.toml"
        ));
        test_toml_deep_nesting(sample1);
    }

    #[test]
    #[ignore = "this sometimes crashes in the debug mode"]
    fn toml_handles_deep_nesting_unstable() {
        let sample2 = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/resources/test/tomlfuzzsample2.toml"
        ));
        test_toml_deep_nesting(sample2);
    }

    #[test]
    fn json_handles_deep_nesting() {
        let sample1 = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/resources/test/jsonfuzzsample1.json"
        ));
        const ERROR: &str = "call limit reached";
        pest::set_call_limit(Some(45_000usize.try_into().unwrap()));
        let s1 = json::JsonParser::parse(json::Rule::json, sample1);
        assert!(s1.is_err());
        assert_eq!(s1.unwrap_err().variant.message(), ERROR);
    }
}
