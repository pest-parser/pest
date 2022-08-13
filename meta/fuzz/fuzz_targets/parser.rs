#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate pest_meta;
extern crate pest;

use std::convert::TryInto;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        pest::set_call_limit(Some(5_000usize.try_into().unwrap()));
        let _ = pest_meta::parser::parse(pest_meta::parser::Rule::grammar_rules, s);
    }
});
