#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate pest;
extern crate pest_grammars;

use std::convert::TryInto;

fuzz_target!(|data: &[u8]| {
    use pest_grammars::json;
    use pest_grammars::Parser;

    if let Ok(s) = std::str::from_utf8(data) {
        pest::set_call_limit(Some(8_000usize.try_into().unwrap()));
        let _ = json::JsonParser::parse(json::Rule::json, s);
    }
});
