#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate pest_meta;
extern crate pest;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        pest::set_call_limit(25_000, true);
        let _ = pest_meta::parser::parse(pest_meta::parser::Rule::grammar_rules, s);
    }
});
