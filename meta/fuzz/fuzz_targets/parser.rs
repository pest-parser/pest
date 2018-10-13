#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate pest_meta;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = pest_meta::parser::parse(pest_meta::parser::Rule::grammar_rules, s);
    }
});
