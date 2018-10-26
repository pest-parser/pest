#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate pest_grammars;

fuzz_target!(|data: &[u8]| {
    use pest_grammars::json;
    use pest_grammars::Parser;

    if let Ok(s) = std::str::from_utf8(data) {
        let _ = json::JsonParser::parse(json::Rule::json, s);
    }
});
