#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate pest;
extern crate pest_grammars;

fuzz_target!(|data: &[u8]| {
    use pest_grammars::http;
    use pest_grammars::Parser;

    if let Ok(s) = std::str::from_utf8(data) {
        let _ = http::HttpParser::parse(http::Rule::http, s);
    }
});
