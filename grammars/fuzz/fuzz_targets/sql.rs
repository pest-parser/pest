#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate pest;
extern crate pest_grammars;

fuzz_target!(|data: &[u8]| {
    use pest_grammars::sql;
    use pest_grammars::Parser;

    if let Ok(s) = std::str::from_utf8(data) {
        let _ = sql::SqlParser::parse(sql::Rule::SQL, s);
    }
});
