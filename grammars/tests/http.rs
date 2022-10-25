#[macro_use]
extern crate pest;
use pest_grammars::{http::*, Parser};

#[test]
fn method() {
    parses_to! {
        parser: HttpParser,
        input: "GET",
        rule: Rule::method,
        tokens: [
            method(0, 3)
        ]
    };
}

#[test]
fn uri() {
    parses_to! {
        parser: HttpParser,
        input: "/",
        rule: Rule::uri,
        tokens: [
            uri(0, 1)
        ]
    };
}

#[test]
fn version() {
    parses_to! {
        parser: HttpParser,
        input: "1.1",
        rule: Rule::version,
        tokens: [
            version(0, 3)
        ]
    };
}

#[test]
fn header() {
    parses_to! {
        parser: HttpParser,
        input: "Connection: keep-alive\n",
        rule: Rule::header,
        tokens: [
            header(0, 23, [
                header_name(0, 10),
                header_value(12, 22),
            ])
        ]
    };
}

#[test]
fn examples() {
    let data = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/examples.http"));
    assert!(HttpParser::parse(Rule::http, data).is_ok());
}
