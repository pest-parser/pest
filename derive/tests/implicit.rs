extern crate pest;
extern crate pest_derive;

use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "../tests/implicit.pest"]
struct TestImplicitParser;

#[test]
fn test_implicit_whitespace() {
    // this failed to parse due to a bug in the optimizer
    // see: https://github.com/pest-parser/pest/issues/762#issuecomment-1375374868
    let successful_parse = TestImplicitParser::parse(Rule::program, "a a");
    assert!(successful_parse.is_ok());
}
