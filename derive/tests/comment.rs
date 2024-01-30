#![cfg_attr(not(feature = "std"), no_std)]
extern crate alloc;
#[cfg(not(feature = "std"))]
use alloc::{format, vec::Vec};

#[cfg(feature = "inner-trivia")]
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "../tests/comment.pest"]
pub struct CommentParser;

#[test]
#[cfg(feature = "inner-trivia")]
pub fn test_comment() {
    let result = CommentParser::parse(Rule::COMMENT, "// some comment\n");
    assert!(result.is_ok());
    let mut pairs = result.unwrap();
    let pair = pairs.next().unwrap();
    assert_eq!(pair.as_rule(), Rule::COMMENT);
    let mut inner = pair.into_inner();
    let comment = inner.next().unwrap();
    assert_eq!(comment.as_rule(), Rule::SingleLineComment);
}
