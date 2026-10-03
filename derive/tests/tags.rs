// pest. The Elegant Parser
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#![cfg_attr(not(feature = "std"), no_std)]
extern crate alloc;
extern crate pest;
extern crate pest_derive;

#[cfg(feature = "grammar-extras")]
mod tags {
    use alloc::borrow::ToOwned;
    use alloc::string::String;
    use alloc::vec::Vec;
    use pest::Parser;
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "../tests/tags.pest"]
    struct TagsParser;

    /// Start position and tag of every `a` pair, in order.
    fn tags(rule: Rule, input: &str) -> Vec<(usize, Option<String>)> {
        TagsParser::parse(rule, input)
            .unwrap()
            .flatten()
            .filter(|pair| pair.as_rule() == Rule::a)
            .map(|pair| {
                (
                    pair.as_span().start(),
                    pair.as_node_tag().map(str::to_owned),
                )
            })
            .collect()
    }

    fn t(pos: usize) -> (usize, Option<String>) {
        (pos, Some("t".to_owned()))
    }

    #[test]
    fn repeat_tags_every_pair() {
        assert_eq!(tags(Rule::rep, "a a a."), [t(0), t(2), t(4)]);
    }

    #[test]
    fn repeat_tags_every_pair_atomic() {
        assert_eq!(tags(Rule::rep_atomic, "aaa."), [t(0), t(1), t(2)]);
    }

    #[test]
    fn repeat_once_tags_every_pair() {
        assert_eq!(tags(Rule::rep_once, "a a a."), [t(0), t(2), t(4)]);
    }

    #[test]
    fn empty_optional_tags_nothing() {
        assert_eq!(tags(Rule::optional, "a."), [(0, None)]);
        assert_eq!(tags(Rule::optional, "a a."), [(0, None), t(2)]);
    }

    #[test]
    fn literal_tags_nothing() {
        assert_eq!(tags(Rule::literal, "a q"), [(0, None)]);
    }

    #[test]
    fn lookahead_tags_nothing() {
        assert_eq!(tags(Rule::lookahead, "a a"), [(0, None), (2, None)]);
    }

    #[test]
    fn choice_tags_only_its_branch() {
        assert_eq!(tags(Rule::choice, "a q"), [(0, None)]);
        assert_eq!(tags(Rule::choice, "a a"), [(0, None), t(2)]);
    }

    #[test]
    fn sequence_tags_its_pairs() {
        assert_eq!(tags(Rule::sequence, "a a a"), [t(0), t(2), (4, None)]);
    }

    #[test]
    fn silent_rule_tag_does_not_leak() {
        assert_eq!(tags(Rule::silent_leak, "a q"), [(0, None)]);
    }

    #[test]
    fn empty_optional_does_not_retag() {
        assert_eq!(tags(Rule::prefix, "a"), [(0, Some("p".to_owned()))]);
    }
}
