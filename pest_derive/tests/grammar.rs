// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[macro_use]
extern crate pest;
#[macro_use]
extern crate pest_derive;

#[derive(Parser)]
#[grammar = "../tests/grammar.pest"]
struct GrammarParser;

#[test]
fn string() {
    parses_to! {
        parser: GrammarParser,
        input: "abc",
        rule: Rule::string,
        tokens: [
            string(0, 3)
        ]
    };
}

#[test]
fn insensitive() {
    parses_to! {
        parser: GrammarParser,
        input: "aBC",
        rule: Rule::insensitive,
        tokens: [
            insensitive(0, 3)
        ]
    };
}

#[test]
fn range() {
    parses_to! {
        parser: GrammarParser,
        input: "6",
        rule: Rule::range,
        tokens: [
            range(0, 1)
        ]
    };
}

#[test]
fn ident() {
    parses_to! {
        parser: GrammarParser,
        input: "abc",
        rule: Rule::ident,
        tokens: [
            ident(0, 3, [
                string(0, 3)
            ])
        ]
    };
}

#[test]
fn pos_pred() {
    parses_to! {
        parser: GrammarParser,
        input: "abc",
        rule: Rule::pos_pred,
        tokens: [
            pos_pred(0, 0)
        ]
    };
}

#[test]
fn neg_pred() {
    parses_to! {
        parser: GrammarParser,
        input: "",
        rule: Rule::neg_pred,
        tokens: [
            neg_pred(0, 0)
        ]
    };
}

#[test]
fn double_neg_pred() {
    parses_to! {
        parser: GrammarParser,
        input: "abc",
        rule: Rule::double_neg_pred,
        tokens: [
            double_neg_pred(0, 0)
        ]
    };
}

#[test]
fn sequence() {
    parses_to! {
        parser: GrammarParser,
        input: "abc   abc",
        rule: Rule::sequence,
        tokens: [
            sequence(0, 9, [
                string(0, 3),
                string(6, 9)
            ])
        ]
    };
}

#[test]
fn sequence_compound() {
    parses_to! {
        parser: GrammarParser,
        input: "abcabc",
        rule: Rule::sequence_compound,
        tokens: [
            sequence_compound(0, 6, [
                string(0, 3),
                string(3, 6)
            ])
        ]
    };
}

#[test]
fn sequence_atomic() {
    parses_to! {
        parser: GrammarParser,
        input: "abcabc",
        rule: Rule::sequence_atomic,
        tokens: [
            sequence_atomic(0, 6)
        ]
    };
}

#[test]
fn sequence_non_atomic() {
    parses_to! {
        parser: GrammarParser,
        input: "abc   abc",
        rule: Rule::sequence_non_atomic,
        tokens: [
            sequence_non_atomic(0, 9, [
                sequence(0, 9, [
                    string(0, 3),
                    string(6, 9)
                ])
            ])
        ]
    };
}

#[test]
#[should_panic]
fn sequence_atomic_space() {
    parses_to! {
        parser: GrammarParser,
        input: "abc abc",
        rule: Rule::sequence_atomic,
        tokens: []
    };
}

#[test]
fn sequence_atomic_compound() {
    parses_to! {
        parser: GrammarParser,
        input: "abcabc",
        rule: Rule::sequence_atomic_compound,
        tokens: [
            sequence_atomic_compound(0, 6, [
                sequence_compound(0, 6, [
                    string(0, 3),
                    string(3, 6)
                ])
            ])
        ]
    };
}

#[test]
fn choice_string() {
    parses_to! {
        parser: GrammarParser,
        input: "abc",
        rule: Rule::choice,
        tokens: [
            choice(0, 3, [
                string(0, 3)
            ])
        ]
    };
}

#[test]
fn choice_range() {
    parses_to! {
        parser: GrammarParser,
        input: "0",
        rule: Rule::choice,
        tokens: [
            choice(0, 1, [
                range(0, 1)
            ])
        ]
    };
}

#[test]
fn optional_string() {
    parses_to! {
        parser: GrammarParser,
        input: "abc",
        rule: Rule::optional,
        tokens: [
            optional(0, 3, [
                string(0, 3)
            ])
        ]
    };
}

#[test]
fn optional_empty() {
    parses_to! {
        parser: GrammarParser,
        input: "",
        rule: Rule::optional,
        tokens: [
            optional(0, 0)
        ]
    };
}

#[test]
fn repeat_empty() {
    parses_to! {
        parser: GrammarParser,
        input: "",
        rule: Rule::repeat,
        tokens: [
            repeat(0, 0)
        ]
    };
}

#[test]
fn repeat_strings() {
    parses_to! {
        parser: GrammarParser,
        input: "abc   abc",
        rule: Rule::repeat,
        tokens: [
            repeat(0, 9, [
                string(0, 3),
                string(6, 9)
            ])
        ]
    };
}

#[test]
fn repeat_atomic_empty() {
    parses_to! {
        parser: GrammarParser,
        input: "",
        rule: Rule::repeat_atomic,
        tokens: [
            repeat_atomic(0, 0)
        ]
    };
}

#[test]
fn repeat_atomic_strings() {
    parses_to! {
        parser: GrammarParser,
        input: "abcabc",
        rule: Rule::repeat_atomic,
        tokens: [
            repeat_atomic(0, 6)
        ]
    };
}

#[test]
#[should_panic]
fn repeat_atomic_space() {
    parses_to! {
        parser: GrammarParser,
        input: "abc abc",
        rule: Rule::repeat_atomic,
        tokens: []
    };
}


#[test]
#[should_panic]
fn repeat_once_empty() {
    parses_to! {
        parser: GrammarParser,
        input: "",
        rule: Rule::repeat_once,
        tokens: []
    };
}

#[test]
fn repeat_once_strings() {
    parses_to! {
        parser: GrammarParser,
        input: "abc   abc",
        rule: Rule::repeat_once,
        tokens: [
            repeat_once(0, 9, [
                string(0, 3),
                string(6, 9)
            ])
        ]
    };
}

#[test]
#[should_panic]
fn repeat_once_atomic_empty() {
    parses_to! {
        parser: GrammarParser,
        input: "",
        rule: Rule::repeat_once_atomic,
        tokens: []
    };
}

#[test]
fn repeat_once_atomic_strings() {
    parses_to! {
        parser: GrammarParser,
        input: "abcabc",
        rule: Rule::repeat_once_atomic,
        tokens: [
            repeat_once_atomic(0, 6)
        ]
    };
}

#[test]
#[should_panic]
fn repeat_once_atomic_space() {
    parses_to! {
        parser: GrammarParser,
        input: "abc abc",
        rule: Rule::repeat_once_atomic,
        tokens: []
    };
}

#[test]
fn peek() {
    parses_to! {
        parser: GrammarParser,
        input: "0111",
        rule: Rule::peek_,
        tokens: [
            peek_(0, 4, [
                range(0, 1),
                range(1, 2)
            ])
        ]
    };
}

#[test]
fn pop() {
    parses_to! {
        parser: GrammarParser,
        input: "0110",
        rule: Rule::pop_,
        tokens: [
            pop_(0, 4, [
                range(0, 1),
                range(1, 2)
            ])
        ]
    };
}

#[test]
fn pop_fail() {
    parses_to! {
        parser: GrammarParser,
        input: "010",
        rule: Rule::pop_fail,
        tokens: [
            pop_fail(0, 3, [
                range(0, 1),
                range(1, 2)
            ])
        ]
    };
}
