// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

extern crate pest;
extern crate pest_meta;
#[macro_use]
extern crate pest_vm;

use pest_meta::parser::Rule;
use pest_meta::{optimizer, parser};
use pest_vm::Vm;

const GRAMMAR: &'static str = include_str!("grammar.pest");

fn vm() -> Vm {
    let pairs = parser::parse(Rule::grammar_rules, GRAMMAR).unwrap();
    let ast = parser::consume_rules(pairs).unwrap();
    Vm::new(optimizer::optimize(ast))
}

#[test]
fn string() {
    parses_to! {
        parser: vm(),
        input: "abc",
        rule: "string",
        tokens: [
            string(0, 3)
        ]
    };
}

#[test]
fn insensitive() {
    parses_to! {
        parser: vm(),
        input: "aBC",
        rule: "insensitive",
        tokens: [
            insensitive(0, 3)
        ]
    };
}

#[test]
fn range() {
    parses_to! {
        parser: vm(),
        input: "6",
        rule: "range",
        tokens: [
            range(0, 1)
        ]
    };
}

#[test]
fn ident() {
    parses_to! {
        parser: vm(),
        input: "abc",
        rule: "ident",
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
        parser: vm(),
        input: "abc",
        rule: "pos_pred",
        tokens: [
            pos_pred(0, 0)
        ]
    };
}

#[test]
fn neg_pred() {
    parses_to! {
        parser: vm(),
        input: "",
        rule: "neg_pred",
        tokens: [
            neg_pred(0, 0)
        ]
    };
}

#[test]
fn double_neg_pred() {
    parses_to! {
        parser: vm(),
        input: "abc",
        rule: "double_neg_pred",
        tokens: [
            double_neg_pred(0, 0)
        ]
    };
}

#[test]
fn sequence() {
    parses_to! {
        parser: vm(),
        input: "abc   abc",
        rule: "sequence",
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
        parser: vm(),
        input: "abcabc",
        rule: "sequence_compound",
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
        parser: vm(),
        input: "abcabc",
        rule: "sequence_atomic",
        tokens: [
            sequence_atomic(0, 6)
        ]
    };
}

#[test]
fn sequence_non_atomic() {
    parses_to! {
        parser: vm(),
        input: "abc   abc",
        rule: "sequence_non_atomic",
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
        parser: vm(),
        input: "abc abc",
        rule: "sequence_atomic",
        tokens: []
    };
}

#[test]
fn sequence_atomic_compound() {
    parses_to! {
        parser: vm(),
        input: "abcabc",
        rule: "sequence_atomic_compound",
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
fn sequence_compound_nested() {
    parses_to! {
        parser: vm(),
        input: "abcabc",
        rule: "sequence_compound_nested",
        tokens: [
            sequence_compound_nested(0, 6, [
                sequence_nested(0, 6, [
                    string(0, 3),
                    string(3, 6)
                ])
            ])
        ]
    };
}

#[test]
#[should_panic]
fn sequence_compound_nested_space() {
    parses_to! {
        parser: vm(),
        input: "abc abc",
        rule: "sequence_compound_nested",
        tokens: []
    };
}

#[test]
fn choice_string() {
    parses_to! {
        parser: vm(),
        input: "abc",
        rule: "choice",
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
        parser: vm(),
        input: "0",
        rule: "choice",
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
        parser: vm(),
        input: "abc",
        rule: "optional",
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
        parser: vm(),
        input: "",
        rule: "optional",
        tokens: [
            optional(0, 0)
        ]
    };
}

#[test]
fn repeat_empty() {
    parses_to! {
        parser: vm(),
        input: "",
        rule: "repeat",
        tokens: [
            repeat(0, 0)
        ]
    };
}

#[test]
fn repeat_strings() {
    parses_to! {
        parser: vm(),
        input: "abc   abc",
        rule: "repeat",
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
        parser: vm(),
        input: "",
        rule: "repeat_atomic",
        tokens: [
            repeat_atomic(0, 0)
        ]
    };
}

#[test]
fn repeat_atomic_strings() {
    parses_to! {
        parser: vm(),
        input: "abcabc",
        rule: "repeat_atomic",
        tokens: [
            repeat_atomic(0, 6)
        ]
    };
}

#[test]
#[should_panic]
fn repeat_atomic_space() {
    parses_to! {
        parser: vm(),
        input: "abc abc",
        rule: "repeat_atomic",
        tokens: []
    };
}

#[test]
#[should_panic]
fn repeat_once_empty() {
    parses_to! {
        parser: vm(),
        input: "",
        rule: "repeat_once",
        tokens: []
    };
}

#[test]
fn repeat_once_strings() {
    parses_to! {
        parser: vm(),
        input: "abc   abc",
        rule: "repeat_once",
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
        parser: vm(),
        input: "",
        rule: "repeat_once_atomic",
        tokens: []
    };
}

#[test]
fn repeat_once_atomic_strings() {
    parses_to! {
        parser: vm(),
        input: "abcabc",
        rule: "repeat_once_atomic",
        tokens: [
            repeat_once_atomic(0, 6)
        ]
    };
}

#[test]
#[should_panic]
fn repeat_once_atomic_space() {
    parses_to! {
        parser: vm(),
        input: "abc abc",
        rule: "repeat_once_atomic",
        tokens: []
    };
}

#[test]
fn repeat_min_max_twice() {
    parses_to! {
        parser: vm(),
        input: "abc abc",
        rule: "repeat_min_max",
        tokens: [
            repeat_min_max(0, 7, [
                string(0, 3),
                string(4, 7)
            ])
        ]
    };
}

#[test]
fn repeat_min_max_thrice() {
    parses_to! {
        parser: vm(),
        input: "abc abc abc",
        rule: "repeat_min_max",
        tokens: [
            repeat_min_max(0, 11, [
                string(0, 3),
                string(4, 7),
                string(8, 11)
            ])
        ]
    };
}

#[test]
fn repeat_min_max_atomic_twice() {
    parses_to! {
        parser: vm(),
        input: "abcabc",
        rule: "repeat_min_max_atomic",
        tokens: [
            repeat_min_max_atomic(0, 6)
        ]
    };
}

#[test]
fn repeat_min_max_atomic_thrice() {
    parses_to! {
        parser: vm(),
        input: "abcabcabc",
        rule: "repeat_min_max_atomic",
        tokens: [
            repeat_min_max_atomic(0, 9)
        ]
    };
}

#[test]
#[should_panic]
fn repeat_min_max_atomic_space() {
    parses_to! {
        parser: vm(),
        input: "abc abc",
        rule: "repeat_min_max_atomic",
        tokens: []
    };
}

#[test]
fn repeat_exact() {
    parses_to! {
        parser: vm(),
        input: "abc abc",
        rule: "repeat_exact",
        tokens: [
            repeat_exact(0, 7, [
                string(0, 3),
                string(4, 7)
            ])
        ]
    };
}

#[test]
#[should_panic]
fn repeat_min_once() {
    parses_to! {
        parser: vm(),
        input: "abc",
        rule: "repeat_min",
        tokens: []
    };
}

#[test]
fn repeat_min_twice() {
    parses_to! {
        parser: vm(),
        input: "abc abc",
        rule: "repeat_min",
        tokens: [
            repeat_min(0, 7, [
                string(0, 3),
                string(4, 7)
            ])
        ]
    };
}

#[test]
fn repeat_min_thrice() {
    parses_to! {
        parser: vm(),
        input: "abc abc  abc",
        rule: "repeat_min",
        tokens: [
            repeat_min(0, 12, [
                string(0, 3),
                string(4, 7),
                string(9, 12)
            ])
        ]
    };
}

#[test]
#[should_panic]
fn repeat_min_atomic_once() {
    parses_to! {
        parser: vm(),
        input: "abc",
        rule: "repeat_min_atomic",
        tokens: []
    };
}

#[test]
fn repeat_min_atomic_twice() {
    parses_to! {
        parser: vm(),
        input: "abcabc",
        rule: "repeat_min_atomic",
        tokens: [
            repeat_min_atomic(0, 6)
        ]
    };
}

#[test]
fn repeat_min_atomic_thrice() {
    parses_to! {
        parser: vm(),
        input: "abcabcabc",
        rule: "repeat_min_atomic",
        tokens: [
            repeat_min_atomic(0, 9)
        ]
    };
}

#[test]
#[should_panic]
fn repeat_min_atomic_space() {
    parses_to! {
        parser: vm(),
        input: "abc abc",
        rule: "repeat_min_atomic",
        tokens: []
    };
}

#[test]
fn repeat_max_once() {
    parses_to! {
        parser: vm(),
        input: "abc",
        rule: "repeat_max",
        tokens: [
            repeat_max(0, 3, [
                string(0, 3)
            ])
        ]
    };
}

#[test]
fn repeat_max_twice() {
    parses_to! {
        parser: vm(),
        input: "abc abc",
        rule: "repeat_max",
        tokens: [
            repeat_max(0, 7, [
                string(0, 3),
                string(4, 7)
            ])
        ]
    };
}

#[test]
#[should_panic]
fn repeat_max_thrice() {
    parses_to! {
        parser: vm(),
        input: "abc abc",
        rule: "repeat_max",
        tokens: []
    };
}

#[test]
fn repeat_max_atomic_once() {
    parses_to! {
        parser: vm(),
        input: "abc",
        rule: "repeat_max_atomic",
        tokens: [
            repeat_max_atomic(0, 3)
        ]
    };
}

#[test]
fn repeat_max_atomic_twice() {
    parses_to! {
        parser: vm(),
        input: "abcabc",
        rule: "repeat_max_atomic",
        tokens: [
            repeat_max_atomic(0, 6)
        ]
    };
}

#[test]
#[should_panic]
fn repeat_max_atomic_thrice() {
    parses_to! {
        parser: vm(),
        input: "abcabcabc",
        rule: "repeat_max_atomic",
        tokens: []
    };
}

#[test]
#[should_panic]
fn repeat_max_atomic_space() {
    parses_to! {
        parser: vm(),
        input: "abc abc",
        rule: "repeat_max_atomic",
        tokens: []
    };
}

#[test]
fn repeat_comment() {
    parses_to! {
        parser: vm(),
        input: "abc$$$ $$$abc",
        rule: "repeat_once",
        tokens: [
            repeat_once(0, 13, [
                string(0, 3),
                string(10, 13)
            ])
        ]
    };
}

#[test]
fn soi_at_start() {
    parses_to! {
        parser: vm(),
        input: "abc",
        rule: "soi_at_start",
        tokens: [
            soi_at_start(0, 3, [
                string(0, 3)
            ])
        ]
    };
}

#[test]
fn peek() {
    parses_to! {
        parser: vm(),
        input: "0111",
        rule: "peek_",
        tokens: [
            peek_(0, 4, [
                range(0, 1),
                range(1, 2)
            ])
        ]
    };
}

#[test]
fn peek_all() {
    parses_to! {
        parser: vm(),
        input: "0110",
        rule: "peek_all",
        tokens: [
            peek_all(0, 4, [
                range(0, 1),
                range(1, 2)
            ])
        ]
    };
}

#[test]
fn pop() {
    parses_to! {
        parser: vm(),
        input: "0110",
        rule: "pop_",
        tokens: [
            pop_(0, 4, [
                range(0, 1),
                range(1, 2)
            ])
        ]
    };
}

#[test]
fn pop_all() {
    parses_to! {
        parser: vm(),
        input: "0110",
        rule: "pop_all",
        tokens: [
            pop_all(0, 4, [
                range(0, 1),
                range(1, 2)
            ])
        ]
    };
}

#[test]
fn pop_fail() {
    parses_to! {
        parser: vm(),
        input: "010",
        rule: "pop_fail",
        tokens: [
            pop_fail(0, 3, [
                range(0, 1),
                range(1, 2)
            ])
        ]
    };
}

#[test]
fn repeat_mutate_stack() {
    parses_to! {
        parser: vm(),
        input: "a,b,c,cba",
        rule: "repeat_mutate_stack",
        tokens: [
            repeat_mutate_stack(0, 9)
        ]
    };
}

#[test]
fn checkpoint_restore() {
    parses_to! {
        parser: vm(),
        input: "a",
        rule: "checkpoint_restore",
        tokens: [
            checkpoint_restore(0, 1, [EOI(1, 1)])
        ]
    };
}

#[test]
fn ascii_digits() {
    parses_to! {
        parser: vm(),
        input: "6",
        rule: "ascii_digits",
        tokens: [
            ascii_digits(0, 1)
        ]
    };
}

#[test]
fn ascii_nonzero_digits() {
    parses_to! {
        parser: vm(),
        input: "5",
        rule: "ascii_nonzero_digits",
        tokens: [
            ascii_nonzero_digits(0, 1)
        ]
    };
}

#[test]
fn ascii_bin_digits() {
    parses_to! {
        parser: vm(),
        input: "1",
        rule: "ascii_bin_digits",
        tokens: [
            ascii_bin_digits(0, 1)
        ]
    };
}

#[test]
fn ascii_oct_digits() {
    parses_to! {
        parser: vm(),
        input: "3",
        rule: "ascii_oct_digits",
        tokens: [
            ascii_oct_digits(0, 1)
        ]
    };
}

#[test]
fn ascii_hex_digits() {
    parses_to! {
        parser: vm(),
        input: "6bC",
        rule: "ascii_hex_digits",
        tokens: [
            ascii_hex_digits(0, 3)
        ]
    };
}

#[test]
fn ascii_alpha_lowers() {
    parses_to! {
        parser: vm(),
        input: "a",
        rule: "ascii_alpha_lowers",
        tokens: [
            ascii_alpha_lowers(0, 1)
        ]
    };
}

#[test]
fn ascii_alpha_uppers() {
    parses_to! {
        parser: vm(),
        input: "K",
        rule: "ascii_alpha_uppers",
        tokens: [
            ascii_alpha_uppers(0, 1)
        ]
    };
}

#[test]
fn ascii_alphas() {
    parses_to! {
        parser: vm(),
        input: "wF",
        rule: "ascii_alphas",
        tokens: [
            ascii_alphas(0, 2)
        ]
    };
}

#[test]
fn ascii_alphanumerics() {
    parses_to! {
        parser: vm(),
        input: "4jU",
        rule: "ascii_alphanumerics",
        tokens: [
            ascii_alphanumerics(0, 3)
        ]
    };
}

#[test]
fn asciis() {
    parses_to! {
        parser: vm(),
        input: "x02",
        rule: "asciis",
        tokens: [
            asciis(0, 3)
        ]
    };
}

#[test]
fn newline() {
    parses_to! {
        parser: vm(),
        input: "\n\r\n\r",
        rule: "newline",
        tokens: [
            newline(0, 4)
        ]
    };
}

#[test]
fn unicode() {
    parses_to! {
        parser: vm(),
        input: "نامهای",
        rule: "unicode",
        tokens: [
            unicode(0, 12)
        ]
    }
}

#[test]
fn shadowing() {
    parses_to! {
        parser: vm(),
        input: "shadows builtin",
        rule: "SYMBOL",
        tokens: [
            SYMBOL(0, 15)
        ]
    }
}
