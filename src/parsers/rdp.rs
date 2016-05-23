// pest. Smart PEGs in Rust
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// A `macro` useful for implementing the `Parser` `trait` as a recursive descent parser.
///
/// # Examples
///
/// ```
/// # #[macro_use] extern crate pest;
/// # use pest::Parser;
/// # use pest::Queues;
/// # use pest::Input;
/// # use pest::StringInput;
/// # fn main() {
/// impl_rdp! {
///     grammar! {
///         rule = { [""] }
///     }
/// }
///
/// let input = Box::new(StringInput::new("asdasdf"));
/// let mut parser = Rdp::new(input);
///
/// assert!(parser.matches("asd"));
/// assert!(parser.matches("asdf"));
/// # }
/// ```
#[macro_export]
macro_rules! impl_rdp {
    ( @rules $( $name:ident )* ) => {
        #[allow(dead_code, non_camel_case_types)]
        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum Rule {
            any,
            eoi,
            $( $name ),*
        }

        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        pub struct Token {
            pub rule: Rule,
            pub pos:  usize,
            pub len:  usize
        }
    };

    // filter out hidden rules
    ( @filter [  ] [ $( $rules:tt )* ] ) => {
        impl_rdp!(@rules $( $rules )*);
    };
    ( @filter [ $name:ident = { $( $_ts:tt )* } $( $tail:tt )* ] [ $( $rules:tt )* ] ) => {
        impl_rdp!(@filter [ $( $tail )* ] [ $name $( $rules )* ]);
    };
    ( @filter [ $name:ident = _{ $( $_ts:tt )* } $( $tail:tt )* ] [ $( $rules:tt )* ] ) => {
        impl_rdp!(@filter [ $( $tail )* ] [ $( $rules )* ]);
    };

    // implement empty ws rule
    ( @ws ) => {
        #[allow(dead_code)]
        pub fn ws(&mut self) -> bool {
            false
        }
    };
    ( @ws ws = $( $_ts:tt )* ) => ();
    ( @ws $_name:ident = { $( $_ts:tt )* } $( $tail:tt )* ) => {
        impl_rdp!(@ws $( $tail )*);
    };
    ( @ws $_name:ident = _{ $( $_ts:tt )* } $( $tail:tt )* ) => {
        impl_rdp!(@ws $( $tail )*);
    };

    ( grammar!{ $( $ts:tt )* } ) => {
        use std::collections::VecDeque;

        pub struct Rdp {
            input:    Box<Input>,
            queues:   Queues<Token>,
            failures: Vec<Rule>,
            fail_pos: usize
        }

        impl_rdp!(@filter [ $( $ts )* ] []);

        impl Rdp {
            pub fn new(input: Box<Input>) -> Rdp {
                Rdp {
                    input:    input,
                    queues:   Queues::new(),
                    failures: vec![],
                    fail_pos: 0
                }
            }

            impl_rdp!(@ws $( $ts )*);

            grammar! {
                $( $ts )*
            }
        }

        impl Parser for Rdp {
            type Rule = Rule;
            type Token = Token;

            fn matches(&mut self, string: &str) -> bool {
                self.input.matches(string)
            }

            fn between(&mut self, left: char, right: char) -> bool {
                self.input.between(left, right)
            }

            fn try<F>(&mut self, revert: bool, rule: F) -> bool
                where F: FnOnce(&mut Self) -> bool {

                let pos = self.input.pos();
                self.queues.push();

                let result = rule(self);

                if revert || !result {
                    self.input.set_pos(pos);
                }

                if result {
                    self.queues.pour();
                } else {
                    self.queues.pop();
                }

                result
            }

            fn pos(&self) -> usize {
                self.input.pos()
            }

            fn set_pos(&mut self, pos: usize) {
                self.input.set_pos(pos);
            }

            fn end(&self) -> bool {
                self.input.len() == self.input.pos()
            }

            fn reset(&mut self) {
                self.input.set_pos(0);
                self.queues.clear();
                self.failures.clear();
                self.fail_pos = 0;
            }

            fn queue(&mut self) -> &mut VecDeque<Token>{
                if let Some(queue) = self.queues.last_mut() {
                    queue
                } else {
                    unreachable!();
                }
            }

            fn skip_ws(&mut self) {
                loop {
                    if !self.ws() {
                        break
                    }
                }
            }

            fn track(&mut self, failed: Rule, pos: usize) {
                if self.failures.is_empty() {
                    self.failures.push(failed);

                    self.fail_pos = pos;
                } else {
                    if pos == self.fail_pos {
                        self.failures.push(failed);
                    } else if pos > self.fail_pos {
                        self.failures.clear();
                        self.failures.push(failed);

                        self.fail_pos = pos;
                    }
                }
            }

            fn expected(&mut self) -> (Vec<Rule>, usize) {
                self.failures.dedup();
                self.failures.sort();

                (self.failures.iter().cloned().collect(), self.fail_pos)
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use super::super::super::Parser;
    use super::super::super::Queues;
    use super::super::super::Input;
    use super::super::super::StringInput;

    impl_rdp! {
        grammar! {
            exp = _{ paren ~ exp | [""] }
            paren = { ["("] ~ exp ~ [")"] }
            zero = { ["a"]* }
            one = { ["a"]+ }
            ws = _{ [" "] }
        }
    }

    #[test]
    fn matches() {
        let input = Box::new(StringInput::new("asdasdf"));
        let mut parser = Rdp::new(input);

        assert!(parser.matches("asd"));
        assert!(parser.matches("asdf"));
        assert!(parser.matches(""));
        assert!(!parser.matches("a"));
    }

    #[test]
    fn try() {
        let input = Box::new(StringInput::new("asdasdf"));
        let mut parser = Rdp::new(input);

        assert!(parser.matches("asd"));

        assert!(!parser.try(false, |parser| {
            parser.matches("as") && parser.matches("dd")
        }));

        assert!(parser.try(false, |parser| {
            parser.matches("as") && parser.matches("df")
        }));
    }

    #[test]
    fn end() {
        let input = Box::new(StringInput::new("asdasdf"));
        let mut parser = Rdp::new(input);

        assert!(parser.matches("asdasdf"));
        assert!(parser.end());
    }

    #[test]
    fn reset() {
        let input = Box::new(StringInput::new("asdasdf"));
        let mut parser = Rdp::new(input);

        assert!(parser.matches("asdasdf"));

        parser.reset();

        assert!(parser.matches("asdasdf"));
    }

    #[test]
    fn ws_seq() {
        let mut parser = Rdp::new(Box::new(StringInput::new("  (  ( ))(( () )() )() ")));

        assert!(parser.exp());
        assert!(parser.end());

        let queue = vec![
            Token { rule: Rule::paren, pos: 2, len: 7 },
            Token { rule: Rule::paren, pos: 5, len: 3 },
            Token { rule: Rule::paren, pos: 9, len: 11 },
            Token { rule: Rule::paren, pos: 10, len: 6 },
            Token { rule: Rule::paren, pos: 12, len: 2 },
            Token { rule: Rule::paren, pos: 16, len: 2 },
            Token { rule: Rule::paren, pos: 20, len: 2 }
        ];

        assert!(parser.queue().iter().eq(&queue));
    }

    #[test]
    fn ws_zero() {
        let mut parser = Rdp::new(Box::new(StringInput::new("  a a aa aaaa a  ")));

        assert!(parser.zero());
        assert!(!parser.end());

        let queue = vec![
            Token { rule: Rule::zero, pos: 2, len: 13 }
        ];

        assert!(parser.queue().iter().eq(&queue));
    }

    #[test]
    fn ws_one() {
        let mut parser = Rdp::new(Box::new(StringInput::new("  a a aa aaaa a  ")));

        assert!(parser.one());
        assert!(!parser.end());

        let queue = vec![
            Token { rule: Rule::one, pos: 2, len: 13 }
        ];

        assert!(parser.queue().iter().eq(&queue));
    }
}
