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
/// # use pest::Token;
/// # use pest::Input;
/// # use pest::StringInput;
/// # fn main() {
/// impl_rdp! {
///     grammar! {
///         rule = { [""] }
///     }
/// }
///
/// let input = StringInput::new("asdasdf");
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
    };

    // filter out quiet rules
    ( @filter [  ] [ $( $rules:tt )* ] ) => {
        impl_rdp!(@rules $( $rules )*);
    };
    ( @filter [ $name:ident = { { $( $_primary:tt )* } $( $ts:tt )* } $( $tail:tt )* ]
      [ $( $rules:tt )* ] ) => {
        impl_rdp!(@filter [ $( $tail )* $( $ts )* ] [ $name $( $rules )* ]);
    };
    ( @filter [ $name:ident = @{ { $( $_primary:tt )* } $( $ts:tt )* } $( $tail:tt )* ]
      [ $( $rules:tt )* ] ) => {
        impl_rdp!(@filter [ $( $tail )* $( $ts )* ] [ $name $( $rules )* ]);
    };
    ( @filter [ $name:ident = _{ { $( $_primary:tt )* } $( $ts:tt )* } $( $tail:tt )* ]
      [ $( $rules:tt )* ] ) => {
        impl_rdp!(@filter [ $( $tail )* $( $ts )* ] [ $name $( $rules )* ]);
    };
    ( @filter [ $name:ident = { $( $_ts:tt )* } $( $tail:tt )* ] [ $( $rules:tt )* ] ) => {
        impl_rdp!(@filter [ $( $tail )* ] [ $name $( $rules )* ]);
    };
    ( @filter [ $name:ident = @{ $( $_ts:tt )* } $( $tail:tt )* ] [ $( $rules:tt )* ] ) => {
        impl_rdp!(@filter [ $( $tail )* ] [ $name $( $rules )* ]);
    };
    ( @filter [ $name:ident = _{ $( $_ts:tt )* } $( $tail:tt )* ] [ $( $rules:tt )* ] ) => {
        impl_rdp!(@filter [ $( $tail )* ] [ $( $rules )* ]);
    };

    // implement empty whitespace rule
    ( @ws ) => {
        #[allow(dead_code)]
        pub fn whitespace(&mut self) -> bool {
            false
        }
    };
    ( @ws whitespace = $( $_ts:tt )* ) => ();
    ( @ws $_name:ident = { $( $_ts:tt )* } $( $tail:tt )* ) => {
        impl_rdp!(@ws $( $tail )*);
    };
    ( @ws $_name:ident = @{ $( $_ts:tt )* } $( $tail:tt )* ) => {
        impl_rdp!(@ws $( $tail )*);
    };
    ( @ws $_name:ident = _{ $( $_ts:tt )* } $( $tail:tt )* ) => {
        impl_rdp!(@ws $( $tail )*);
    };

    // implement empty comment rule
    ( @com ) => {
        #[allow(dead_code)]
        pub fn comment(&mut self) -> bool {
            false
        }
    };
    ( @com comment = $( $_ts:tt )* ) => ();
    ( @com $_name:ident = { $( $_ts:tt )* } $( $tail:tt )* ) => {
        impl_rdp!(@com $( $tail )*);
    };
    ( @com $_name:ident = @{ $( $_ts:tt )* } $( $tail:tt )* ) => {
        impl_rdp!(@com $( $tail )*);
    };
    ( @com $_name:ident = _{ $( $_ts:tt )* } $( $tail:tt )* ) => {
        impl_rdp!(@com $( $tail )*);
    };

    ( grammar!{ $( $ts:tt )* } ) => {
        use std::cmp;
        use std::collections::VecDeque;

        pub struct Rdp<T: Input> {
            input:    T,
            queues:   Queues<Token<Rule>>,
            failures: Vec<Rule>,
            fail_pos: usize,
            atomic:   bool,
            comment:  bool
        }

        impl_rdp!(@filter [ $( $ts )* ] []);

        impl<T: Input> Rdp<T> {
            pub fn new(input: T) -> Rdp<T> {
                Rdp {
                    input:    input,
                    queues:   Queues::new(),
                    failures: vec![],
                    fail_pos: 0,
                    atomic:   false,
                    comment:  false
                }
            }

            impl_rdp!(@ws $( $ts )*);
            impl_rdp!(@com $( $ts )*);

            grammar! {
                $( $ts )*
            }
        }

        impl<T: Input> Parser for Rdp<T> {
            type Rule = Rule;
            type Token = Token<Rule>;

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

            fn prec_climb<F, G>(&mut self, pos: usize, left: usize, prec: u8,
                                last_op: Option<(Option<Rule>, u8, bool)>, primary: &mut F,
                                climb: &mut G) -> (Option<(Option<Rule>, u8, bool)>, Option<usize>)
                where F: FnMut(&mut Self) -> bool,
                      G: FnMut(&mut Self) -> Option<(Option<Rule>, u8, bool)> {

                let mut op = if last_op.is_some() {
                    last_op
                } else {
                    climb(self)
                };
                let mut last_right = None;

                while let Some((rule, new_prec, _)) = op {
                    if new_prec >= prec {
                        let mut new_pos = self.pos();
                        let mut right = self.pos();
                        let queue_pos = self.queue().len();

                        primary(self);

                        if let Some(token) = self.queue().get(queue_pos) {
                            new_pos = token.start;
                            right   = token.end;
                        }

                        op = climb(self);

                        while let Some((_, new_prec, right_assoc)) = op {
                            if new_prec > prec || right_assoc && new_prec == prec {
                                let (new_op, new_lr) = self.prec_climb(queue_pos, new_pos,
                                                                       new_prec, op, primary,
                                                                       climb);

                                op = new_op;
                                last_right = new_lr;
                            } else {
                                break
                            }
                        }

                        if let Some(pos) = last_right {
                            right = cmp::max(pos, right);
                        } else {
                            last_right = Some(right);
                        }

                        if let Some(rule) = rule {
                            let token = Token {
                                rule:  rule,
                                start: left,
                                end:   right
                            };

                            self.queue().insert(pos, token);
                        }
                    } else {
                        return (op, last_right)
                    }
                }

                (op, last_right)
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

            fn queue(&mut self) -> &mut VecDeque<Token<Rule>>{
                if let Some(queue) = self.queues.last_mut() {
                    queue
                } else {
                    unreachable!();
                }
            }

            fn skip_ws(&mut self) {
                if self.atomic {
                    return
                }

                loop {
                    if !self.whitespace() {
                        break
                    }
                }
            }

            fn skip_com(&mut self) {
                if self.atomic {
                    return
                }

                if !self.comment {
                    self.comment = true;

                    loop {
                        if !self.comment() {
                            break
                        }
                    }

                    self.comment = false;
                }
            }

            fn is_atomic(&self) -> bool {
                self.atomic
            }

            fn set_atomic(&mut self, value: bool) {
                self.atomic = value;
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
                self.failures.sort();
                self.failures.dedup();

                (self.failures.iter().cloned().collect(), self.fail_pos)
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use super::super::super::Parser;
    use super::super::super::Queues;
    use super::super::super::Token;
    use super::super::super::Input;
    use super::super::super::StringInput;

    impl_rdp! {
        grammar! {
            exp = _{ paren ~ exp | [""] }
            paren = { ["("] ~ exp ~ [")"] }
            zero = { ["a"]* }
            one = { ["a"]+ }
            comment = _{ ["//"] ~ (!["\n"] ~ any)* ~ ["\n"] }
            whitespace = _{ [" "] }
        }
    }

    #[test]
    fn matches() {
        let input = StringInput::new("asdasdf");
        let mut parser = Rdp::new(input);

        assert!(parser.matches("asd"));
        assert!(parser.matches("asdf"));
        assert!(parser.matches(""));
        assert!(!parser.matches("a"));
    }

    #[test]
    fn try() {
        let input = StringInput::new("asdasdf");
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
        let input = StringInput::new("asdasdf");
        let mut parser = Rdp::new(input);

        assert!(parser.matches("asdasdf"));
        assert!(parser.end());
    }

    #[test]
    fn reset() {
        let input = StringInput::new("asdasdf");
        let mut parser = Rdp::new(input);

        assert!(parser.matches("asdasdf"));

        parser.reset();

        assert!(parser.matches("asdasdf"));
    }

    #[test]
    fn whitespace_seq() {
        let mut parser = Rdp::new(StringInput::new("  (  ( ))(( () )() )() "));

        assert!(parser.exp());
        assert!(!parser.end());

        let queue = vec![
            Token { rule: Rule::paren, start: 2, end: 9 },
            Token { rule: Rule::paren, start: 5, end: 8 },
            Token { rule: Rule::paren, start: 9, end: 20 },
            Token { rule: Rule::paren, start: 10, end: 16 },
            Token { rule: Rule::paren, start: 12, end: 14 },
            Token { rule: Rule::paren, start: 16, end: 18 },
            Token { rule: Rule::paren, start: 20, end: 22 }
        ];

        assert!(parser.queue().iter().eq(&queue));
    }

    #[test]
    fn whitespace_zero() {
        let mut parser = Rdp::new(StringInput::new("  a a aa aaaa a  "));

        assert!(parser.zero());
        assert!(!parser.end());

        let queue = vec![
            Token { rule: Rule::zero, start: 2, end: 15 }
        ];

        assert!(parser.queue().iter().eq(&queue));
    }

    #[test]
    fn whitespace_one() {
        let mut parser = Rdp::new(StringInput::new("  a a aa aaaa a  "));

        assert!(parser.one());
        assert!(!parser.end());

        let queue = vec![
            Token { rule: Rule::one, start: 2, end: 15 }
        ];

        assert!(parser.queue().iter().eq(&queue));
    }

    #[test]
    fn comment() {
        let mut parser = Rdp::new(StringInput::new("// hi\n(())"));

        assert!(parser.exp());
        assert!(parser.end());

        let queue = vec![
            Token { rule: Rule::paren, start: 6, end: 10 },
            Token { rule: Rule::paren, start: 7, end: 9 }
        ];

        assert!(parser.queue().iter().eq(&queue));
    }

    #[test]
    fn comment_whitespace() {
        let mut parser = Rdp::new(StringInput::new("   // hi\n  (())"));

        assert!(parser.exp());
        assert!(parser.end());

        let queue = vec![
            Token { rule: Rule::paren, start: 11, end: 15 },
            Token { rule: Rule::paren, start: 12, end: 14 }
        ];

        assert!(parser.queue().iter().eq(&queue));
    }
}
