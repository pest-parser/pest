// pest. Elegant, efficient grammars
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// A `macro` useful for implementing the `Parser` `trait` as a recursive descent parser. It only
/// accepts `grammar!` and `process!` calls that get implemented on `self`.
///
/// # Rule
///
/// It also implements an `enum` called `Rule` that has a value for all
/// [non-silent](macro.grammar!#silent-rules-_) rules, but also for
/// [`any` and `eoi`](macro.grammar!). These `Rule`s are used within `Token`s to specify the type
/// of rule that matched.
///
/// # Examples
///
/// ```
/// # #[macro_use] extern crate pest;
/// # use pest::prelude::*;
/// # fn main() {
/// impl_rdp! {
///     grammar! {
///         expression = _{ paren ~ expression? }
///         paren      =  { ["("] ~ expression? ~ [")"] }
///     }
/// }
///
/// let mut parser = Rdp::new(StringInput::new("(())((())())()"));
///
/// assert!(parser.expression());
/// assert!(parser.end());
/// # }
/// ```
#[macro_export]
macro_rules! impl_rdp {
    // implement rules
    ( @rules $( $name:ident )* ) => {
        #[allow(dead_code, non_camel_case_types)]
        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum Rule {
            any,
            eoi,
            $( $name ),*
        }
    };

    // filter out silent rules
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
    ( @filter [ $_name:ident = _{ { $( $_primary:tt )* } $( $ts:tt )* } $( $tail:tt )* ]
      [ $( $rules:tt )* ] ) => {
        impl_rdp!(@filter [ $( $tail )* $( $ts )* ] [ $( $rules )* ]);
    };
    ( @filter [ $name:ident = { $( $_ts:tt )* } $( $tail:tt )* ] [ $( $rules:tt )* ] ) => {
        impl_rdp!(@filter [ $( $tail )* ] [ $name $( $rules )* ]);
    };
    ( @filter [ $name:ident = @{ $( $_ts:tt )* } $( $tail:tt )* ] [ $( $rules:tt )* ] ) => {
        impl_rdp!(@filter [ $( $tail )* ] [ $name $( $rules )* ]);
    };
    ( @filter [ $_name:ident = _{ $( $_ts:tt )* } $( $tail:tt )* ] [ $( $rules:tt )* ] ) => {
        impl_rdp!(@filter [ $( $tail )* ] [ $( $rules )* ]);
    };

    // implement empty whitespace rule
    ( @ws ) => {
        #[allow(dead_code)]
        #[inline]
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
        #[inline]
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

    ( grammar! { $( $ts:tt )* } $( $mac:ident! { $( $rest:tt )* } )* ) => {
        use std::cell::Cell;
        use std::cmp;

        pub struct Rdp<T> {
            input:       T,
            queue:       Vec<Token<Rule>>,
            queue_index: Cell<usize>,
            failures:    Vec<Rule>,
            fail_pos:    usize,
            atomic:      bool,
            comment:     bool,
            eoi_matched: bool
        }

        impl_rdp!(@filter [ $( $ts )* ] []);

        impl<'input, T: Input<'input>> Rdp<T> {
            pub fn new(input: T) -> Rdp<T> {
                Rdp {
                    input:       input,
                    queue:       vec![],
                    queue_index: Cell::new(0),
                    failures:    vec![],
                    fail_pos:    0,
                    atomic:      false,
                    comment:     false,
                    eoi_matched: false
                }
            }

            impl_rdp!(@ws $( $ts )*);
            impl_rdp!(@com $( $ts )*);

            #[allow(dead_code)]
            #[inline]
            pub fn any(&mut self) -> bool {
                if self.end() {
                    let pos = self.input.pos();

                    self.track(Rule::any, pos);

                    false
                } else {
                    let next = self.input.pos() + 1;
                    self.input.set_pos(next);

                    true
                }
            }

            #[allow(dead_code)]
            #[inline]
            pub fn eoi(&mut self) -> bool {
                let result = self.end();

                if !result {
                    let pos = self.input.pos();

                    self.track(Rule::eoi, pos);
                } else {
                    self.eoi_matched = true;
                }

                result
            }

            grammar! {
                $( $ts )*
            }

            $(
                $mac! {
                    $( $rest )*
                }
            )*
        }

        impl<'input, T: Input<'input>> Parser<'input, T> for Rdp<T> {
            type Rule = Rule;
            type Token = Token<Rule>;

            #[inline]
            fn input(&self) -> &T {
                &self.input
            }

            #[inline]
            fn input_mut(&mut self) -> &mut T {
                &mut self.input
            }

            #[inline]
            fn end(&self) -> bool {
                self.input.len() == self.input.pos()
            }

            #[inline]
            fn eoi_matched(&self) -> bool {
                self.eoi_matched
            }

            #[inline]
            fn reset(&mut self) {
                self.input.set_pos(0);
                self.queue.clear();
                self.queue_index.set(0);
                self.failures.clear();
                self.fail_pos = 0;
            }

            #[inline]
            fn queue(&self) -> &Vec<Token<Rule>>{
                &self.queue
            }

            #[inline]
            fn queue_index(&self) -> usize {
                self.queue_index.get()
            }

            #[inline]
            fn inc_queue_index(&self) {
                self.queue_index.set(self.queue_index.get() + 1);
            }

            #[inline]
            fn set_queue_index(&self, index: usize) {
                self.queue_index.set(index);
            }

            #[inline]
            fn queue_mut(&mut self) -> &mut Vec<Token<Rule>>{
                &mut self.queue
            }

            #[inline]
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

            #[inline]
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

            #[inline]
            fn is_atomic(&self) -> bool {
                self.atomic
            }

            #[inline]
            fn set_atomic(&mut self, value: bool) {
                self.atomic = value;
            }

            #[inline]
            fn track(&mut self, failed: Rule, pos: usize) {
                if self.atomic {
                    return
                }

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

            fn tracked_len(&self) -> usize {
                self.failures.len()
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
    use super::super::super::prelude::*;

    impl_rdp! {
        grammar! {
            expression = _{ paren ~ expression? }
            paren = { ["("] ~ expression? ~ [")"] }
            zero = { ["a"]* }
            one = { ["a"]+ }
            comment = _{ ["//"] ~ (!["\n"] ~ any)* ~ ["\n"] }
            whitespace = _{ [" "] }
        }
    }

    #[test]
    fn try() {
        let input = StringInput::new("asdasdf");
        let mut parser = Rdp::new(input);

        assert!(parser.input_mut().match_string("asd"));

        assert!(!parser.try(false, |parser| {
            parser.input_mut().match_string("as") && parser.input_mut().match_string("dd")
        }));

        assert!(parser.try(false, |parser| {
            parser.input_mut().match_string("as") && parser.input_mut().match_string("df")
        }));
    }

    #[test]
    fn end() {
        let input = StringInput::new("asdasdf");
        let mut parser = Rdp::new(input);

        assert!(parser.input_mut().match_string("asdasdf"));
        assert!(parser.end());
    }

    #[test]
    fn reset() {
        let input = StringInput::new("asdasdf");
        let mut parser = Rdp::new(input);

        assert!(parser.input_mut().match_string("asdasdf"));

        parser.reset();

        assert!(parser.input_mut().match_string("asdasdf"));
    }

    #[test]
    fn whitespace_seq() {
        let mut parser = Rdp::new(StringInput::new("  (  ( ))(( () )() )() "));

        assert!(parser.expression());
        assert!(!parser.end());

        let queue = vec![
            Token::new(Rule::paren, 2, 9),
            Token::new(Rule::paren, 5, 8),
            Token::new(Rule::paren, 9, 20),
            Token::new(Rule::paren, 10, 16),
            Token::new(Rule::paren, 12, 14),
            Token::new(Rule::paren, 16, 18),
            Token::new(Rule::paren, 20, 22)
        ];

        assert_eq!(parser.queue(), &queue);
    }

    #[test]
    fn whitespace_zero() {
        let mut parser = Rdp::new(StringInput::new("  a a aa aaaa a  "));

        assert!(parser.zero());
        assert!(!parser.end());

        let queue = vec![
            Token::new(Rule::zero, 2, 15)
        ];

        assert_eq!(parser.queue(), &queue);
    }

    #[test]
    fn whitespace_one() {
        let mut parser = Rdp::new(StringInput::new("  a a aa aaaa a  "));

        assert!(parser.one());
        assert!(!parser.end());

        let queue = vec![Token {
                             rule: Rule::one,
                             start: 2,
                             end: 15,
                         }];

        assert_eq!(parser.queue(), &queue);
    }

    #[test]
    fn comment() {
        let mut parser = Rdp::new(StringInput::new("// hi\n(())"));

        assert!(parser.expression());
        assert!(parser.end());

        let queue = vec![
            Token::new(Rule::paren, 6, 10),
            Token::new(Rule::paren, 7, 9)
        ];

        assert_eq!(parser.queue(), &queue);
    }

    #[test]
    fn comment_whitespace() {
        let mut parser = Rdp::new(StringInput::new("   // hi\n  (())"));

        assert!(parser.expression());
        assert!(parser.end());

        let queue = vec![
            Token::new(Rule::paren, 11, 15),
            Token::new(Rule::paren, 12, 14)
        ];

        assert_eq!(parser.queue(), &queue);
    }
}
