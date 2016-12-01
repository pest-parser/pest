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
            soi,
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
    ( @filter [ $name:ident = !@{ { $( $_primary:tt )* } $( $ts:tt )* } $( $tail:tt )* ]
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
    ( @filter [ $name:ident = !@{ $( $_ts:tt )* } $( $tail:tt )* ] [ $( $rules:tt )* ] ) => {
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
    ( @ws $_name:ident = !@{ $( $_ts:tt )* } $( $tail:tt )* ) => {
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
    ( @com $_name:ident = !@{ $( $_ts:tt )* } $( $tail:tt )* ) => {
        impl_rdp!(@com $( $tail )*);
    };
    ( @com $_name:ident = _{ $( $_ts:tt )* } $( $tail:tt )* ) => {
        impl_rdp!(@com $( $tail )*);
    };

    ( grammar! { $( $ts:tt )* } $( $mac:ident! { $( $rest:tt )* } )* ) => {
        pub struct Rdp<T> {
            input:       T,
            queue:       Vec<Token<Rule>>,
            queue_index: ::std::cell::Cell<usize>,
            failures:    Vec<Rule>,
            fail_pos:    usize,
            stack:       Vec<String>,
            atomic:      bool,
            eoi_matched: bool
        }

        impl_rdp!(@filter [ $( $ts )* ] []);

        impl<'input, T: $crate::Input<'input>> Rdp<T> {
            pub fn new(input: T) -> Rdp<T> {
                Rdp {
                    input:       input,
                    queue:       vec![],
                    queue_index: ::std::cell::Cell::new(0),
                    failures:    vec![],
                    fail_pos:    0,
                    stack:       vec![],
                    atomic:      false,
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
            pub fn soi(&mut self) -> bool {
                let result = self.input.pos() == 0;

                if !result {
                    let pos = self.input.pos();

                    self.track(Rule::soi, pos);
                }

                result
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

        impl<'input, T: $crate::Input<'input>> $crate::Parser<'input, T> for Rdp<T> {
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
            fn queue(&self) -> &Vec<Token<Rule>> {
                &self.queue
            }

            #[inline]
            fn queue_mut(&mut self) -> &mut Vec<Token<Rule>> {
                &mut self.queue
            }

            fn queue_with_captures(&self) -> Vec<(Token<Rule>, String)> {
                self.queue
                    .clone()
                    .into_iter()
                    .map(|t| (t, self.input().slice(t.start, t.end).to_owned()))
                    .collect()
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
            fn skip(&mut self) {
                if self.atomic {
                    return
                }

                loop {
                    if !self.whitespace() {
                        break
                    }
                }

                while self.comment() {
                    loop {
                        if !self.whitespace() {
                            break
                        }
                    }
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

            fn tracked_len_pos(&self) -> (usize, usize) {
                (self.failures.len(), self.fail_pos)
            }

            fn expected(&mut self) -> (Vec<Rule>, usize) {
                self.failures.sort();
                self.failures.dedup();

                (self.failures.iter().cloned().collect(), self.fail_pos)
            }

            #[inline]
            fn stack(&self) -> &Vec<String> {
                &self.stack
            }

            #[inline]
            fn stack_mut(&mut self) -> &mut Vec<String> {
                &mut self.stack
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
            soi_eoi = { soi ~ ["a"] ~ eoi }
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
        let mut parser = Rdp::new(StringInput::new("(  ( ))(( () )() )() "));

        assert!(parser.expression());
        assert!(!parser.end());

        let queue = vec![
            Token::new(Rule::paren, 0, 7),
            Token::new(Rule::paren, 3, 6),
            Token::new(Rule::paren, 7, 18),
            Token::new(Rule::paren, 8, 14),
            Token::new(Rule::paren, 10, 12),
            Token::new(Rule::paren, 14, 16),
            Token::new(Rule::paren, 18, 20)
        ];

        assert_eq!(parser.queue(), &queue);
    }

    #[test]
    fn whitespace_zero() {
        let mut parser = Rdp::new(StringInput::new("a a aa aaaa a  "));

        assert!(parser.zero());
        assert!(!parser.end());

        let queue = vec![
            Token::new(Rule::zero, 0, 13)
        ];

        assert_eq!(parser.queue(), &queue);
    }

    #[test]
    fn whitespace_one() {
        let mut parser = Rdp::new(StringInput::new("a a aa aaaa a  "));

        assert!(parser.one());
        assert!(!parser.end());

        let queue = vec![Token::new(Rule::one, 0, 13)];

        assert_eq!(parser.queue(), &queue);
    }

    #[test]
    fn comment() {
        let mut parser = Rdp::new(StringInput::new("((// hi\n))"));

        assert!(parser.expression());
        assert!(parser.end());

        let queue = vec![
            Token::new(Rule::paren, 0, 10),
            Token::new(Rule::paren, 1, 9)
        ];

        assert_eq!(parser.queue(), &queue);
    }

    #[test]
    fn comment_whitespace() {
        let mut parser = Rdp::new(StringInput::new("((  // hi\n  ))"));

        assert!(parser.expression());
        assert!(parser.end());

        let queue = vec![
            Token::new(Rule::paren, 0, 14),
            Token::new(Rule::paren, 1, 13)
        ];

        assert_eq!(parser.queue(), &queue);
    }

    #[test]
    fn soi_eoi() {
        let mut parser = Rdp::new(StringInput::new("  a  "));

        assert!(parser.soi_eoi());
        assert!(parser.end());
    }

    #[test]
    fn queue_with_captures() {
        let mut parser = Rdp::new(StringInput::new("(  ( ))(( () )() )() "));

        assert!(parser.expression());
        assert!(!parser.end());

        let queue = vec![
            (Token::new(Rule::paren, 0, 7), "(  ( ))".to_owned()),
            (Token::new(Rule::paren, 3, 6), "( )".to_owned()),
            (Token::new(Rule::paren, 7, 18), "(( () )() )".to_owned()),
            (Token::new(Rule::paren, 8, 14), "( () )".to_owned()),
            (Token::new(Rule::paren, 10, 12), "()".to_owned()),
            (Token::new(Rule::paren, 14, 16), "()".to_owned()),
            (Token::new(Rule::paren, 18, 20), "()".to_owned()),
        ];

        assert_eq!(parser.queue_with_captures(), queue);
    }
}
