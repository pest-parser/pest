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
/// # use pest::Input;
/// # use pest::Parser;
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
        #[allow(non_camel_case_types)]
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        pub enum Rules {
            $( $name(usize, usize) ),*
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
            input: Box<Input>,
            committed: VecDeque<Rules>,
            uncommitted: VecDeque<Rules>,
            commit: bool
        }

        impl_rdp!(@filter [ $( $ts )* ] []);

        impl Rdp {
            pub fn new(input: Box<Input>) -> Rdp {
                Rdp {
                    input: input,
                    committed: VecDeque::new(),
                    uncommitted: VecDeque::new(),
                    commit: false
                }
            }

            impl_rdp!(@ws $( $ts )*);

            grammar! {
                $( $ts )*
            }
        }

        impl Parser for Rdp {
            type Rules = Rules;

            fn matches(&mut self, string: &str) -> bool {
                self.input.matches(string)
            }

            fn try(&mut self, revert: bool, rule: Box<Fn(&mut Self) -> bool>) -> bool {
                let pos = self.input.pos();
                let commit = self.commit;

                if commit {
                    self.commit = false;
                }

                let result = rule(self);

                if revert || !result {
                    self.input.set_pos(pos);
                }

                if commit {
                    self.commit = true;

                    if result {
                        self.committed.append(&mut self.uncommitted);
                    }
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
            }

            fn queue(&mut self) -> &mut VecDeque<Rules>{
                if self.commit {
                    &mut self.committed
                } else {
                    &mut self.uncommitted
                }
            }

            fn skip_ws(&mut self) {
                loop {
                    if !self.ws() {
                        break
                    }
                }
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use super::super::super::Parser;
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

        assert!(!parser.try(false, Box::new(|parser| {
            parser.matches("as") && parser.matches("dd")
        })));

        assert!(parser.try(false, Box::new(|parser| {
            parser.matches("as") && parser.matches("df")
        })));
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
            Rules::paren(2, 7),
            Rules::paren(5, 3),
            Rules::paren(9, 11),
            Rules::paren(10, 6),
            Rules::paren(12, 2),
            Rules::paren(16, 2),
            Rules::paren(20, 2)
        ];

        assert!(parser.queue().iter().eq(&queue));
    }

    #[test]
    fn ws_zero() {
        let mut parser = Rdp::new(Box::new(StringInput::new("  a a aa aaaa a  ")));

        assert!(parser.zero());
        assert!(!parser.end());

        let queue = vec![
            Rules::zero(2, 13)
        ];

        assert!(parser.queue().iter().eq(&queue));
    }

    #[test]
    fn ws_one() {
        let mut parser = Rdp::new(Box::new(StringInput::new("  a a aa aaaa a  ")));

        assert!(parser.one());
        assert!(!parser.end());

        let queue = vec![
            Rules::one(2, 13)
        ];

        assert!(parser.queue().iter().eq(&queue));
    }
}
