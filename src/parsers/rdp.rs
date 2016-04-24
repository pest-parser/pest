// pest. Smart PEGs in Rust
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use super::super::Input;
use super::super::Parser;

/// A `macro` useful for implementing the `Rdp` `trait`.
///
/// # Examples
///
/// ```
/// # #[macro_use] extern crate pest;
/// # use pest::Input;
/// # use pest::StringInput;
/// # use pest::Rdp;
///
/// # fn main() {
/// impl_rdp!(MyRdp);
///
/// let input = Box::new(StringInput::new("asdasdf"));
/// let mut parser = MyRdp::new(input);
///
/// assert!(parser.matches("asd"));
/// assert!(parser.matches("asdf"));
/// # }
/// ```
#[macro_export]
macro_rules! impl_rdp {
    ( $name:ident ) => {
        pub struct $name {
            input: Box<Input>
        }

        impl Rdp for $name {
            fn new(input: Box<Input>) -> $name {
                $name {
                    input: input
                }
            }

            fn input(&mut self) -> &mut Box<Input> {
                &mut self.input
            }
        }
    };
}

/// A `trait` that implements a recursive descent parser on a `struct` containing an `Input`.
pub trait Rdp {
    /// Creates a new `Rdp::Self` instance.
    ///
    /// Is subject to change in order to enhance flexibility.
    fn new(input: Box<Input>) -> Self;

    /// Returns `&mut Box<Input>` in order for the parser to have access to its input.
    fn input(&mut self) -> &mut Box<Input>;

    fn matches(&mut self, string: &str) -> bool {
        self.input().matches(string)
    }

    fn try(&mut self, rule: Box<Fn(&mut Self) -> bool>) -> bool {
        let pos = self.input().pos();
        let result = rule(self);

        if !result {
            self.input().set_pos(pos);
        }

        result
    }

    fn end(&mut self) -> bool {
        self.input().len() == self.input().pos()
    }

    fn reset(&mut self) {
        self.input().set_pos(0);
    }
}

#[cfg(test)]
mod tests {
    use super::Rdp;
    use super::super::super::Parser;
    use super::super::super::Input;
    use super::super::super::StringInput;

    impl_rdp!(MyRdp);

    #[test]
    fn matches() {
        let input = Box::new(StringInput::new("asdasdf"));
        let mut parser = MyRdp::new(input);

        assert!(parser.matches("asd"));
        assert!(parser.matches("asdf"));
        assert!(parser.matches(""));
        assert!(!parser.matches("a"));
    }

    #[test]
    fn try() {
        let input = Box::new(StringInput::new("asdasdf"));
        let mut parser = MyRdp::new(input);

        assert!(parser.matches("asd"));

        assert!(!parser.try(Box::new(|parser| {
            parser.matches("as") && parser.matches("dd")
        })));

        assert!(parser.try(Box::new(|parser| {
            parser.matches("as") && parser.matches("df")
        })));
    }

    #[test]
    fn end() {
        let input = Box::new(StringInput::new("asdasdf"));
        let mut parser = MyRdp::new(input);

        assert!(parser.matches("asdasdf"));
        assert!(parser.end());
    }

    #[test]
    fn reset() {
        let input = Box::new(StringInput::new("asdasdf"));
        let mut parser = MyRdp::new(input);

        assert!(parser.matches("asdasdf"));

        parser.reset();

        assert!(parser.matches("asdasdf"));
    }
}
