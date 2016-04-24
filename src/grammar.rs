// pest. Smart PEGs in Rust
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// A `macro` that defines each rule as a method on a `Parser`.
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
/// impl MyRdp {
///     grammar! {
///         exp = { paren ~ exp | [""] }
///         paren = { ["("] ~ exp ~ [")"] }
///     }
/// }
///
/// let mut parser = MyRdp::new(Box::new(StringInput::new("(())((())())()")));
///
/// assert!(parser.exp());
/// assert!(parser.end());
/// # }
/// ```
#[macro_export]
macro_rules! grammar {
    // handle parens
    (@conv $slf:ident ( ( $( $head:tt )* ) $( $tail:tt )* ) ( $( $optail:tt )* )
     ( $( $output:tt )* )) => {
        grammar!(@conv $slf ( $( $head )* @rp $( $tail )* ) ( @lp $( $optail )* )
                 ( $( $output )* ))
    };
    (@conv $slf:ident ( @rp $( $tail:tt )* ) ( @lp $( $optail:tt )* ) ( $( $output:tt )* )) => {
        grammar!(@conv $slf ( $( $tail )* ) ( $( $optail )* ) ( $( $output )* ))
    };
    (@conv $slf:ident ( @rp $( $tail:tt )* ) ( $op:tt $( $optail:tt )* ) ( $( $output:tt )* )) => {
        grammar!(@conv $slf ( @rp $( $tail )* ) ( $( $optail )* ) ( $( $output )* $op ))
    };

    // handle operands
    (@conv $slf:ident ( ~ $( $tail:tt )* ) ( ~ $( $optail:tt )* ) ( $( $output:tt )* )) => {
        grammar!(@conv $slf ( ~ $( $tail )* ) ( $( $optail )* ) ( $( $output )* ~ ))
    };
    (@conv $slf:ident ( ~ $( $tail:tt )* ) ( $( $optail:tt )* ) $output:tt) => {
        grammar!(@conv $slf ( $( $tail )* ) ( ~ $( $optail )* ) $output)
    };
    (@conv $slf:ident ( | $( $tail:tt )* ) ( ~ $( $optail:tt )* ) ( $( $output:tt )* )) => {
        grammar!(@conv $slf ( | $( $tail )* ) ( $( $optail )* ) ( $( $output )* ~ ))
    };
    (@conv $slf:ident ( | $( $tail:tt )* ) ( | $( $optail:tt )* ) ( $( $output:tt )* )) => {
        grammar!(@conv $slf ( | $( $tail )* ) ( $( $optail )* ) ( $( $output )* | ))
    };
    (@conv $slf:ident ( | $( $tail:tt )* ) ( $( $optail:tt )* ) $output:tt) => {
        grammar!(@conv $slf ( $( $tail )* ) ( | $( $optail )* ) $output)
    };

    // handle everything else
    (@conv $slf:ident ( $head:tt $( $tail:tt )* ) $ops:tt ( $( $output:tt )* )) => {
        grammar!(@conv $slf ( $( $tail )* ) $ops ( $( $output )* $head ))
    };

    // output remaining operators
    (@conv $slf:ident () () ( $( $output:tt )* )) => {
        grammar!(@process $slf () ($( $output )*))
    };
    (@conv $slf:ident () ( $op:tt $( $optail:tt )* ) ( $( $output:tt )* )) => {
        grammar!(@conv $slf () ( $( $optail )* ) ( $( $output )* $op ))
    };

    // match
    (@mtc $slf:ident (( $exp:expr ))) => (($exp));
    (@mtc $slf:ident [ $str:expr ]) => ($slf.matches($str));
    (@mtc $slf:ident $rule:ident) => ($slf.$rule());

    // process postfix
    (@process $_slf:ident (( $result:expr )) ()) => ($result);
    (@process $slf:ident ( $b:tt $a:tt $( $tail:tt )* ) ( ~ $( $optail:tt )* )) => {
        {
            let try = $slf.try(Box::new(move |parser| {
                grammar!(@mtc parser $a) && grammar!(@mtc parser $b)
            }));

            grammar!(@process $slf ((( try )) $( $tail )* ) ( $( $optail )* ))
        }
    };
    (@process $slf:ident ( $b:tt $a:tt $( $tail:tt )* ) ( | $( $optail:tt )* )) => {
        {
            let a = grammar!(@mtc $slf $a);
            let b = grammar!(@mtc $slf $b);

            grammar!(@process $slf ((( a || b )) $( $tail )* ) ( $( $optail )* ))
        }
    };
    (@process $slf:ident () ( $single:tt )) => {
        grammar!(@mtc $slf $single)
    };
    (@process $slf:ident ( $( $optail:tt )* ) ( $head:tt $( $tail:tt )* )) => {
        grammar!(@process $slf ( $head $( $optail )* ) ( $( $tail )* ))
    };

    ( $( $name:ident = { $( $ts:tt )* } )* ) => {
        $(
            #[allow(unused_parens)]
            #[allow(unused_variables)]
            pub fn $name(&mut self) -> bool {
                grammar!(@conv self ( $( $ts )* ) () ())
            }
        )*
    };
}

#[cfg(test)]
mod tests {
    use super::super::Rdp;
    use super::super::Parser;
    use super::super::Input;
    use super::super::StringInput;

    impl_rdp!(MyRdp);

    impl MyRdp {
        grammar! {
            exp = { paren ~ exp | [""] }
            paren = { ["("] ~ exp ~ [")"] }
        }
    }

    #[test]
    fn basic() {
        let mut parser = MyRdp::new(Box::new(StringInput::new("(())((())())()")));

        assert!(parser.exp());
        assert!(parser.end());
    }

    #[test]
    fn fail() {
        let mut parser = MyRdp::new(Box::new(StringInput::new("(())((())())(")));

        assert!(parser.exp());
        assert!(!parser.end());
    }
}
