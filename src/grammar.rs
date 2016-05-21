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
/// # use pest::Parser;
/// # use pest::StringInput;
/// # fn main() {
/// impl_rdp! {
///     grammar! {
///         exp = { paren ~ exp | [""] }
///         paren = { ["("] ~ exp ~ [")"] }
///     }
/// }
///
/// let mut parser = Rdp::new(Box::new(StringInput::new("(())((())())()")));
///
/// assert!(parser.exp());
/// assert!(parser.end());
/// # }
/// ```
#[macro_export]
macro_rules! grammar {
    // handle parens
    ( @conv $slf:ident [ ( $( $head:tt )* ) $( $tail:tt )* ] [ $( $optail:tt )* ]
      [ $( $output:tt )* ] ) => {
        grammar!(@conv $slf [ $( $head )* @rp $( $tail )* ] [ @lp $( $optail )* ]
                 [ $( $output )* ])
    };
    ( @conv $slf:ident [ @rp $( $tail:tt )* ] [ @lp $( $optail:tt )* ] [ $( $output:tt )* ] ) => {
        grammar!(@conv $slf [ $( $tail )* ] [ $( $optail )* ] [ $( $output )* ])
    };
    ( @conv $slf:ident [ @rp $( $tail:tt )* ] [ $op:tt $( $optail:tt )* ]
      [ $( $output:tt )* ] ) => {
        grammar!(@conv $slf [ @rp $( $tail )* ] [ $( $optail )* ] [ $( $output )* $op ])
    };

    // handle prefix operands
    ( @conv $slf:ident [ & $head:tt $( $tail:tt )* ] [ $( $optail:tt )* ]
      [ $( $output:tt )* ] ) => {
        grammar!(@conv $slf [ $( $tail )* ] [ $( $optail )* ] [ $( $output )* $head & ])
    };
    ( @conv $slf:ident [ ! $head:tt $( $tail:tt )* ] [ $( $optail:tt )* ]
      [ $( $output:tt )* ] ) => {
        grammar!(@conv $slf [ $( $tail )* ] [ $( $optail )* ] [ $( $output )* $head ! ])
    };

    // handle infix operands
    ( @conv $slf:ident [ ~ $( $tail:tt )* ] [ ~ $( $optail:tt )* ] [ $( $output:tt )* ] ) => {
        grammar!(@conv $slf [ ~ $( $tail )* ] [ $( $optail )* ] [ $( $output )* ~ ])
    };
    ( @conv $slf:ident [ ~ $( $tail:tt )* ] [ $( $optail:tt )* ] $output:tt) => {
        grammar!(@conv $slf [ $( $tail )* ] [ ~ $( $optail )* ] $output)
    };
    ( @conv $slf:ident [ | $( $tail:tt )* ] [ ~ $( $optail:tt )* ] [ $( $output:tt )* ] ) => {
        grammar!(@conv $slf [ | $( $tail )* ] [ $( $optail )* ] [ $( $output )* ~ ])
    };
    ( @conv $slf:ident [ | $( $tail:tt )* ] [ | $( $optail:tt )* ] [ $( $output:tt )* ] ) => {
        grammar!(@conv $slf [ | $( $tail )* ] [ $( $optail )* ] [ $( $output )* | ])
    };
    ( @conv $slf:ident [ | $( $tail:tt )* ] [ $( $optail:tt )* ] $output:tt) => {
        grammar!(@conv $slf [ $( $tail )* ] [ | $( $optail )* ] $output)
    };

    // handle everything else
    ( @conv $slf:ident [ $head:tt $( $tail:tt )* ] $ops:tt [ $( $output:tt )* ] ) => {
        grammar!(@conv $slf [ $( $tail )* ] $ops [ $( $output )* $head ])
    };

    // output remaining operators
    ( @conv $slf:ident [] [] [ $( $output:tt )* ] ) => {
        grammar!(@process $slf [] [ $( $output )* ])
    };
    ( @conv $slf:ident [] [ $op:tt $( $optail:tt )* ] [ $( $output:tt )* ] ) => {
        grammar!(@conv $slf [] [ $( $optail )* ] [ $( $output )* $op ])
    };

    // match
    ( @mtc $slf:ident (( $exp:expr )) ) => (($exp));
    ( @mtc $slf:ident [ $str:expr ]) => ($slf.matches($str));
    ( @mtc $slf:ident $rule:ident) => ($slf.$rule());

    // process postfix
    ( @process $_slf:ident [( $result:expr )] [] ) => ($result);
    ( @process $slf:ident [ $b:tt $a:tt $( $tail:tt )* ] [ ~ $( $optail:tt )* ] ) => {
        {
            let result = $slf.try(false, Box::new(move |parser| {
                grammar!(@mtc parser $a) && grammar!(@mtc parser $b)
            }));

            grammar!(@process $slf [(( result )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $slf:ident [ $b:tt $a:tt $( $tail:tt )* ] [ | $( $optail:tt )* ] ) => {
        {
            let result = grammar!(@mtc $slf $a) || grammar!(@mtc $slf $b);

            grammar!(@process $slf [(( result )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $slf:ident [ $a:tt $( $tail:tt )* ] [ * $( $optail:tt )* ] ) => {
        {
            let result = {
                loop {
                    if !grammar!(@mtc $slf $a) {
                        break
                    }
                }

                true
            };

            grammar!(@process $slf [(( result )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $slf:ident [ $a:tt $( $tail:tt )* ] [ + $( $optail:tt )* ] ) => {
        {
            let result = if grammar!(@mtc $slf $a) {
                loop {
                    if !grammar!(@mtc $slf $a) {
                        break
                    }
                }

                true
            } else {
                false
            };

            grammar!(@process $slf [(( result )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $slf:ident [ $a:tt $( $tail:tt )* ] [ ? $( $optail:tt )* ] ) => {
        {
            let result = {
                grammar!(@mtc $slf $a);

                true
            };

            grammar!(@process $slf [(( result )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $slf:ident [ $a:tt $( $tail:tt )* ] [ & $( $optail:tt )* ] ) => {
        {
            let result = $slf.try(true, Box::new(move |parser| {
                grammar!(@mtc parser $a)
            }));

            grammar!(@process $slf [(( result )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $slf:ident [ $a:tt $( $tail:tt )* ] [ ! $( $optail:tt )* ] ) => {
        {
            let result = $slf.try(true, Box::new(move |parser| {
                !grammar!(@mtc parser $a)
            }));

            grammar!(@process $slf [(( result )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $slf:ident [] [ $single:tt ] ) => {
        grammar!(@mtc $slf $single)
    };
    ( @process $slf:ident [ $( $optail:tt )* ] [ $head:tt $( $tail:tt )* ] ) => {
        grammar!(@process $slf [ $head $( $optail )* ] [ $( $tail )* ])
    };

    () => ();

    ( $name:ident = { $( $ts:tt )* } $( $tail:tt )* ) => {
        #[allow(unused_parens)]
        #[allow(unused_variables)]
        pub fn $name(&mut self) -> bool {
            let pos = self.pos();
            let queue_pos = self.queue().len();
            let result = grammar!(@conv self [ $( $ts )* ] [] []);

            if result {
                let new_pos = self.pos();

                self.queue().insert(queue_pos, Rules::$name(pos, new_pos - pos));
            }

            result
        }

        grammar!($( $tail )*);
    };

    ( $name:ident = _{ $( $ts:tt )* } $( $tail:tt )* ) => {
        #[allow(unused_parens)]
        #[allow(unused_variables)]
        pub fn $name(&mut self) -> bool {
            let pos = self.pos();
            let result = grammar!(@conv self [ $( $ts )* ] [] []);

            result
        }

        grammar!($( $tail )*);
    };
}
