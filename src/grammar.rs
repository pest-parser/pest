// pest. Smart PEGs in Rust
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// A `macro` that defines each rule as a method on a `Parser`. Rules starting with an uderscore
/// will not be placed into the queue.
///
/// `ws` is a special rules used exclusively for white-space.
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
///         exp = _{ paren ~ exp | [""] }
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
    ( @mtc $slf:ident [ $left:tt .. $right:tt ]) => (grammar!(@mtc $slf [$left, $right]));
    ( @mtc $slf:ident [ $left:expr, $right:expr ]) => ($slf.between($left, $right));
    ( @mtc $slf:ident [ $str:expr ]) => ($slf.matches($str));
    ( @mtc $slf:ident $rule:ident) => ($slf.$rule());

    // process postfix
    ( @process $_slf:ident [( $result:expr )] [] ) => ($result);
    ( @process $slf:ident [ $b:tt $a:tt $( $tail:tt )* ] [ ~ $( $optail:tt )* ] ) => {
        {
            grammar!(@process $slf [(( $slf.try(false, |$slf| {
                if grammar!(@mtc $slf $a) {
                    $slf.skip_ws();

                    grammar!(@mtc $slf $b)
                } else {
                    false
                }
            }) )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $slf:ident [ $b:tt $a:tt $( $tail:tt )* ] [ | $( $optail:tt )* ] ) => {
        {
            grammar!(@process $slf [((
                grammar!(@mtc $slf $a) || grammar!(@mtc $slf $b)
            )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $slf:ident [ $a:tt $( $tail:tt )* ] [ * $( $optail:tt )* ] ) => {
        {
            grammar!(@process $slf [(( {
                let mut pos = $slf.pos();

                loop {
                    if !grammar!(@mtc $slf $a) {
                        $slf.set_pos(pos);

                        break
                    }

                    pos = $slf.pos();

                    $slf.skip_ws();
                }

                true
            } )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $slf:ident [ $a:tt $( $tail:tt )* ] [ + $( $optail:tt )* ] ) => {
        {
            grammar!(@process $slf [(( if grammar!(@mtc $slf $a) {
                loop {
                    let pos = $slf.pos();

                    $slf.skip_ws();

                    if !grammar!(@mtc $slf $a) {
                        $slf.set_pos(pos);

                        break
                    }
                }

                true
            } else {
                false
            } )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $slf:ident [ $a:tt $( $tail:tt )* ] [ ? $( $optail:tt )* ] ) => {
        {
            grammar!(@process $slf [(( {
                grammar!(@mtc $slf $a);

                true
            } )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $slf:ident [ $a:tt $( $tail:tt )* ] [ & $( $optail:tt )* ] ) => {
        {
            grammar!(@process $slf [(( $slf.try(true, |$slf| {
                grammar!(@mtc $slf $a)
            }) )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $slf:ident [ $a:tt $( $tail:tt )* ] [ ! $( $optail:tt )* ] ) => {
        {
            grammar!(@process $slf [(( $slf.try(true, |$slf| {
                !grammar!(@mtc $slf $a)
            }) )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $slf:ident [] [ $single:tt ] ) => {
        grammar!(@mtc $slf $single)
    };
    ( @process $slf:ident [ $( $optail:tt )* ] [ $head:tt $( $tail:tt )* ] ) => {
        grammar!(@process $slf [ $head $( $optail )* ] [ $( $tail )* ])
    };

    // skip only if not ws
    ( @skip ws $_slf:ident ) => ();
    ( @skip $_name:ident $slf:ident ) => ($slf.skip_ws());

    () => {
        #[allow(dead_code)]
        pub fn eoi(&mut self) -> bool {
            self.end()
        }
    };

    ( $name:ident = { $( $ts:tt )* } $( $tail:tt )* ) => {
        #[allow(unused_parens)]
        #[allow(unused_variables)]
        pub fn $name(&mut self) -> bool {
            let slf = self;
            grammar!(@skip $name slf);

            let pos = slf.pos();
            let queue_pos = slf.queue().len();

            let result = grammar!(@conv slf [ $( $ts )* ] [] []);

            if result {
                let new_pos = slf.pos();

                slf.queue().insert(queue_pos, Rules::$name(pos, new_pos - pos));
            }

            result
        }

        grammar!($( $tail )*);
    };

    ( $name:ident = _{ $( $ts:tt )* } $( $tail:tt )* ) => {
        #[allow(unused_parens)]
        #[allow(unused_variables)]
        pub fn $name(&mut self) -> bool {
            let slf = self;
            grammar!(@skip $name slf);

            let pos = slf.pos();

            let result = grammar!(@conv slf [ $( $ts )* ] [] []);

            result
        }

        grammar!($( $tail )*);
    };
}
