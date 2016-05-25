// pest. Smart PEGs in Rust
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// A `macro` that defines each rule as a method on a `Parser`. Rules starting with an uderscore
/// will not be placed into the queue.
///
/// `whitespace` is a special rules used exclusively for white-space.
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
    ( @conv $atomic:tt $slf:ident [ ( $( $head:tt )* ) $( $tail:tt )* ] [ $( $optail:tt )* ]
      [ $( $output:tt )* ] ) => {
        grammar!(@conv $atomic $slf [ $( $head )* _rp $( $tail )* ] [ _lp $( $optail )* ]
                 [ $( $output )* ])
    };
    ( @conv $atomic:tt $slf:ident [ _rp $( $tail:tt )* ] [ _lp $( $optail:tt )* ]
      [ $( $output:tt )* ] ) => {
        grammar!(@conv $atomic $slf [ $( $tail )* ] [ $( $optail )* ] [ $( $output )* ])
    };
    ( @conv $atomic:tt $slf:ident [ _rp $( $tail:tt )* ] [ $op:tt $( $optail:tt )* ]
      [ $( $output:tt )* ] ) => {
        grammar!(@conv $atomic $slf [ _rp $( $tail )* ] [ $( $optail )* ] [ $( $output )* $op ])
    };

    // handle prefix operands
    ( @conv $atomic:tt $slf:ident [ & $head:tt $( $tail:tt )* ] [ $( $optail:tt )* ]
      $output:tt ) => {
        grammar!(@conv $atomic $slf [ $head _pres $( $tail )* ] [ $( $optail )* ] $output)
    };
    ( @conv $atomic:tt $slf:ident [ ! $head:tt $( $tail:tt )* ] [ $( $optail:tt )* ]
      $output:tt ) => {
        grammar!(@conv $atomic $slf [ $head _abs $( $tail )* ] [ $( $optail )* ] $output)
    };

    // handle infix operands
    ( @conv $atomic:tt $slf:ident [ ~ $( $tail:tt )* ] [ ~ $( $optail:tt )* ]
      [ $( $output:tt )* ] ) => {
        grammar!(@conv $atomic $slf [ ~ $( $tail )* ] [ $( $optail )* ] [ $( $output )* ~ ])
    };
    ( @conv $atomic:tt $slf:ident [ ~ $( $tail:tt )* ] [ $( $optail:tt )* ] $output:tt) => {
        grammar!(@conv $atomic $slf [ $( $tail )* ] [ ~ $( $optail )* ] $output)
    };
    ( @conv $atomic:tt $slf:ident [ | $( $tail:tt )* ] [ ~ $( $optail:tt )* ]
      [ $( $output:tt )* ] ) => {
        grammar!(@conv $atomic $slf [ | $( $tail )* ] [ $( $optail )* ] [ $( $output )* ~ ])
    };
    ( @conv $atomic:tt $slf:ident [ | $( $tail:tt )* ] [ | $( $optail:tt )* ]
      [ $( $output:tt )* ] ) => {
        grammar!(@conv $atomic $slf [ | $( $tail )* ] [ $( $optail )* ] [ $( $output )* | ])
    };
    ( @conv $atomic:tt $slf:ident [ | $( $tail:tt )* ] [ $( $optail:tt )* ] $output:tt) => {
        grammar!(@conv $atomic $slf [ $( $tail )* ] [ | $( $optail )* ] $output)
    };

    // handle everything else
    ( @conv $atomic:tt $slf:ident [ $head:tt $( $tail:tt )* ] $ops:tt [ $( $output:tt )* ] ) => {
        grammar!(@conv $atomic $slf [ $( $tail )* ] $ops [ $( $output )* $head ])
    };

    // output remaining operators
    ( @conv $atomic:tt $slf:ident [] [] [ $( $output:tt )* ] ) => {
        grammar!(@process $atomic $slf [] [ $( $output )* ])
    };
    ( @conv $atomic:tt $slf:ident [] [ $op:tt $( $optail:tt )* ] [ $( $output:tt )* ] ) => {
        grammar!(@conv $atomic $slf [] [ $( $optail )* ] [ $( $output )* $op ])
    };

    // match
    ( @mtc $slf:ident (( $exp:expr )) ) => (($exp));
    ( @mtc $slf:ident [ $left:tt .. $right:tt ]) => (grammar!(@mtc $slf [$left, $right]));
    ( @mtc $slf:ident [ $left:expr, $right:expr ]) => ($slf.between($left, $right));
    ( @mtc $slf:ident [ $str:expr ]) => ($slf.matches($str));
    ( @mtc $slf:ident $rule:ident) => ($slf.$rule());

    // process postfix
    ( @process $_atomic:tt $_slf:ident [( $result:expr )] [] ) => ($result);
    ( @process false $slf:ident [ $b:tt $a:tt $( $tail:tt )* ] [ ~ $( $optail:tt )* ] ) => {
        {
            grammar!(@process false $slf [(( $slf.try(false, |$slf| {
                if grammar!(@mtc $slf $a) {
                    $slf.skip_ws();

                    grammar!(@mtc $slf $b)
                } else {
                    false
                }
            }) )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process true $slf:ident [ $b:tt $a:tt $( $tail:tt )* ] [ ~ $( $optail:tt )* ] ) => {
        {
            grammar!(@process true $slf [(( $slf.try(false, |$slf| {
                grammar!(@mtc $slf $a) && grammar!(@mtc $slf $b)
            }) )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $atomic:tt $slf:ident [ $b:tt $a:tt $( $tail:tt )* ] [ | $( $optail:tt )* ] ) => {
        {
            grammar!(@process $atomic $slf [((
                grammar!(@mtc $slf $a) || grammar!(@mtc $slf $b)
            )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process false $slf:ident [ $a:tt $( $tail:tt )* ] [ * $( $optail:tt )* ] ) => {
        {
            grammar!(@process false $slf [(( {
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
    ( @process true $slf:ident [ $a:tt $( $tail:tt )* ] [ * $( $optail:tt )* ] ) => {
        {
            grammar!(@process true $slf [(( {
                loop {
                    if !grammar!(@mtc $slf $a) {
                        break
                    }
                }

                true
            } )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process false $slf:ident [ $a:tt $( $tail:tt )* ] [ + $( $optail:tt )* ] ) => {
        {
            grammar!(@process false $slf [(( if grammar!(@mtc $slf $a) {
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
    ( @process true $slf:ident [ $a:tt $( $tail:tt )* ] [ + $( $optail:tt )* ] ) => {
        {
            grammar!(@process true $slf [(( if grammar!(@mtc $slf $a) {
                loop {
                    if !grammar!(@mtc $slf $a) {
                        break
                    }
                }

                true
            } else {
                false
            } )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $atomic:tt $slf:ident [ $a:tt $( $tail:tt )* ] [ ? $( $optail:tt )* ] ) => {
        {
            grammar!(@process $atomic $slf [(( {
                grammar!(@mtc $slf $a);

                true
            } )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $atomic:tt $slf:ident [ $a:tt $( $tail:tt )* ] [ _pres $( $optail:tt )* ] ) => {
        {
            grammar!(@process $atomic $slf [(( $slf.try(true, |$slf| {
                grammar!(@mtc $slf $a)
            }) )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $atomic:tt $slf:ident [ $a:tt $( $tail:tt )* ] [ _abs $( $optail:tt )* ] ) => {
        {
            grammar!(@process $atomic $slf [(( $slf.try(true, |$slf| {
                !grammar!(@mtc $slf $a)
            }) )) $( $tail )* ] [ $( $optail )* ])
        }
    };
    ( @process $_atomic:tt $slf:ident [] [ $single:tt ] ) => {
        grammar!(@mtc $slf $single)
    };
    ( @process $atomic:tt $slf:ident [ $( $optail:tt )* ] [ $head:tt $( $tail:tt )* ] ) => {
        grammar!(@process $atomic $slf [ $head $( $optail )* ] [ $( $tail )* ])
    };

    // skip only if not whitespace
    ( @skip whitespace $_slf:ident )  => ();
    ( @skip comment $slf:ident )      => ($slf.skip_ws());
    ( @skip $_name:ident $slf:ident ) => {
        {
            $slf.skip_com();
            $slf.skip_ws();
        }
    };

    // whitespace is always atomic
    ( @atomic whitespace $_atomic:tt $slf:ident $rules:tt ) => {
        grammar!(@conv true $slf $rules [] [])
    };
    ( @atomic $_name:ident $atomic:tt $slf:ident $rules:tt ) => {
        grammar!(@conv $atomic $slf $rules [] [])
    };

    () => {
        #[allow(dead_code)]
        pub fn any(&mut self) -> bool {
            if self.end() {
                let pos = self.pos();

                self.track(Rule::any, pos);

                false
            } else {
                let next = self.pos() + 1;
                self.set_pos(next);

                true
            }
        }

        #[allow(dead_code)]
        pub fn eoi(&mut self) -> bool {
            let result = self.end();

            if !result {
                let pos = self.pos();

                self.track(Rule::eoi, pos);
            }

            result
        }
    };

    // normal rule
    ( $name:ident = { $( $ts:tt )* } $( $tail:tt )* ) => {
        #[allow(unused_parens, unused_variables)]
        pub fn $name(&mut self) -> bool {
            let slf = self;
            grammar!(@skip $name slf);

            let pos = slf.pos();
            let queue_pos = slf.queue().len();

            let result = grammar!(@atomic $name false slf [ $( $ts )* ]);

            if result {
                let new_pos = slf.pos();

                let token = Token {
                    rule:  Rule::$name,
                    start: pos,
                    end:   new_pos
                };

                slf.queue().insert(queue_pos, token);
            } else {
                slf.track(Rule::$name, pos);
            }

            result
        }

        grammar!($( $tail )*);
    };

    // atomic rule
    ( $name:ident = @{ $( $ts:tt )* } $( $tail:tt )* ) => {
        #[allow(unused_parens, unused_variables)]
        pub fn $name(&mut self) -> bool {
            let slf = self;
            grammar!(@skip $name slf);

            let pos = slf.pos();
            let queue_pos = slf.queue().len();

            let result = grammar!(@conv true slf [ $( $ts )* ] [] []);

            if result {
                let new_pos = slf.pos();

                let token = Token {
                    rule:  Rule::$name,
                    start: pos,
                    end:   new_pos
                };

                slf.queue().insert(queue_pos, token);
            } else {
                slf.track(Rule::$name, pos);
            }

            result
        }

        grammar!($( $tail )*);
    };

    // quiet rule
    ( $name:ident = _{ $( $ts:tt )* } $( $tail:tt )* ) => {
        #[allow(unused_parens, unused_variables)]
        pub fn $name(&mut self) -> bool {
            let slf = self;
            grammar!(@skip $name slf);

            let pos = slf.pos();

            let result = grammar!(@atomic $name false slf [ $( $ts )* ]);

            result
        }

        grammar!($( $tail )*);
    };
}
