// pest. Elegant, efficient grammars
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// A `macro` that defines each rule as a method on a `Parser` which parses from the current
/// position. Rules are always defined between braces, with an optional symbol marking the type of
/// rule defined.
///
/// *Note:* `grammar!` may require you to increase the recursion limit of your create with
/// `#![recursion_limit = "*"]` where * is the new limit.
///
/// pest has four special rules:
///
/// * `whitespace` - gets run between rules and sub-rules
/// * `comment` - gets run only between rules
/// * `any` - matches exactly one `char`
/// * `eoi` - (end-of-input) matches only when a `Parser` has reached its
///   [end](trait.Parser#tymethod.end)
///
/// `whitespace` and `comment` should be overridden and are void otherwise, while `any` and `eoi`
/// are predefined rules.
///
/// # Normal rules
///
/// A normal rule will always create a [`Token`](struct.Token) and add it to the
/// [`Parser::queue`](trait.Parser#tymethod.queue), along with any subsequent rules that its
/// subrules may create.
///
/// ```ignore
/// one = { two }
/// ```
///
/// If the above rule matches and `two` is a normal rule as well, the queue of `Token`s will
/// contain `Token { rule: Rule::one, ... }, Token { rule: Rule::two, ... }`.
///
/// Normal rules are also tracked for error reporting. Once a normal rule fails, its failure is
/// automatically recorded and available in [`Parser::expected`](trait.Parser#tymethod.expected).
///
/// # Atomic rules `@`
///
/// Atomic rules work as normal rules apart from the fact that they don't accept any `whitespace`
/// or `comment` between rules and sub-rules, and any sub-rules of an atomic rule will not appear
/// in [`Parser::expected`](trait.Parser#tymethod.expected).
///
/// ```ignore
/// ab = @{ a ~ b }
/// ```
///
/// In the rule above, there cannot be anything between `a` and `b` for `ab` to match. This rule
/// has a *cascading* effect, so any rules called from an atomic rule will also be atomic while
/// being matched in an atomic context. In other words, `a` and `b` will also be atomic when being
/// matched inside of `ab`.
///
/// # Silent rules `_`
///
/// Silent rules work like normal rules without appearing in
/// [`Parser::queue`](trait.Parser#tymethod.queue) or
/// [`Parser::expected`](trait.Parser#tymethod.expected).
///
/// ```ignore
/// whitespace = _{ [" "] }
/// ```
///
/// Unlike atomic rules, silent rules are *not cascading*. A rule inside a silent rule will not be
/// silent unless it's explicitly stated.
///
/// # Syntax
///
/// | Rule         | What it does                                         |
/// |--------------|------------------------------------------------------|
/// | `["a"]`      | matches the exact string `"a"`                       |
/// | `['a'..'z']` | matches one character between `'a'` and `'z'`        |
/// | `a`          | matches rule `a`                                     |
/// | `a ~ b`      | matches the sequence `a` `b`                         |
/// | `a | b`      | matches either `a` or `b`                            |
/// | `a*`         | matches `a` zero or more times                       |
/// | `a+`         | matches `a` one or more times                        |
/// | `a?`         | optionally matches `a`                               |
/// | `&a`         | matches `a` without making progress                  |
/// | `!a`         | matches if `a` doesn't match without making progress |
///
/// ## Precedence climbing
///
/// pest supports a special type of rule that implements precedence climbing in the background.
///
/// ```ignore
/// expression = _{
///     { ["("] ~ expression ~ [")"] | number }
///     addition       = {  plus  | minus }
///     multiplication = {  times | slash }
///     power          = {< pow } // < for right-associativity
/// }
/// ```
///
/// The first part of the precedence climbing rule is the primary expression that comes between
/// braces. It's followed by any number of rules, each rule having a precedence higher than the
/// previous one. The `<` denote right-associativity, the default being left-associativity.
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
macro_rules! grammar {
    // handle associativity
    ( @assoc < $( $ts:tt )* ) => (true);
    ( @assoc $( $ts:tt )* )   => (false);

    // handle right associativity rule
    ( @conv $atomic:tt $slf:ident [ < $( $ts:tt )* ] [] [] ) => {
        grammar!(@conv $atomic $slf [ $( $ts )* ] [] [])
    };

    // handle precedence climbing
    ( @conv_prec $pos:ident ($_prec:expr) $_atomic:tt $slf:ident [] [] [] ) => {
        {
            $slf.input_mut().set_pos($pos);

            None
        }
    };
    // normal
    ( @conv_prec $pos:ident ($prec:expr) $atomic:tt $slf:ident
      [ $name:ident = { $( $head:tt )* } $( $tail:tt )* ] [] [] ) => {
        {
            if grammar!(@conv $atomic $slf [ $( $head )* ] [] []) {
                return Some((Some(Rule::$name), $prec, grammar!(@assoc $( $head )*)))
            } else {
                grammar!(@conv_prec $pos ($prec + 1) $atomic $slf [ $( $tail )* ] [] [])
            }
        }
    };
    // atomic
    ( @conv_prec $pos:ident ($prec:expr) $atomic:tt $slf:ident
      [ $name:ident = @{ $( $head:tt )* } $( $tail:tt )* ] [] [] ) => {
        {
            if grammar!(@conv true $slf [ $( $head )* ] [] []) {
                return Some((Some(Rule::$name), $prec, grammar!(@assoc $( $head )*)))
            } else {
                grammar!(@conv_prec $pos ($prec + 1) $atomic $slf [ $( $tail )* ] [] [])
            }
        }
    };
    // silent
    ( @conv_prec $pos:ident ($prec:expr) $atomic:tt $slf:ident
      [ $name:ident = _{ $( $head:tt )* } $( $tail:tt )* ] [] [] ) => {
        {
            if grammar!(@conv $atomic $slf [ $( $head )* ] [] []) {
                return Some((None, $prec, grammar!(@assoc $( $head )*)))
            } else {
                grammar!(@conv_prec $pos ($prec + 1) $atomic $slf [ $( $tail )* ] [] [])
            }
        }
    };
    ( @conv $atomic:tt $slf:ident [ { $( $primary:tt )* } $( $ts:tt )* ] [] [] ) => {

          {
              let mut primary = |slf: &mut Self| {
                  let pos = slf.input().pos();

                  grammar!(@skip $atomic slf);

                  let result = grammar!(@conv $atomic slf [ $( $primary )* ] [] []);

                  if !result {
                      slf.input_mut().set_pos(pos);
                  }

                  result
              };
              let mut climb = |slf: &mut Self| {
                  let pos = slf.input().pos();

                  grammar!(@skip $atomic slf);

                  grammar!(@conv_prec pos (0u8) $atomic slf [ $( $ts )* ] [] [])
              };

              let mut pos = $slf.input().pos();
              let queue_pos = $slf.queue().len();

              let result = primary($slf);

              if let Some(token) = $slf.queue().get(queue_pos) {
                  pos = token.start;
              }

              if result {
                  $slf.prec_climb(queue_pos, pos, 0, None, &mut primary, &mut climb);
              }

              result
          }
    };

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
    ( @mtc $slf:ident [ $left:expr, $right:expr ]) => {
        $slf.input_mut().match_range($left, $right)
    };
    ( @mtc $slf:ident [ $str:expr ]) => ($slf.input_mut().match_string($str));
    ( @mtc $slf:ident $rule:ident) => ($slf.$rule());

    // process postfix
    ( @process $_atomic:tt $_slf:ident [( $result:expr )] [] ) => ($result);
    ( @process false $slf:ident [ $b:tt $a:tt $( $tail:tt )* ] [ ~ $( $optail:tt )* ] ) => {
        {
            grammar!(@process false $slf [(( $slf.try(false, |$slf| {
                if grammar!(@mtc $slf $a) {
                    let original = $slf.input().pos();

                    $slf.skip_ws();

                    let pos = $slf.input().pos();
                    let len = $slf.queue().len();

                    let result = grammar!(@mtc $slf $b);

                    if $slf.input().pos() == pos && !$slf.eoi_matched() {
                        $slf.input_mut().set_pos(original);
                    }

                    result
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
                let mut pos = $slf.input().pos();

                loop {
                    if !grammar!(@mtc $slf $a) {
                        $slf.input_mut().set_pos(pos);

                        break
                    }

                    pos = $slf.input().pos();

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
                    let pos = $slf.input().pos();

                    $slf.skip_ws();

                    if !grammar!(@mtc $slf $a) {
                        $slf.input_mut().set_pos(pos);

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
    // skip if not atomic
    ( @skip false $slf:ident ) => ();
    ( @skip true $slf:ident ) => {
        {
            $slf.skip_com();
            $slf.skip_ws();
        }
    };

    // whitespace and comment are always atomic
    ( @atomic whitespace $_atomic:tt $slf:ident $rules:tt ) => {
        grammar!(@conv true $slf $rules [] [])
    };
    ( @atomic comment $_atomic:tt $slf:ident $rules:tt ) => {
        grammar!(@conv true $slf $rules [] [])
    };
    ( @atomic $_name:ident $atomic:tt $slf:ident $rules:tt ) => {
        grammar!(@conv $atomic $slf $rules [] [])
    };

    () => {
        #[allow(dead_code)]
        #[inline]
        pub fn try<F>(&mut self, revert: bool, rule: F) -> bool
            where F: FnOnce(&mut Self) -> bool {

            let pos = self.input().pos();
            let len = self.queue().len();

            let result = rule(self);

            if revert || !result {
                self.input_mut().set_pos(pos);
            }

            if !result {
                self.queue_mut().truncate(len);
            }

            result
        }

        #[allow(dead_code)]
        #[inline]
        pub fn prec_climb<F, G>(&mut self, pos: usize, left: usize, min_prec: u8,
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

            while let Some((rule, prec, _)) = op {
                if prec >= min_prec {
                    let mut new_pos = self.input().pos();
                    let mut right = self.input().pos();
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

                        self.queue_mut().insert(pos, token);
                    }
                } else {
                    return (op, last_right)
                }
            }

            (op, last_right)
        }
    };

    // normal rule
    ( $name:ident = { $( $ts:tt )* } $( $tail:tt )* ) => {
        #[allow(unused_parens, unused_variables)]
        #[inline]
        pub fn $name(&mut self) -> bool {
            let slf = self;
            grammar!(@skip $name slf);

            let pos = slf.input().pos();
            let len = slf.queue().len();
            let tracked_len = slf.tracked_len();

            let result = grammar!(@atomic $name false slf [ $( $ts )* ]);

            if result {
                let new_pos = slf.input().pos();

                let token = Token {
                    rule:  Rule::$name,
                    start: pos,
                    end:   new_pos
                };

                slf.queue_mut().insert(len, token);
            } else {
                slf.queue_mut().truncate(len);

                if slf.tracked_len() == tracked_len {
                    slf.track(Rule::$name, pos);
                }
            }

            result
        }

        grammar!($( $tail )*);
    };

    // atomic rule
    ( $name:ident = @{ $( $ts:tt )* } $( $tail:tt )* ) => {
        #[allow(unused_parens, unused_variables)]
        #[inline]
        pub fn $name(&mut self) -> bool {
            let slf = self;
            grammar!(@skip $name slf);

            let pos = slf.input().pos();
            let len = slf.queue().len();

            let toggled = slf.is_atomic();

            if !toggled {
                slf.set_atomic(true);
            }

            let result = grammar!(@conv true slf [ $( $ts )* ] [] []);

            if !toggled {
                slf.set_atomic(false);
            }

            if result {
                let new_pos = slf.input().pos();

                let token = Token {
                    rule:  Rule::$name,
                    start: pos,
                    end:   new_pos
                };

                slf.queue_mut().insert(len, token);
            } else {
                slf.queue_mut().truncate(len);

                slf.track(Rule::$name, pos);
            }

            result
        }

        grammar!($( $tail )*);
    };

    // silent rule
    ( $name:ident = _{ $( $ts:tt )* } $( $tail:tt )* ) => {
        #[allow(unused_parens, unused_variables)]
        #[inline]
        pub fn $name(&mut self) -> bool {
            let slf = self;
            grammar!(@skip $name slf);

            let result = grammar!(@atomic $name false slf [ $( $ts )* ]);

            result
        }

        grammar!($( $tail )*);
    };
}
