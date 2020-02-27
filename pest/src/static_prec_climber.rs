// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Statically constructible variant of [`crate::prec_climber::PrecClimber`].

use std::iter::Peekable;

use iterators::Pair;
use RuleType;


/// Macro for more convenient definition of static `static_prec_climber::PrecClimber`.
///
/// # Examples
///
/// ```
/// # use pest::static_prec_climber::{Assoc, PrecClimber};
/// # use pest::static_prec_climber;
/// # #[allow(non_camel_case_types)]
/// # #[allow(dead_code)]
/// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
/// # enum Rule {
/// #     plus,
/// #     minus,
/// #     times,
/// #     divide,
/// #     power
/// # }
/// static CLIMBER: PrecClimber<Rule> = static_prec_climber![
///     L   plus | minus,
///     L   times | divide,
///     R   power,
/// ];
/// ```
#[macro_export]
macro_rules! static_prec_climber {
    ( __assoc L ) => { $crate::static_prec_climber::Assoc::Left };
    ( __assoc R ) => { $crate::static_prec_climber::Assoc::Right };

    (
        __array
        $(
            $assoc:ident $rule:ident
        ),*
    ) => {
        &[
            $(
                (
                    Rule::$rule, 
                    $rule,
                    static_prec_climber!( __assoc $assoc ),
                )
            ),*
        ]
    };

    (
        __precedences { $precedence:expr }
    ) => {};

    (
        __precedences { $precedence:expr }
        [ $( $rule:ident )* ]
        $( [ $( $rules:ident )* ] )*
    ) => {
        $(
            const $rule: u32 = $precedence;
        )*
        static_prec_climber!(
            __precedences { 1u32 + $precedence }
            $( [ $( $rules )* ] )*
        );
    };

    (
        $( $assoc:ident $rule:ident $( | $rules:ident )* ),+ $(,)?
    ) => {{
        static_prec_climber!(
            __precedences { 1u32 }
            $( [ $rule $( $rules )* ] )*
        );

        $crate::static_prec_climber::PrecClimber::new(
            static_prec_climber!(
                __array
                $( $assoc $rule $(, $assoc $rules )* ),*
            )
        )
    }};
}

/// Associativity of a `Rule`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Assoc {
    /// Left `Rule` associativity
    Left,
    /// Right `Rule` associativity
    Right,
}

/// List of operators and precedences, which can perform [precedence climbing][1] on infix
/// expressions contained in a [`Pairs`]. The token pairs contained in the `Pairs` should start
/// with a *primary* pair and then alternate between an *operator* and a *primary*.
///
/// [1]: https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method
/// [`Pairs`]: ../iterators/struct.Pairs.html
#[derive(Debug)]
pub struct PrecClimber<'r, R> {
    ops: &'r[(R, u32, Assoc)],
}

impl<'r, R> PrecClimber<'r, R> {
    /// Creates a new `PrecClimber` directly from a static slice of 
    /// `(rule: Rule, precedence: u32, associativity: Assoc)` tuples.
    ///
    /// Precedence starts from `1`.  Entries don't have to be ordered in any way, but it's easier to read when 
    /// sorted.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::static_prec_climber::{Assoc, PrecClimber};
    /// # #[allow(non_camel_case_types)]
    /// # #[allow(dead_code)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// # enum Rule {
    /// #     plus,
    /// #     minus,
    /// #     times,
    /// #     divide,
    /// #     power
    /// # }
    /// static CLIMBER: PrecClimber<Rule> = PrecClimber::new(&[
    ///     (Rule::plus, 1, Assoc::Left), (Rule::minus, 1, Assoc::Left),
    ///     (Rule::times, 2, Assoc::Left), (Rule::divide, 2, Assoc::Left),
    ///     (Rule::power, 3, Assoc::Right)
    /// ]);
    /// ```
    pub const fn new(ops: &'r [(R, u32, Assoc)]) -> PrecClimber<R> {
        PrecClimber { ops }
    }
}

impl<'r, R: RuleType> PrecClimber<'r, R> {
    fn get(&self, rule: &R) -> Option<(u32, Assoc)> {
        self.ops
            .iter()
            .find(|(r, _, _)| r == rule)
            .map(|(_, precedence, assoc)| (*precedence, *assoc))
    }

    /// Performs the precedence climbing algorithm on the `pairs` in a similar manner to map-reduce.
    /// *Primary* pairs are mapped with `primary` and then reduced to one single result with
    /// `infix`.
    ///
    /// # Panics
    ///
    /// Panics will occur when `pairs` is empty or when the alternating *primary*, *operator*,
    /// *primary* order is not respected.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let primary = |pair| {
    ///     consume(pair, climber)
    /// };
    /// let infix = |lhs: i32, op: Pair<Rule>, rhs: i32| {
    ///     match op.rule() {
    ///         Rule::plus => lhs + rhs,
    ///         Rule::minus => lhs - rhs,
    ///         Rule::times => lhs * rhs,
    ///         Rule::divide => lhs / rhs,
    ///         Rule::power => lhs.pow(rhs as u32),
    ///         _ => unreachable!()
    ///     }
    /// };
    ///
    /// let result = CLIMBER.climb(pairs, primary, infix);
    /// ```
    pub fn climb<'i, P, F, G, T>(&self, mut pairs: P, mut primary: F, mut infix: G) -> T
    where
        P: Iterator<Item = Pair<'i, R>>,
        F: FnMut(Pair<'i, R>) -> T,
        G: FnMut(T, Pair<'i, R>, T) -> T,
    {
        let lhs = primary(
            pairs
                .next()
                .expect("precedence climbing requires a non-empty Pairs"),
        );
        self.climb_rec(lhs, 0, &mut pairs.peekable(), &mut primary, &mut infix)
    }

    fn climb_rec<'i, P, F, G, T>(
        &self,
        mut lhs: T,
        min_prec: u32,
        pairs: &mut Peekable<P>,
        primary: &mut F,
        infix: &mut G,
    ) -> T
    where
        P: Iterator<Item = Pair<'i, R>>,
        F: FnMut(Pair<'i, R>) -> T,
        G: FnMut(T, Pair<'i, R>, T) -> T,
    {
        while pairs.peek().is_some() {
            let rule = pairs.peek().unwrap().as_rule();
            if let Some((prec, _)) = self.get(&rule) {
                if prec >= min_prec {
                    let op = pairs.next().unwrap();
                    let mut rhs = primary(pairs.next().expect(
                        "infix operator must be followed by \
                         a primary expression",
                    ));

                    while pairs.peek().is_some() {
                        let rule = pairs.peek().unwrap().as_rule();
                        if let Some((new_prec, assoc)) = self.get(&rule) {
                            if new_prec > prec || assoc == Assoc::Right && new_prec == prec {
                                rhs = self.climb_rec(rhs, new_prec, pairs, primary, infix);
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }

                    lhs = infix(lhs, op, rhs);
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        lhs
    }
}
