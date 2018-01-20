// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#[doc(hidden)]
#[macro_export]
macro_rules! consumes_to {
    ( $_rules:ident, $tokens:expr, [] ) => ();
    ( $rules:ident, $tokens:expr, [ $name:ident ( $start:expr, $end:expr ) ] ) => {
        let expected = format!("expected Start {{ rule: {:?}, pos: Position {{ pos: {} }} }}",
                               $rules::$name, $start);
        match $tokens.next().expect(&format!("{} but found nothing", expected)) {
            $crate::Token::Start { rule, pos } => {
                let message = format!("{} but found Start {{ rule: {:?}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != $rules::$name || pos.pos() != $start {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        let expected = format!("expected End {{ rule: {:?}, pos: Position {{ pos: {} }} }}",
                               $rules::$name, $end);
        match $tokens.next().expect(&format!("{} but found nothing", expected)) {
            $crate::Token::End { rule, pos } => {
                let message = format!("{} but found End {{ rule: {:?}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != $rules::$name || pos.pos() != $end {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };
    };
    ( $rules:ident, $tokens:expr, [ $name:ident ( $start:expr, $end:expr ),
                                    $( $names:ident $calls:tt ),* $(,)* ] ) => {

        let expected = format!("expected Start {{ rule: {:?}, pos: Position {{ pos: {} }} }}",
                               $rules::$name, $start);
        match $tokens.next().expect(&format!("{} but found nothing", expected)) {
            $crate::Token::Start { rule, pos } => {
                let message = format!("{} but found Start {{ rule: {:?}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != $rules::$name || pos.pos() != $start {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        let expected = format!("expected End {{ rule: {:?}, pos: Position {{ pos: {} }} }}",
                               $rules::$name, $end);
        match $tokens.next().expect(&format!("{} but found nothing", expected)) {
            $crate::Token::End { rule, pos } => {
                let message = format!("{} but found End {{ rule: {:?}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != $rules::$name || pos.pos() != $end {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        consumes_to!($rules, $tokens, [ $( $names $calls ),* ]);
    };
    ( $rules:ident, $tokens:expr, [ $name:ident ( $start:expr, $end:expr,
                                                  [ $( $names:ident $calls:tt ),* $(,)* ] ) ] ) => {
        let expected = format!("expected Start {{ rule: {:?}, pos: Position {{ pos: {} }} }}",
                               $rules::$name, $start);
        match $tokens.next().expect(&format!("{} but found nothing", expected)) {
            $crate::Token::Start { rule, pos } => {
                let message = format!("{} but found Start {{ rule: {:?}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != $rules::$name || pos.pos() != $start {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        consumes_to!($rules, $tokens, [ $( $names $calls ),* ]);

        let expected = format!("expected End {{ rule: {:?}, pos: Position {{ pos: {} }} }}",
                               $rules::$name, $end);
        match $tokens.next().expect(&format!("{} but found nothing", expected)) {
            $crate::Token::End { rule, pos } => {
                let message = format!("{} but found End {{ rule: {:?}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != $rules::$name || pos.pos() != $end {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };
    };
    ( $rules:ident, $tokens:expr, [ $name:ident ( $start:expr, $end:expr,
                                                  [ $( $nested_names:ident $nested_calls:tt ),*
                                                  $(,)* ] ),
                                    $( $names:ident $calls:tt ),* ] ) => {

        let expected = format!("expected Start {{ rule: {:?}, pos: Position {{ pos: {} }} }}",
                               $rules::$name, $start);
        match $tokens.next().expect(&format!("{} but found nothing", expected)) {
            $crate::Token::Start { rule, pos } => {
                let message = format!("{} but found Start {{ rule: {:?}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != $rules::$name || pos.pos() != $start {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        consumes_to!($rules, $tokens, [ $( $nested_names $nested_calls ),* ]);

        let expected = format!("expected End {{ rule: {:?}, pos: Position {{ pos: {} }} }}",
                               $rules::$name, $end);
        match $tokens.next().expect(&format!("{} but found nothing", expected)) {
            $crate::Token::End { rule, pos } => {
                let message = format!("{} but found End {{ rule: {:?}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != $rules::$name || pos.pos() != $end {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        consumes_to!($rules, $tokens, [ $( $names $calls ),* ]);
    };
}

/// A `macro` which facilitates grammar testing and debugging by comparing produced tokens.
///
/// This macro takes several arguments:
///
/// * `parser` - name of the data structure implementing `Parser`
/// * `input` - input to be tested against
/// * `rule` - `Rule` which will be run
/// * `tokens` - token pairs of the form `name(start_pos, end_pos, [nested_child_tokens])`
///
/// *Note:* `start_pos` and `end_pos` are byte positions.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate pest;
/// # use pest::{Error, Parser};
/// # use pest::iterators::Pairs;
/// # fn main() {
/// # #[allow(non_camel_case_types)]
/// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
/// # enum Rule {
/// #     a,
/// #     b,
/// #     c
/// # }
/// #
/// # struct AbcParser;
/// #
/// # impl Parser<Rule> for AbcParser {
/// #     fn parse<'i>(_: Rule, input: &'i str) -> Result<Pairs<'i, Rule>, Error<'i, Rule>> {
/// #         pest::state(input, |state, pos| {
/// #             state.rule(Rule::a, pos, |state, pos| {
/// #                 state.rule(Rule::b, pos.skip(1).unwrap(), |_, pos| {
/// #                     pos.skip(1)
/// #                 }).unwrap().skip(1)
/// #             }).and_then(|p| {
/// #                 state.rule(Rule::c, p.skip(1).unwrap(), |_, pos| {
/// #                     pos.skip(1)
/// #                 })
/// #             })
/// #         })
/// #     }
/// # }
/// parses_to! {
///     parser: AbcParser,
///     input:  "abcde",
///     rule:   Rule::a,
///     tokens: [
///         a(0, 3, [
///             b(1, 2)
///         ]),
///         c(4, 5)
///     ]
/// };
/// # }
/// ```
#[macro_export]
macro_rules! parses_to {
    ( parser: $parser:ident, input: $string:expr, rule: $rules:tt :: $rule:tt,
      tokens: [ $( $names:ident $calls:tt ),* $(,)* ] ) => {

        #[allow(unused_mut)]
        {
            use $crate::Parser;

            let mut tokens = $parser::parse($rules::$rule, $string).unwrap().tokens();

            consumes_to!($rules, &mut tokens, [ $( $names $calls ),* ]);

            let rest: Vec<_> = tokens.collect();

            assert!(rest.is_empty(), format!("expected end of stream, but found {:?}", rest));
        }
    };
}

/// A `macro` which facilitates grammar testing and debugging by comparing produced errors.
///
/// This macro takes several arguments:
///
/// * `parser` - name of the data structure implementing `Parser`
/// * `input` - input to be tested against
/// * `rule` - `Rule` which will be run
/// * `positives` - positive `Rule` attempts that failed
/// * `negative` - negative `Rule` attempts that failed
/// * `pos` - byte position of failure
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate pest;
/// # use pest::{Error, Parser};
/// # use pest::iterators::Pairs;
/// # fn main() {
/// # #[allow(non_camel_case_types)]
/// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
/// # enum Rule {
/// #     a,
/// #     b,
/// #     c
/// # }
/// #
/// # struct AbcParser;
/// #
/// # impl Parser<Rule> for AbcParser {
/// #     fn parse<'i>(_: Rule, input: &'i str) -> Result<Pairs<'i, Rule>, Error<'i, Rule>> {
/// #         pest::state(input, |state, pos| {
/// #             state.rule(Rule::a, pos, |state, pos| {
/// #                 state.rule(Rule::b, pos.skip(1).unwrap(), |_, pos| {
/// #                     pos.skip(1)
/// #                 }).unwrap().skip(1)
/// #             }).and_then(|p| {
/// #                 state.rule(Rule::c, p.skip(1).unwrap(), |_, pos| {
/// #                     pos.match_string("e")
/// #                 })
/// #             })
/// #         })
/// #     }
/// # }
/// fails_with! {
///     parser: AbcParser,
///     input: "abcdf",
///     rule: Rule::a,
///     positives: vec![Rule::c],
///     negatives: vec![],
///     pos: 4
/// };
/// # }
/// ```
#[macro_export]
macro_rules! fails_with {
    ( parser: $parser:ident, input: $string:expr, rule: $rules:tt :: $rule:tt,
      positives: $positives:expr, negatives: $negatives:expr, pos: $pos:expr ) => {

        #[allow(unused_mut)]
        {
            use $crate::Parser;

            let error = $parser::parse($rules::$rule, $string).unwrap_err();

            match error {
                $crate::Error::ParsingError { positives, negatives, pos } => {
                    assert_eq!(positives, $positives);
                    assert_eq!(negatives, $negatives);
                    assert_eq!(pos.pos(), $pos);
                }
                _ => unreachable!()
            };
        }
    };
}

#[cfg(test)]
pub mod tests {
    use super::super::{state, Parser};
    use super::super::error::Error;
    use super::super::iterators::Pairs;

    #[allow(non_camel_case_types)]
    #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    pub enum Rule {
        a,
        b,
        c
    }

    pub struct AbcParser;

    impl Parser<Rule> for AbcParser {
        fn parse<'i>(_: Rule, input: &'i str) -> Result<Pairs<'i, Rule>, Error<'i, Rule>> {
            state(input, |state, pos| {
                state
                    .rule(Rule::a, pos, |state, pos| {
                        state
                            .rule(Rule::b, pos.skip(1).unwrap(), |_, pos| pos.skip(1))
                            .unwrap()
                            .skip(1)
                    })
                    .and_then(|p| {
                        state.rule(Rule::c, p.skip(1).unwrap(), |_, pos| pos.match_string("e"))
                    })
            })
        }
    }

    #[test]
    fn parses_to() {
        parses_to! {
            parser: AbcParser,
            input: "abcde",
            rule: Rule::a,
            tokens: [
                a(0, 3, [
                    b(1, 2),
                ]),
                c(4, 5)
            ]
        };
    }

    #[test]
    #[should_panic]
    fn missing_end() {
        parses_to! {
            parser: AbcParser,
            input: "abcde",
            rule: Rule::a,
            tokens: [
                a(0, 3, [
                    b(1, 2)
                ])
            ]
        };
    }

    #[test]
    #[should_panic]
    fn empty() {
        parses_to! {
            parser: AbcParser,
            input: "abcde",
            rule: Rule::a,
            tokens: []
        };
    }

    #[test]
    fn fails_with() {
        fails_with! {
            parser: AbcParser,
            input: "abcdf",
            rule: Rule::a,
            positives: vec![Rule::c],
            negatives: vec![],
            pos: 4
        };
    }

    #[test]
    #[should_panic]
    fn wrong_positives() {
        fails_with! {
            parser: AbcParser,
            input: "abcdf",
            rule: Rule::a,
            positives: vec![Rule::a],
            negatives: vec![],
            pos: 4
        };
    }

    #[test]
    #[should_panic]
    fn wrong_negatives() {
        fails_with! {
            parser: AbcParser,
            input: "abcdf",
            rule: Rule::a,
            positives: vec![Rule::c],
            negatives: vec![Rule::c],
            pos: 4
        };
    }

    #[test]
    #[should_panic]
    fn wrong_pos() {
        fails_with! {
            parser: AbcParser,
            input: "abcdf",
            rule: Rule::a,
            positives: vec![Rule::c],
            negatives: vec![],
            pos: 3
        };
    }
}
