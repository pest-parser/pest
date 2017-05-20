// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[doc(hidden)]
#[macro_export]
macro_rules! consumes_to {
    ( $_rules:ident, $tokens:expr, [] ) => {
        let rest: Vec<_> = $tokens.map(|r| r.unwrap()).collect();

        assert!(rest.is_empty(), format!("expected end of stream, but found {:?}", rest));
    };
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
                                                  [ $( $names:ident $calls:tt ),* ] ) ] ) => {
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
                                                  [ $( $nested_names:ident $nested_calls:tt ),* ] ),
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

/// A `macro` which facilitates grammar testing and debugging.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate pest;
/// # use std::rc::Rc;
/// # use pest::{Error, Parser};
/// # use pest::inputs::Input;
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
/// #     fn parse<I: Input>(_: Rule, input: Rc<I>) -> Result<Pairs<Rule, I>, Error<Rule, I>> {
/// #         pest::state(input, |mut state, pos| {
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
      tokens: [ $( $names:ident $calls:tt ),* ] ) => {

        let input = $crate::inputs::StringInput::new($string.to_owned());
        let mut tokens = $parser::parse($rules::$rule,
                                        ::std::rc::Rc::new(input)).unwrap().into_iter();

        consumes_to!($rules, tokens, [ $( $names $calls ),* ]);
    };
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use super::super::error::Error;
    use super::super::inputs::Input;
    use super::super::{Parser, state};
    use super::super::iterators::Pairs;

    #[allow(non_camel_case_types)]
    #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    enum Rule {
        a,
        b,
        c
    }

    struct AbcParser;

    impl Parser<Rule> for AbcParser {
        fn parse<I: Input>(_: Rule, input: Rc<I>) -> Result<Pairs<Rule, I>, Error<Rule, I>> {
            state(input, |mut state, pos| {
                state.rule(Rule::a, pos, |state, pos| {
                    state.rule(Rule::b, pos.skip(1).unwrap(), |_, pos| {
                        pos.skip(1)
                    }).unwrap().skip(1)
                }).and_then(|p| {
                    state.rule(Rule::c, p.skip(1).unwrap(), |_, pos| {
                        pos.skip(1)
                    })
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
                    b(1, 2)
                ]),
                c(4, 5)
            ]
        };
    }
}
