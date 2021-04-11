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
    ( $tokens:expr, [] ) => ();
    ( $tokens:expr, [ $name:ident ( $start:expr, $end:expr ) ] ) => {
        let expected = format!("expected Start {{ rule: {}, pos: Position {{ pos: {} }} }}",
                               stringify!($name), $start);
        match $tokens.next().expect(&format!("{} but found nothing", expected)) {
            ::pest::Token::Start { rule, pos } => {
                let message = format!("{} but found Start {{ rule: {}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != stringify!($name) || pos.pos() != $start {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        let expected = format!("expected End {{ rule: {}, pos: Position {{ pos: {} }} }}",
                               stringify!($name), $end);
        match $tokens.next().expect(&format!("{} but found nothing", expected)) {
            ::pest::Token::End { rule, pos } => {
                let message = format!("{} but found End {{ rule: {}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != stringify!($name) || pos.pos() != $end {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };
    };
    ( $tokens:expr, [ $name:ident ( $start:expr, $end:expr ),
                      $( $names:ident $calls:tt ),* $(,)* ] ) => {

        let expected = format!("expected Start {{ rule: {}, pos: Position {{ pos: {} }} }}",
                               stringify!($name), $start);
        match $tokens.next().expect(&format!("{} but found nothing", expected)) {
            ::pest::Token::Start { rule, pos } => {
                let message = format!("{} but found Start {{ rule: {}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != stringify!($name) || pos.pos() != $start {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        let expected = format!("expected End {{ rule: {}, pos: Position {{ pos: {} }} }}",
                               stringify!($name), $end);
        match $tokens.next().expect(&format!("{} but found nothing", expected)) {
            ::pest::Token::End { rule, pos } => {
                let message = format!("{} but found End {{ rule: {}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != stringify!($name) || pos.pos() != $end {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        consumes_to!($tokens, [ $( $names $calls ),* ]);
    };
    ( $tokens:expr, [ $name:ident ( $start:expr, $end:expr,
                      [ $( $names:ident $calls:tt ),* $(,)* ] ) ] ) => {
        let expected = format!("expected Start {{ rule: {}, pos: Position {{ pos: {} }} }}",
                               stringify!($name), $start);
        match $tokens.next().expect(&format!("{} but found nothing", expected)) {
            ::pest::Token::Start { rule, pos } => {
                let message = format!("{} but found Start {{ rule: {}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != stringify!($name) || pos.pos() != $start {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        consumes_to!($tokens, [ $( $names $calls ),* ]);

        let expected = format!("expected End {{ rule: {}, pos: Position {{ pos: {} }} }}",
                               stringify!($name), $end);
        match $tokens.next().expect(&format!("{} but found nothing", expected)) {
            ::pest::Token::End { rule, pos } => {
                let message = format!("{} but found End {{ rule: {}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != stringify!($name) || pos.pos() != $end {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };
    };
    ( $tokens:expr, [ $name:ident ( $start:expr, $end:expr,
                      [ $( $nested_names:ident $nested_calls:tt ),* $(,)* ] ),
      $( $names:ident $calls:tt ),* ] ) => {

        let expected = format!("expected Start {{ rule: {}, pos: Position {{ pos: {} }} }}",
                               stringify!($name), $start);
        match $tokens.next().expect(&format!("{} but found nothing", expected)) {
            ::pest::Token::Start { rule, pos } => {
                let message = format!("{} but found Start {{ rule: {}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != stringify!($name) || pos.pos() != $start {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        consumes_to!($tokens, [ $( $nested_names $nested_calls ),* ]);

        let expected = format!("expected End {{ rule: {}, pos: Position {{ pos: {} }} }}",
                               stringify!($name), $end);
        match $tokens.next().expect(&format!("{} but found nothing", expected)) {
            ::pest::Token::End { rule, pos } => {
                let message = format!("{} but found End {{ rule: {}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != stringify!($name) || pos.pos() != $end {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        consumes_to!($tokens, [ $( $names $calls ),* ]);
    };
}

#[macro_export]
macro_rules! parses_to {
    ( parser: $parser:expr, input: $string:expr, rule: $rule:expr,
      tokens: [ $( $names:ident $calls:tt ),* $(,)* ] ) => {

        #[allow(unused_mut)]
        {
            let vm = $parser;
            let mut tokens = vm.parse($rule, $string).unwrap().tokens();

            consumes_to!(&mut tokens, [ $( $names $calls ),* ]);

            let rest: Vec<_> = tokens.collect();

            match rest.len() {
                0 => (),
                2 => {
                    let (first, second) = (&rest[0], &rest[1]);

                    match (first, second) {
                        (
                            &::pest::Token::Start { rule: ref first_rule, .. },
                            &::pest::Token::End { rule: ref second_rule, .. }
                        ) => {
                            assert!(
                                format!("{}", first_rule) == "EOI",
                                "expected end of input, but found {:?}",
                                rest
                            );
                            assert!(
                                format!("{}", second_rule) == "EOI",
                                "expected end of input, but found {:?}",
                                rest
                            );
                        }
                        _ => panic!("expected end of input, but found {:?}", rest)
                    }
                }
                _ => panic!("expected end of input, but found {:?}", rest)
            };
        }
    };
}

#[macro_export]
macro_rules! fails_with {
    ( parser: $parser:expr, input: $string:expr, rule: $rule:expr,
      positives: $positives:expr, negatives: $negatives:expr, pos: $pos:expr ) => {
        #[allow(unused_mut)]
        #[allow(unused_variables)]
        {
            let vm = $parser;
            let error = vm.parse($rule, $string).unwrap_err();

            match error.variant {
                ::pest::error::ErrorVariant::ParsingError {
                    positives,
                    negatives,
                } => {
                    let positives: Vec<&str> = $positives;
                    let negatives: Vec<&str> = $negatives;

                    assert_eq!(positives, positives);
                    assert_eq!(negatives, negatives);
                }
                _ => unreachable!(),
            };

            match error.location {
                ::pest::error::InputLocation::Pos(pos) => assert_eq!(pos, $pos),
                _ => unreachable!(),
            }
        }
    };
}
