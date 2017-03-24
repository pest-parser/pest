// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[macro_export]
macro_rules! expands_to {
    ( $_rules:ident, $tokens:expr, [] ) => {
        let rest: Vec<_> = $tokens.map(|r| r.unwrap()).collect();

        assert!(rest.is_empty(), format!("expected end of stream, but found {:?}", rest));
    };
    ( $rules:ident, $tokens:expr, [ $name:ident ( $start:expr, $end:expr ) ] ) => {
        let expected = format!("expected Start {{ rule: {:?}, pos: Position {{ pos: {} }} }}",
                               $rules::$name, $start);
        match $tokens.next().expect(&format!("{} but found nothing", expected)).unwrap() {
            $crate::tokens::Token::Start { rule, pos } => {
                let message = format!("{} but found Start {{ rule: {:?}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != $rules::$name && pos.pos() != $start {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        let expected = format!("expected End {{ rule: {:?}, pos: Position {{ pos: {} }} }}",
                               $rules::$name, $end);
        match $tokens.next().expect(&format!("{} but found nothing", expected)).unwrap() {
            $crate::tokens::Token::End { rule, pos } => {
                let message = format!("{} but found End {{ rule: {:?}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != $rules::$name && pos.pos() != $end {
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
        match $tokens.next().expect(&format!("{} but found nothing", expected)).unwrap() {
            $crate::tokens::Token::Start { rule, pos } => {
                let message = format!("{} but found Start {{ rule: {:?}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != $rules::$name && pos.pos() != $start {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        let expected = format!("expected End {{ rule: {:?}, pos: Position {{ pos: {} }} }}",
                               $rules::$name, $end);
        match $tokens.next().expect(&format!("{} but found nothing", expected)).unwrap() {
            $crate::tokens::Token::End { rule, pos } => {
                let message = format!("{} but found End {{ rule: {:?}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != $rules::$name && pos.pos() != $end {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        expands_to!($rules, $tokens, [ $( $names $calls ),* ]);
    };
    ( $rules:ident, $tokens:expr, [ $name:ident ( $start:expr, $end:expr,
                                                  [ $( $names:ident $calls:tt ),* ] ) ] ) => {
        let expected = format!("expected Start {{ rule: {:?}, pos: Position {{ pos: {} }} }}",
                               $rules::$name, $start);
        match $tokens.next().expect(&format!("{} but found nothing", expected)).unwrap() {
            $crate::tokens::Token::Start { rule, pos } => {
                let message = format!("{} but found Start {{ rule: {:?}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != $rules::$name && pos.pos() != $start {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        expands_to!($rules, $tokens, [ $( $names $calls ),* ]);

        let expected = format!("expected End {{ rule: {:?}, pos: Position {{ pos: {} }} }}",
                               $rules::$name, $end);
        match $tokens.next().expect(&format!("{} but found nothing", expected)).unwrap() {
            $crate::tokens::Token::End { rule, pos } => {
                let message = format!("{} but found End {{ rule: {:?}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != $rules::$name && pos.pos() != $end {
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
        match $tokens.next().expect(&format!("{} but found nothing", expected)).unwrap() {
            $crate::tokens::Token::Start { rule, pos } => {
                let message = format!("{} but found Start {{ rule: {:?}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != $rules::$name && pos.pos() != $start {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        expands_to!($rules, $tokens, [ $( $nested_names $nested_calls ),* ]);

        let expected = format!("expected End {{ rule: {:?}, pos: Position {{ pos: {} }} }}",
                               $rules::$name, $end);
        match $tokens.next().expect(&format!("{} but found nothing", expected)).unwrap() {
            $crate::tokens::Token::End { rule, pos } => {
                let message = format!("{} but found End {{ rule: {:?}, pos: Position {{ {} }} }}",
                                      expected, rule, pos.pos());

                if rule != $rules::$name && pos.pos() != $end {
                    panic!("{}", message);
                }
            },
            token => panic!("{}", format!("{} but found {:?}", expected, token))
        };

        expands_to!($rules, $tokens, [ $( $names $calls ),* ]);
    };
}

#[macro_export]
macro_rules! parses_to {
    ( parser: $parser:ident, input: $string:expr, rule: $rules:tt :: $rule:tt,
      tokens: [ $( $names:ident $calls:tt ),* ] ) => {

        use futures::stream::Stream;

        let input = $crate::inputs::StringInput::new($string);
        let mut tokens = $parser::parse($rules::$rule, input).wait();

        expands_to!($rules, tokens, [ $( $names $calls ),* ]);
    };
}

#[cfg(test)]
mod tests {
    use super::super::inputs::Input;
    use super::super::{Parser, state};
    use super::super::streams::ParserStream;
    use super::super::tokens::Token;

    #[allow(non_camel_case_types)]
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    enum Rule {
        a,
        b,
        c
    }

    struct AbcParser;

    impl Parser<Rule> for AbcParser {
        fn parse<I: Input>(_: Rule, input: I) -> ParserStream<Rule, I> {
            let (mut state, stream) = state(input);
            let pos = state.start();

            state.rule(Rule::a, pos, true, |pos, state| {
                state.rule(Rule::b, pos.skip(1).unwrap(), true, |pos, state| {
                    pos.skip(1)
                }).unwrap().skip(1)
            }).and_then(|p| {
                state.rule(Rule::c, p.skip(1).unwrap(), true, |pos, state| {
                    pos.skip(1)
                })
            }).unwrap();

            stream
        }
    }

    #[test]
    fn parses_to() {
        parses_to! {
            parser: AbcParser,
            input:  "abcde",
            rule:   Rule::a,
            tokens: [
                a(0, 3, [
                    b(1, 2)
                ]),
                c(4, 5)
            ]
        };
    }
}
