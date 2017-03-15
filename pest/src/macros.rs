// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[macro_export]
macro_rules! expands_to {
    ( $_rules:ident, $_tokens:expr, [] ) => ();
    ( $rules:ident, $tokens:expr, [ $name:ident ( $start:expr, $end:expr ) ] ) => {
        let expected = format!("expected Start {{ rule: {:?}, .. }}", $rules::$name);
        match $tokens.next().expect(&format!("{} but found nothing", expected)).unwrap() {
            $crate::tokens::Token::Start { rule, pos } => {
                assert_eq!(rule, $rules::$name);
                assert_eq!(pos.pos(), $start);
            },
            _ => panic!("{}", format!("{} but found End", expected))
        };

        let expected = format!("expected End {{ rule: {:?}, .. }}", $rules::$name);
        match $tokens.next().expect(&format!("{} but found nothing", expected)).unwrap() {
            $crate::tokens::Token::End { rule, pos } => {
                assert_eq!(rule, $rules::$name);
                assert_eq!(pos.pos(), $end);
            },
            _ => panic!("{}", format!("{} but found Start", expected))
        };
    };
    ( $rules:ident, $tokens:expr, [ $name:ident ( $start:expr, $end:expr ),
                                    $( $names:ident $calls:tt ),* ] ) => {

        let expected = format!("expected Start {{ rule: {:?}, .. }}", $rules::$name);
        match $tokens.next().expect(&format!("{} but found nothing", expected)).unwrap() {
            $crate::tokens::Token::Start { rule, pos } => {
                assert_eq!(rule, $rules::$name);
                assert_eq!(pos.pos(), $start);
            },
            _ => panic!("{}", format!("{} but found End", expected))
        };

        let expected = format!("expected End {{ rule: {:?}, .. }}", $rules::$name);
        match $tokens.next().expect(&format!("{} but found nothing", expected)).unwrap() {
            $crate::tokens::Token::End { rule, pos } => {
                assert_eq!(rule, $rules::$name);
                assert_eq!(pos.pos(), $end);
            },
            _ => panic!("{}", format!("{} but found Start", expected))
        };

        expands_to!($rules, $tokens, [ $( $names $calls ),* ]);
    };
    ( $rules:ident, $tokens:expr, [ $name:ident ( $start:expr, $end:expr,
                                                  [ $( $names:ident $calls:tt ),* ] ) ] ) => {
        let expected = format!("expected Start {{ rule: {:?}, .. }}", $rules::$name);
        match $tokens.next().expect(&format!("{} but found nothing", expected)).unwrap() {
            $crate::tokens::Token::Start { rule, pos } => {
                assert_eq!(rule, $rules::$name);
                assert_eq!(pos.pos(), $start);
            },
            _ => panic!("{}", format!("{} but found End", expected))
        };

        expands_to!($rules, $tokens, [ $( $names $calls ),* ]);

        let expected = format!("expected End {{ rule: {:?}, .. }}", $rules::$name);
        match $tokens.next().expect(&format!("{} but found nothing", expected)).unwrap() {
            $crate::tokens::Token::End { rule, pos } => {
                assert_eq!(rule, $rules::$name);
                assert_eq!(pos.pos(), $end);
            },
            _ => panic!("{}", format!("{} but found Start", expected))
        };
    };
    ( $rules:ident, $tokens:expr, [ $name:ident ( $start:expr, $end:expr,
                                                  [ $( $nested_names:ident $nested_calls:tt ),* ] ),
                                    $( $names:ident $calls:tt ),* ] ) => {

        let expected = format!("expected Start {{ rule: {:?}, .. }}", $rules::$name);
        match $tokens.next().expect(&format!("{} but found nothing", expected)).unwrap() {
            $crate::tokens::Token::Start { rule, pos } => {
                assert_eq!(rule, $rules::$name);
                assert_eq!(pos.pos(), $start);
            },
            _ => panic!("{}", format!("{} but found End", expected))
        };

        expands_to!($rules, $tokens, [ $( $nested_names $nested_calls ),* ]);

        let expected = format!("expected End {{ rule: {:?}, .. }}", $rules::$name);
        match $tokens.next().expect(&format!("{} but found nothing", expected)).unwrap() {
            $crate::tokens::Token::End { rule, pos } => {
                assert_eq!(rule, $rules::$name);
                assert_eq!(pos.pos(), $end);
            },
            _ => panic!("{}", format!("{} but found Start", expected))
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

            let pos0 = state.start();
            let pos1 = pos0.clone().skip(1).unwrap();
            let pos2 = pos1.clone().skip(1).unwrap();
            let pos3 = pos2.clone().skip(1).unwrap();
            let pos4 = pos3.clone().skip(1).unwrap();
            let pos5 = pos4.clone().skip(1).unwrap();

            state.send(Token::Start { rule: Rule::a, pos: pos0 });
            state.send(Token::Start { rule: Rule::b, pos: pos1 });
            state.send(Token::End   { rule: Rule::b, pos: pos2 });
            state.send(Token::End   { rule: Rule::a, pos: pos3 });
            state.send(Token::Start { rule: Rule::c, pos: pos4 });
            state.send(Token::End   { rule: Rule::c, pos: pos5 });

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
