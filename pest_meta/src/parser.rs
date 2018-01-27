// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::iter::Peekable;

use pest::{self, Error, Parser, ParserState};
use pest::Position;
use pest::Span;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};

use ast::{Expr, Rule, RuleType};
use validator;

#[allow(dead_code, non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum PestRule {
    grammar_rules,
    grammar_rule,
    assignment_operator,
    silent_modifier,
    atomic_modifier,
    compound_atomic_modifier,
    non_atomic_modifier,
    opening_brace,
    closing_brace,
    opening_paren,
    closing_paren,
    expression,
    term,
    positive_predicate_operator,
    negative_predicate_operator,
    sequence_operator,
    choice_operator,
    optional_operator,
    repeat_operator,
    repeat_once_operator,
    repeat_exact,
    repeat_min,
    repeat_max,
    repeat_min_max,
    comma,
    push,
    identifier,
    string,
    quote,
    insensitive_string,
    range,
    range_operator,
    character,
    number,
    single_quote
}

pub struct PestParser;

impl Parser<PestRule> for PestParser {
    fn parse<'i>(
        rule: PestRule,
        input: &'i str
    ) -> Result<Pairs<'i, PestRule>, Error<'i, PestRule>> {
        fn grammar_rules<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.sequence(|pos| {
                soi(pos, state)
                    .and_then(|pos| skip(pos, state))
                    .and_then(|pos| grammar_rule(pos, state))
                    .and_then(|pos| {
                        pos.repeat(|pos| {
                            state.sequence(move |state| {
                                pos.sequence(|pos| {
                                    skip(pos, state).and_then(|pos| grammar_rule(pos, state))
                                })
                            })
                        })
                    })
                    .and_then(|pos| skip(pos, state))
                    .and_then(|pos| eoi(pos, state))
            })
        }

        fn soi<'i>(
            pos: Position<'i>,
            _: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.at_start()
        }

        fn eoi<'i>(
            pos: Position<'i>,
            _: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.at_end()
        }

        fn grammar_rule<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::grammar_rule, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        identifier(pos, state)
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| assignment_operator(pos, state))
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| pos.optional(|pos| modifier(pos, state)))
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| opening_brace(pos, state))
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| expression(pos, state))
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| closing_brace(pos, state))
                    })
                })
            })
        }

        fn assignment_operator<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::assignment_operator, pos, |_, pos| {
                pos.match_string("=")
            })
        }

        fn modifier<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            silent_modifier(pos, state)
                .or_else(|pos| atomic_modifier(pos, state))
                .or_else(|pos| compound_atomic_modifier(pos, state))
                .or_else(|pos| non_atomic_modifier(pos, state))
        }

        fn silent_modifier<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::silent_modifier, pos, |_, pos| {
                pos.match_string("_")
            })
        }

        fn atomic_modifier<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::atomic_modifier, pos, |_, pos| {
                pos.match_string("@")
            })
        }

        fn compound_atomic_modifier<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::compound_atomic_modifier, pos, |_, pos| {
                pos.match_string("$")
            })
        }

        fn non_atomic_modifier<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::non_atomic_modifier, pos, |_, pos| {
                pos.match_string("!")
            })
        }

        fn opening_brace<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::opening_brace, pos, |_, pos| {
                pos.match_string("{")
            })
        }

        fn closing_brace<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::closing_brace, pos, |_, pos| {
                pos.match_string("}")
            })
        }

        fn opening_paren<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::opening_paren, pos, |_, pos| {
                pos.match_string("(")
            })
        }

        fn closing_paren<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::closing_paren, pos, |_, pos| {
                pos.match_string(")")
            })
        }

        fn expression<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::expression, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        term(pos, state).and_then(|pos| {
                            pos.repeat(|pos| {
                                state.sequence(move |state| {
                                    pos.sequence(|pos| {
                                        skip(pos, state)
                                            .and_then(|pos| infix_operator(pos, state))
                                            .and_then(|pos| skip(pos, state))
                                            .and_then(|pos| term(pos, state))
                                    })
                                })
                            })
                        })
                    })
                })
            })
        }

        fn term<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::term, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        pos.repeat(|pos| {
                            pos.sequence(|pos| {
                                prefix_operator(pos, state).and_then(|pos| skip(pos, state))
                            })
                        }).and_then(|pos| {
                                state
                                    .sequence(move |state| {
                                        pos.sequence(|pos| {
                                            opening_paren(pos, state)
                                                .and_then(|pos| skip(pos, state))
                                                .and_then(|pos| expression(pos, state))
                                                .and_then(|pos| skip(pos, state))
                                                .and_then(|pos| closing_paren(pos, state))
                                        })
                                    })
                                    .or_else(|pos| terminal(pos, state))
                            })
                            .and_then(|pos| {
                                pos.repeat(|pos| {
                                    pos.sequence(|pos| {
                                        skip(pos, state)
                                            .and_then(|pos| postfix_operator(pos, state))
                                    })
                                })
                            })
                    })
                })
            })
        }

        fn terminal<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            push(pos, state)
                .or_else(|pos| identifier(pos, state))
                .or_else(|pos| string(pos, state))
                .or_else(|pos| insensitive_string(pos, state))
                .or_else(|pos| range(pos, state))
        }

        fn prefix_operator<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            positive_predicate_operator(pos, state)
                .or_else(|pos| negative_predicate_operator(pos, state))
        }

        fn infix_operator<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            sequence_operator(pos, state).or_else(|pos| choice_operator(pos, state))
        }

        fn postfix_operator<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            optional_operator(pos, state)
                .or_else(|pos| repeat_operator(pos, state))
                .or_else(|pos| repeat_once_operator(pos, state))
                .or_else(|pos| repeat_exact(pos, state))
                .or_else(|pos| repeat_min(pos, state))
                .or_else(|pos| repeat_max(pos, state))
                .or_else(|pos| repeat_min_max(pos, state))
        }

        fn positive_predicate_operator<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::positive_predicate_operator, pos, |_, pos| {
                pos.match_string("&")
            })
        }

        fn negative_predicate_operator<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::negative_predicate_operator, pos, |_, pos| {
                pos.match_string("!")
            })
        }

        fn sequence_operator<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::sequence_operator, pos, |_, pos| {
                pos.match_string("~")
            })
        }

        fn choice_operator<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::choice_operator, pos, |_, pos| {
                pos.match_string("|")
            })
        }

        fn optional_operator<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::optional_operator, pos, |_, pos| {
                pos.match_string("?")
            })
        }

        fn repeat_operator<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::repeat_operator, pos, |_, pos| {
                pos.match_string("*")
            })
        }

        fn repeat_once_operator<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::repeat_once_operator, pos, |_, pos| {
                pos.match_string("+")
            })
        }

        fn repeat_exact<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::repeat_exact, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        opening_brace(pos, state)
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| number(pos, state))
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| closing_brace(pos, state))
                    })
                })
            })
        }

        fn repeat_min<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::repeat_min, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        opening_brace(pos, state)
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| number(pos, state))
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| comma(pos, state))
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| closing_brace(pos, state))
                    })
                })
            })
        }

        fn repeat_max<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::repeat_max, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        opening_brace(pos, state)
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| comma(pos, state))
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| number(pos, state))
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| closing_brace(pos, state))
                    })
                })
            })
        }

        fn repeat_min_max<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::repeat_min_max, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        opening_brace(pos, state)
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| number(pos, state))
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| comma(pos, state))
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| number(pos, state))
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| closing_brace(pos, state))
                    })
                })
            })
        }

        fn comma<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::comma, pos, |_, pos| {
                pos.match_string(",")
            })
        }

        fn push<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::push, pos, |state, pos| {
                pos.sequence(|pos| {
                    pos.match_string("push").and_then(|pos| {
                        skip(pos, state)
                    }).and_then(|pos| {
                        opening_paren(pos, state)
                    }).and_then(|pos| {
                        skip(pos, state)
                    }).and_then(|pos| {
                        expression(pos, state)
                    }).and_then(|pos| {
                        skip(pos, state)
                    }).and_then(|pos| {
                        closing_paren(pos, state)
                    })
                })
            })
        }

        fn identifier<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::identifier, pos, |state, pos| {
                pos.sequence(|pos| {
                    pos.lookahead(false, |pos| {
                        pos.match_string("push")
                    }).and_then(|pos| {
                        pos.match_string("_").or_else(|pos| {
                            alpha(pos, state)
                        }).and_then(|pos| {
                            pos.repeat(|pos| {
                                pos.match_string("_").or_else(|pos| {
                                    alpha_num(pos, state)
                                })
                            })
                        })
                    })
                })
            })
        }


        fn alpha<'i>(
            pos: Position<'i>,
            _: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.match_range('a'..'z')
                .or_else(|pos| pos.match_range('A'..'Z'))
        }

        fn alpha_num<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            alpha(pos, state).or_else(|pos| pos.match_range('0'..'9'))
        }

        fn string<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::string, pos, |state, pos| {
                pos.sequence(|pos| {
                    quote(pos, state)
                        .and_then(|pos| {
                            pos.repeat(|pos| {
                                pos.sequence(|pos| {
                                    pos.lookahead(false, |pos| {
                                        pos.match_string("\"").or_else(|pos| pos.match_string("\\"))
                                    }).and_then(|pos| pos.skip(1))
                                }).or_else(|pos| escape(pos, state))
                            })
                        })
                        .and_then(|pos| quote(pos, state))
                })
            })
        }

        fn quote<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::quote, pos, |_, pos| pos.match_string("\""))
        }

        fn insensitive_string<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::insensitive_string, pos, |state, pos| {
                pos.sequence(|pos| {
                    pos.match_string("^")
                        .and_then(|pos| skip(pos, state))
                        .and_then(|pos| string(pos, state))
                })
            })
        }

        fn range<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::range, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        character(pos, state)
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| range_operator(pos, state))
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| character(pos, state))
                    })
                })
            })
        }

        fn range_operator<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::range_operator, pos, |_, pos| {
                pos.match_string("..")
            })
        }

        fn character<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::character, pos, |state, pos| {
                pos.sequence(|pos| {
                    single_quote(pos, state)
                        .and_then(|pos| {
                            pos.sequence(|pos| {
                                pos.lookahead(false, |pos| {
                                    pos.match_string("'").or_else(|pos| pos.match_string("\\"))
                                }).and_then(|pos| pos.skip(1))
                            }).or_else(|pos| escape(pos, state))
                        })
                        .and_then(|pos| single_quote(pos, state))
                })
            })
        }

        fn number<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::number, pos, |_, pos| {
                pos.sequence(|pos| {
                    pos.match_range('0'..'9')
                        .and_then(|pos| pos.repeat(|pos| pos.match_range('0'..'9')))
                })
            })
        }

        fn single_quote<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(PestRule::single_quote, pos, |_, pos| {
                pos.match_string("'")
            })
        }

        fn escape<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.sequence(|pos| {
                pos.match_string("\\").and_then(|pos| {
                    pos.match_string("n")
                        .or_else(|pos| pos.match_string("r"))
                        .or_else(|pos| pos.match_string("t"))
                        .or_else(|pos| pos.match_string("\\"))
                        .or_else(|pos| pos.match_string("0"))
                        .or_else(|pos| pos.match_string("'"))
                        .or_else(|pos| pos.match_string("\""))
                        .or_else(|pos| unicode(pos, state))
                        .or_else(|pos| code(pos, state))
                })
            })
        }

        fn unicode<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.sequence(|pos| {
                pos.match_string("u")
                    .and_then(|pos| pos.match_string("{"))
                    .and_then(|pos| hex_digit(pos, state))
                    .and_then(|pos| hex_digit(pos, state))
                    .and_then(|pos| pos.optional(|pos| hex_digit(pos, state)))
                    .and_then(|pos| pos.optional(|pos| hex_digit(pos, state)))
                    .and_then(|pos| pos.optional(|pos| hex_digit(pos, state)))
                    .and_then(|pos| pos.optional(|pos| hex_digit(pos, state)))
                    .and_then(|pos| pos.match_string("}"))
            })
        }

        fn code<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.sequence(|pos| {
                pos.match_string("x")
                    .and_then(|pos| hex_digit(pos, state))
                    .and_then(|pos| hex_digit(pos, state))
            })
        }

        fn hex_digit<'i>(
            pos: Position<'i>,
            _: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.match_range('0'..'9')
                .or_else(|pos| pos.match_range('a'..'f'))
                .or_else(|pos| pos.match_range('A'..'F'))
        }

        fn skip<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.sequence(|pos| {
                pos.repeat(|pos| whitespace(pos, state)).and_then(|pos| {
                    pos.repeat(|pos| {
                        pos.sequence(|pos| {
                            pos.optional(|pos| comment(pos, state)).and_then(|pos| {
                                pos.sequence(|pos| {
                                    whitespace(pos, state)
                                        .and_then(|pos| pos.repeat(|pos| whitespace(pos, state)))
                                })
                            })
                        })
                    })
                })
            })
        }

        fn whitespace<'i>(
            pos: Position<'i>,
            _: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.match_string(" ")
                .or_else(|pos| pos.match_string("\t"))
                .or_else(|pos| pos.match_string("\r"))
                .or_else(|pos| pos.match_string("\n"))
        }

        fn comment<'i>(
            pos: Position<'i>,
            _: &mut ParserState<'i, PestRule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.sequence(|pos| {
                pos.match_string("//").and_then(|pos| {
                    pos.repeat(|pos| {
                        pos.sequence(|pos| {
                            pos.lookahead(false, |pos| pos.match_string("\n"))
                                .and_then(|pos| pos.skip(1))
                        })
                    })
                })
            })
        }

        pest::state(input, move |mut state, pos: Position<'i>| match rule {
            PestRule::grammar_rules => grammar_rules(pos, &mut state),
            PestRule::grammar_rule => grammar_rule(pos, &mut state),
            PestRule::assignment_operator => assignment_operator(pos, &mut state),
            PestRule::silent_modifier => silent_modifier(pos, &mut state),
            PestRule::atomic_modifier => atomic_modifier(pos, &mut state),
            PestRule::compound_atomic_modifier => compound_atomic_modifier(pos, &mut state),
            PestRule::non_atomic_modifier => non_atomic_modifier(pos, &mut state),
            PestRule::opening_brace => opening_brace(pos, &mut state),
            PestRule::closing_brace => closing_brace(pos, &mut state),
            PestRule::opening_paren => opening_paren(pos, &mut state),
            PestRule::closing_paren => closing_paren(pos, &mut state),
            PestRule::expression => expression(pos, &mut state),
            PestRule::term => term(pos, &mut state),
            PestRule::positive_predicate_operator => {
                positive_predicate_operator(pos, &mut state)
            }
            PestRule::negative_predicate_operator => {
                negative_predicate_operator(pos, &mut state)
            }
            PestRule::sequence_operator => sequence_operator(pos, &mut state),
            PestRule::choice_operator => choice_operator(pos, &mut state),
            PestRule::optional_operator => optional_operator(pos, &mut state),
            PestRule::repeat_operator => repeat_operator(pos, &mut state),
            PestRule::repeat_once_operator => repeat_once_operator(pos, &mut state),
            PestRule::repeat_exact => repeat_exact(pos, &mut state),
            PestRule::repeat_min => repeat_min(pos, &mut state),
            PestRule::repeat_max => repeat_max(pos, &mut state),
            PestRule::repeat_min_max => repeat_min_max(pos, &mut state),
            PestRule::comma => comma(pos, &mut state),
            PestRule::push => push(pos, &mut state),
            PestRule::identifier => identifier(pos, &mut state),
            PestRule::string => string(pos, &mut state),
            PestRule::quote => quote(pos, &mut state),
            PestRule::insensitive_string => insensitive_string(pos, &mut state),
            PestRule::range => range(pos, &mut state),
            PestRule::range_operator => range_operator(pos, &mut state),
            PestRule::character => character(pos, &mut state),
            PestRule::number => number(pos, &mut state),
            PestRule::single_quote => single_quote(pos, &mut state)
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParserRule<'i> {
    pub name: &'i str,
    pub span: Span<'i>,
    pub ty: RuleType,
    pub node: ParserNode<'i>
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParserNode<'i> {
    pub expr: ParserExpr<'i>,
    pub span: Span<'i>
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParserExpr<'i> {
    Str(String),
    Insens(String),
    Range(String, String),
    Ident(&'i str),
    PosPred(Box<ParserNode<'i>>),
    NegPred(Box<ParserNode<'i>>),
    Seq(Box<ParserNode<'i>>, Box<ParserNode<'i>>),
    Choice(Box<ParserNode<'i>>, Box<ParserNode<'i>>),
    Opt(Box<ParserNode<'i>>),
    Rep(Box<ParserNode<'i>>),
    RepOnce(Box<ParserNode<'i>>),
    RepExact(Box<ParserNode<'i>>, u32),
    RepMin(Box<ParserNode<'i>>, u32),
    RepMax(Box<ParserNode<'i>>, u32),
    RepMinMax(Box<ParserNode<'i>>, u32, u32),
    Push(Box<ParserNode<'i>>)
}

fn convert_rule<'i>(rule: ParserRule<'i>) -> Rule {
    match rule {
        ParserRule { name, ty, node, .. } => {
            let expr = convert_node(node);

            Rule { name, ty, expr }
        }
    }
}

fn convert_node<'i>(node: ParserNode<'i>) -> Expr<'i> {
    match node.expr {
        ParserExpr::Str(string) => Expr::Str(string),
        ParserExpr::Insens(string) => Expr::Insens(string),
        ParserExpr::Range(start, end) => Expr::Range(start, end),
        ParserExpr::Ident(ident) => Expr::Ident(ident),
        ParserExpr::PosPred(node) => Expr::PosPred(Box::new(convert_node(*node))),
        ParserExpr::NegPred(node) => Expr::NegPred(Box::new(convert_node(*node))),
        ParserExpr::Seq(node1, node2) => Expr::Seq(
            Box::new(convert_node(*node1)),
            Box::new(convert_node(*node2))
        ),
        ParserExpr::Choice(node1, node2) => Expr::Choice(
            Box::new(convert_node(*node1)),
            Box::new(convert_node(*node2))
        ),
        ParserExpr::Opt(node) => Expr::Opt(Box::new(convert_node(*node))),
        ParserExpr::Rep(node) => Expr::Rep(Box::new(convert_node(*node))),
        ParserExpr::RepOnce(node) => Expr::RepOnce(Box::new(convert_node(*node))),
        ParserExpr::RepExact(node, num) => Expr::RepExact(Box::new(convert_node(*node)), num),
        ParserExpr::RepMin(node, max) => Expr::RepMin(Box::new(convert_node(*node)), max),
        ParserExpr::RepMax(node, max) => Expr::RepMax(Box::new(convert_node(*node)), max),
        ParserExpr::RepMinMax(node, min, max) => {
            Expr::RepMinMax(Box::new(convert_node(*node)), min, max)
        }
        ParserExpr::Push(node) => Expr::Push(Box::new(convert_node(*node)))
    }
}

pub fn consume_rules<'i>(pairs: Pairs<'i, PestRule>) -> Result<Vec<Rule<'i>>, Vec<Error<'i, PestRule>>> {
    let rules = consume_rules_with_spans(pairs);
    let errors = validator::validate_ast(&rules);
    if errors.len() == 0 {
        Ok(rules.into_iter().map(|rule| convert_rule(rule)).collect())
    } else {
        Err(errors)
    }
}

fn consume_rules_with_spans<'i>(pairs: Pairs<'i, PestRule>) -> Vec<ParserRule<'i>> {
    let climber = PrecClimber::new(vec![
        Operator::new(PestRule::choice_operator, Assoc::Left),
        Operator::new(PestRule::sequence_operator, Assoc::Left),
    ]);

    pairs
        .filter(|pair| pair.as_rule() == PestRule::grammar_rule)
        .map(|pair| {
            let mut pairs = pair.into_inner().peekable();

            let span = pairs.next().unwrap().into_span();
            let name = span.as_str();

            pairs.next().unwrap(); // assignment_operator

            let ty = if pairs.peek().unwrap().as_rule() != PestRule::opening_brace {
                match pairs.next().unwrap().as_rule() {
                    PestRule::silent_modifier => RuleType::Silent,
                    PestRule::atomic_modifier => RuleType::Atomic,
                    PestRule::compound_atomic_modifier => RuleType::CompoundAtomic,
                    PestRule::non_atomic_modifier => RuleType::NonAtomic,
                    _ => unreachable!()
                }
            } else {
                RuleType::Normal
            };

            pairs.next().unwrap(); // opening_brace

            let node = consume_expr(pairs.next().unwrap().into_inner().peekable(), &climber);

            ParserRule {
                name,
                span,
                ty,
                node
            }
        })
        .collect()
}

fn consume_expr<'i>(
    pairs: Peekable<Pairs<'i, PestRule>>,
    climber: &PrecClimber<PestRule>
) -> ParserNode<'i> {
    fn unaries<'i>(
        mut pairs: Peekable<Pairs<'i, PestRule>>,
        climber: &PrecClimber<PestRule>
    ) -> ParserNode<'i> {
        let pair = pairs.next().unwrap();

        match pair.as_rule() {
            PestRule::opening_paren => {
                let node = unaries(pairs, climber);
                let end = node.span.end_pos();

                ParserNode {
                    expr: node.expr,
                    span: pair.into_span().start_pos().span(&end)
                }
            }
            PestRule::positive_predicate_operator => {
                let node = unaries(pairs, climber);
                let end = node.span.end_pos();

                ParserNode {
                    expr: ParserExpr::PosPred(Box::new(node)),
                    span: pair.into_span().start_pos().span(&end)
                }
            }
            PestRule::negative_predicate_operator => {
                let node = unaries(pairs, climber);
                let end = node.span.end_pos();

                ParserNode {
                    expr: ParserExpr::NegPred(Box::new(node)),
                    span: pair.into_span().start_pos().span(&end)
                }
            }
            other_rule => {
                let node = match other_rule {
                    PestRule::expression => consume_expr(pair.into_inner().peekable(), climber),
                    PestRule::push => {
                        let start = pair.clone().into_span().start_pos();
                        let mut pairs = pair.into_inner();
                        pairs.next().unwrap(); // opening_paren
                        let pair = pairs.next().unwrap();

                        let node = consume_expr(pair.into_inner().peekable(), climber);
                        let end = node.span.end_pos();

                        ParserNode {
                            expr: ParserExpr::Push(Box::new(node)),
                            span: start.span(&end)
                        }
                    }
                    PestRule::identifier => ParserNode {
                        expr: ParserExpr::Ident(pair.as_str()),
                        span: pair.clone().into_span()
                    },
                    PestRule::string => {
                        let string = pair.as_str();
                        ParserNode {
                            expr: ParserExpr::Str(string[1..string.len() - 1].to_owned()),
                            span: pair.clone().into_span()
                        }
                    }
                    PestRule::insensitive_string => {
                        let string = pair.as_str();
                        ParserNode {
                            expr: ParserExpr::Insens(string[2..string.len() - 1].to_owned()),
                            span: pair.clone().into_span()
                        }
                    }
                    PestRule::range => {
                        let mut pairs = pair.into_inner();
                        let pair = pairs.next().unwrap();
                        let start = pair.as_str();
                        let start_pos = pair.clone().into_span().start_pos();
                        pairs.next();
                        let pair = pairs.next().unwrap();
                        let end = pair.as_str();
                        let end_pos = pair.clone().into_span().end_pos();

                        ParserNode {
                            expr: ParserExpr::Range(start.to_owned(), end.to_owned()),
                            span: start_pos.span(&end_pos)
                        }
                    }
                    _ => unreachable!()
                };

                pairs.fold(node, |node, pair| {
                    match pair.as_rule() {
                        PestRule::optional_operator => {
                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::Opt(Box::new(node)),
                                span: start.span(&pair.into_span().end_pos())
                            }
                        }
                        PestRule::repeat_operator => {
                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::Rep(Box::new(node)),
                                span: start.span(&pair.into_span().end_pos())
                            }
                        }
                        PestRule::repeat_once_operator => {
                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::RepOnce(Box::new(node)),
                                span: start.span(&pair.into_span().end_pos())
                            }
                        }
                        PestRule::repeat_exact => {
                            let overflow = |span| {
                                let error: Error<()> = Error::CustomErrorSpan {
                                    message: "number cannot overflow u32".to_owned(),
                                    span
                                };

                                format!("parsing error\n\n{}", error)
                            };

                            let mut inner = pair.clone().into_inner();

                            inner.next().unwrap(); // opening_brace

                            let number = inner.next().unwrap();
                            let num: u32 = number
                                .as_str()
                                .parse()
                                .expect(&overflow(number.into_span()));

                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::RepExact(Box::new(node), num),
                                span: start.span(&pair.into_span().end_pos())
                            }
                        }
                        PestRule::repeat_min => {
                            let overflow = |span| {
                                let error: Error<()> = Error::CustomErrorSpan {
                                    message: "number cannot overflow u32".to_owned(),
                                    span
                                };

                                format!("parsing error\n\n{}", error)
                            };

                            let mut inner = pair.clone().into_inner();

                            inner.next().unwrap(); // opening_brace

                            let min_number = inner.next().unwrap();
                            let min: u32 = min_number
                                .as_str()
                                .parse()
                                .expect(&overflow(min_number.into_span()));

                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::RepMin(Box::new(node), min),
                                span: start.span(&pair.into_span().end_pos())
                            }
                        }
                        PestRule::repeat_max => {
                            let overflow = |span| {
                                let error: Error<()> = Error::CustomErrorSpan {
                                    message: "number cannot overflow u32".to_owned(),
                                    span
                                };

                                format!("parsing error\n\n{}", error)
                            };

                            let mut inner = pair.clone().into_inner();

                            inner.next().unwrap(); // opening_brace
                            inner.next().unwrap(); // comma

                            let max_number = inner.next().unwrap();
                            let max: u32 = max_number
                                .as_str()
                                .parse()
                                .expect(&overflow(max_number.into_span()));

                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::RepMax(Box::new(node), max),
                                span: start.span(&pair.into_span().end_pos())
                            }
                        }
                        PestRule::repeat_min_max => {
                            let overflow = |span| {
                                let error: Error<()> = Error::CustomErrorSpan {
                                    message: "number cannot overflow u32".to_owned(),
                                    span
                                };

                                format!("parsing error\n\n{}", error)
                            };

                            let mut inner = pair.clone().into_inner();

                            inner.next().unwrap(); // opening_brace

                            let min_number = inner.next().unwrap();
                            let min: u32 = min_number
                                .as_str()
                                .parse()
                                .expect(&overflow(min_number.into_span()));

                            inner.next().unwrap(); // comma

                            let max_number = inner.next().unwrap();
                            let max: u32 = max_number
                                .as_str()
                                .parse()
                                .expect(&overflow(max_number.into_span()));

                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::RepMinMax(Box::new(node), min, max),
                                span: start.span(&pair.into_span().end_pos())
                            }
                        }
                        PestRule::closing_paren => {
                            let start = node.span.start_pos();

                            ParserNode {
                                expr: node.expr,
                                span: start.span(&pair.into_span().end_pos())
                            }
                        }
                        _ => unreachable!()
                    }
                })
            }
        }
    }

    let term = |pair: Pair<'i, PestRule>| unaries(pair.into_inner().peekable(), climber);
    let infix =
        |lhs: ParserNode<'i>, op: Pair<'i, PestRule>, rhs: ParserNode<'i>| match op.as_rule() {
            PestRule::sequence_operator => {
                let start = lhs.span.start_pos();
                let end = rhs.span.end_pos();

                ParserNode {
                    expr: ParserExpr::Seq(Box::new(lhs), Box::new(rhs)),
                    span: start.span(&end)
                }
            }
            PestRule::choice_operator => {
                let start = lhs.span.start_pos();
                let end = rhs.span.end_pos();

                ParserNode {
                    expr: ParserExpr::Choice(Box::new(lhs), Box::new(rhs)),
                    span: start.span(&end)
                }
            }
            _ => unreachable!()
        };

    climber.climb(pairs, term, infix)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rules() {
        parses_to! {
            parser: PestParser,
            input: "a = { b } c = { d }",
            rule: PestRule::grammar_rules,
            tokens: [
                grammar_rule(0, 9, [
                    identifier(0, 1),
                    assignment_operator(2, 3),
                    opening_brace(4, 5),
                    expression(6, 7, [
                        term(6, 7, [
                            identifier(6, 7)
                        ])
                    ]),
                    closing_brace(8, 9)
                ]),
                grammar_rule(10, 19, [
                    identifier(10, 11),
                    assignment_operator(12, 13),
                    opening_brace(14, 15),
                    expression(16, 17, [
                        term(16, 17, [
                            identifier(16, 17)
                        ])
                    ]),
                    closing_brace(18, 19)
                ])
            ]
        };
    }

    #[test]
    fn rule() {
        parses_to! {
            parser: PestParser,
            input: "a = ! { b ~ c }",
            rule: PestRule::grammar_rule,
            tokens: [
                grammar_rule(0, 15, [
                    identifier(0, 1),
                    assignment_operator(2, 3),
                    non_atomic_modifier(4, 5),
                    opening_brace(6, 7),
                    expression(8, 13, [
                        term(8, 9, [
                            identifier(8, 9)
                        ]),
                        sequence_operator(10, 11),
                        term(12, 13, [
                            identifier(12, 13)
                        ])
                    ]),
                    closing_brace(14, 15)
                ])
            ]
        };
    }

    #[test]
    fn expression() {
        parses_to! {
            parser: PestParser,
            input: "_a | 'a'..'b' ~ !^\"abc\" ~ (d | e)*?",
            rule: PestRule::expression,
            tokens: [
                expression(0, 35, [
                    term(0, 2, [
                        identifier(0, 2)
                    ]),
                    choice_operator(3, 4),
                    term(5, 13, [
                        range(5, 13, [
                            character(5, 8, [
                                single_quote(5, 6),
                                single_quote(7, 8)
                            ]),
                            range_operator(8, 10),
                            character(10, 13, [
                                single_quote(10, 11),
                                single_quote(12, 13)
                            ])
                        ])
                    ]),
                    sequence_operator(14, 15),
                    term(16, 23, [
                        negative_predicate_operator(16, 17),
                        insensitive_string(17, 23, [
                            string(18, 23, [
                                quote(18, 19),
                                quote(22, 23)
                            ])
                        ])
                    ]),
                    sequence_operator(24, 25),
                    term(26, 35, [
                        opening_paren(26, 27),
                        expression(27, 32, [
                            term(27, 28, [
                                identifier(27, 28)
                            ]),
                            choice_operator(29, 30),
                            term(31, 32, [
                                identifier(31, 32)
                            ])
                        ]),
                        closing_paren(32, 33),
                        repeat_operator(33, 34),
                        optional_operator(34, 35)
                    ])
                ])
            ]
        };
    }

    #[test]
    fn repeat_exact() {
        parses_to! {
            parser: PestParser,
            input: "{1}",
            rule: PestRule::repeat_exact,
            tokens: [
                repeat_exact(0, 3, [
                    opening_brace(0, 1),
                    number(1, 2),
                    closing_brace(2, 3)
                ])
            ]
        };
    }

    #[test]
    fn repeat_min() {
        parses_to! {
            parser: PestParser,
            input: "{2,}",
            rule: PestRule::repeat_min,
            tokens: [
                repeat_min(0, 4, [
                    opening_brace(0,1),
                    number(1,2),
                    comma(2,3),
                    closing_brace(3,4)
                ])
            ]
        }
    }

    #[test]
    fn repeat_max() {
        parses_to! {
            parser: PestParser,
            input: "{, 3}",
            rule: PestRule::repeat_max,
            tokens: [
                repeat_max(0, 5, [
                    opening_brace(0,1),
                    comma(1,2),
                    number(3,4),
                    closing_brace(4,5)
                ])
            ]
        }
    }

    #[test]
    fn repeat_min_max() {
        parses_to! {
            parser: PestParser,
            input: "{1, 2}",
            rule: PestRule::repeat_min_max,
            tokens: [
                repeat_min_max(0, 6, [
                    opening_brace(0, 1),
                    number(1, 2),
                    comma(2, 3),
                    number(4, 5),
                    closing_brace(5, 6)
                ])
            ]
        };
    }

    #[test]
    fn push() {
        parses_to! {
            parser: PestParser,
            input: "push ( a )",
            rule: PestRule::push,
            tokens: [
                push(0, 10, [
                    opening_paren(5, 6),
                    expression(7, 8, [
                        term(7, 8, [
                            identifier(7, 8)
                        ])
                    ]),
                    closing_paren(9, 10)
                ])
            ]
        };
    }

    #[test]
    fn identifier() {
        parses_to! {
            parser: PestParser,
            input: "_a8943",
            rule: PestRule::identifier,
            tokens: [
                identifier(0, 6)
            ]
        };
    }

    #[test]
    fn string() {
        parses_to! {
            parser: PestParser,
            input: "\"aaaaa\\n\\r\\t\\\\\\0\\'\\\"\\x0F\\u{123abC}\\u{12}aaaaa\"",
            rule: PestRule::string,
            tokens: [
                string(0, 46, [
                    quote(0, 1),
                    quote(45, 46)
                ])
            ]
        };
    }

    #[test]
    fn insensitive_string() {
        parses_to! {
            parser: PestParser,
            input: "^  \"\\\"hi\"",
            rule: PestRule::insensitive_string,
            tokens: [
                insensitive_string(0, 9, [
                    string(3, 9, [
                        quote(3, 4),
                        quote(8, 9)
                    ])
                ])
            ]
        };
    }

    #[test]
    fn range() {
        parses_to! {
            parser: PestParser,
            input: "'\\n' .. '\\x1a'",
            rule: PestRule::range,
            tokens: [
                range(0, 14, [
                    character(0, 4, [
                        single_quote(0, 1),
                        single_quote(3, 4)
                    ]),
                    range_operator(5, 7),
                    character(8, 14, [
                        single_quote(8, 9),
                        single_quote(13, 14)
                    ])
                ])
            ]
        };
    }

    #[test]
    fn character() {
        parses_to! {
            parser: PestParser,
            input: "'\\u{123abC}'",
            rule: PestRule::character,
            tokens: [
                character(0, 12, [
                    single_quote(0, 1),
                    single_quote(11, 12)
                ])
            ]
        };
    }

    #[test]
    fn number() {
        parses_to! {
            parser: PestParser,
            input: "0123",
            rule: PestRule::number,
            tokens: [
                number(0, 4)
            ]
        };
    }

    #[test]
    fn comment() {
        parses_to! {
            parser: PestParser,
            input: "a ~    // asda\n b",
            rule: PestRule::expression,
            tokens: [
                expression(0, 17, [
                    term(0, 1, [
                        identifier(0, 1)
                    ]),
                    sequence_operator(2, 3),
                    term(16, 17, [
                        identifier(16, 17)
                    ])
                ])
            ]
        };
    }

    #[test]
    fn wrong_identifier() {
        fails_with! {
            parser: PestParser,
            input: "0",
            rule: PestRule::grammar_rules,
            positives: vec![PestRule::identifier],
            negatives: vec![],
            pos: 0
        };
    }

    #[test]
    fn missing_assignment_operator() {
        fails_with! {
            parser: PestParser,
            input: "a {}",
            rule: PestRule::grammar_rules,
            positives: vec![PestRule::assignment_operator],
            negatives: vec![],
            pos: 2
        };
    }

    #[test]
    fn wrong_modifier() {
        fails_with! {
            parser: PestParser,
            input: "a = *{}",
            rule: PestRule::grammar_rules,
            positives: vec![
                PestRule::silent_modifier,
                PestRule::atomic_modifier,
                PestRule::compound_atomic_modifier,
                PestRule::non_atomic_modifier,
                PestRule::opening_brace
            ],
            negatives: vec![],
            pos: 4
        };
    }

    #[test]
    fn missing_opening_brace() {
        fails_with! {
            parser: PestParser,
            input: "a = _",
            rule: PestRule::grammar_rules,
            positives: vec![PestRule::opening_brace],
            negatives: vec![],
            pos: 5
        };
    }

    #[test]
    fn empty_rule() {
        fails_with! {
            parser: PestParser,
            input: "a = {}",
            rule: PestRule::grammar_rules,
            positives: vec![PestRule::expression],
            negatives: vec![],
            pos: 5
        };
    }

    #[test]
    fn missing_rhs() {
        fails_with! {
            parser: PestParser,
            input: "a = { b ~ }",
            rule: PestRule::grammar_rules,
            positives: vec![PestRule::term],
            negatives: vec![],
            pos: 10
        };
    }

    #[test]
    fn wrong_op() {
        fails_with! {
            parser: PestParser,
            input: "a = { b % }",
            rule: PestRule::grammar_rules,
            positives: vec![
                PestRule::opening_brace,
                PestRule::closing_brace,
                PestRule::sequence_operator,
                PestRule::choice_operator,
                PestRule::optional_operator,
                PestRule::repeat_operator,
                PestRule::repeat_once_operator
            ],
            negatives: vec![],
            pos: 8
        };
    }

    #[test]
    fn missing_closing_paren() {
        fails_with! {
            parser: PestParser,
            input: "a = { (b }",
            rule: PestRule::grammar_rules,
            positives: vec![
                PestRule::opening_brace,
                PestRule::closing_paren,
                PestRule::sequence_operator,
                PestRule::choice_operator,
                PestRule::optional_operator,
                PestRule::repeat_operator,
                PestRule::repeat_once_operator
            ],
            negatives: vec![],
            pos: 9
        };
    }

    #[test]
    fn missing_term() {
        fails_with! {
            parser: PestParser,
            input: "a = { ! }",
            rule: PestRule::grammar_rules,
            positives: vec![
                PestRule::opening_paren,
                PestRule::positive_predicate_operator,
                PestRule::negative_predicate_operator,
                PestRule::push,
                PestRule::identifier,
                PestRule::quote,
                PestRule::insensitive_string,
                PestRule::single_quote
            ],
            negatives: vec![],
            pos: 8
        };
    }

    #[test]
    fn string_missing_ending_quote() {
        fails_with! {
            parser: PestParser,
            input: "a = { \" }",
            rule: PestRule::grammar_rules,
            positives: vec![PestRule::quote],
            negatives: vec![],
            pos: 9
        };
    }

    #[test]
    fn insensitive_missing_string() {
        fails_with! {
            parser: PestParser,
            input: "a = { ^ }",
            rule: PestRule::grammar_rules,
            positives: vec![PestRule::string],
            negatives: vec![],
            pos: 8
        };
    }

    #[test]
    fn char_missing_ending_single_quote() {
        fails_with! {
            parser: PestParser,
            input: "a = { \' }",
            rule: PestRule::grammar_rules,
            positives: vec![PestRule::single_quote],
            negatives: vec![],
            pos: 8
        };
    }

    #[test]
    fn range_missing_range_operator() {
        fails_with! {
            parser: PestParser,
            input: "a = { \'a\' }",
            rule: PestRule::grammar_rules,
            positives: vec![PestRule::range_operator],
            negatives: vec![],
            pos: 10
        };
    }

    #[test]
    fn wrong_postfix() {
        fails_with! {
            parser: PestParser,
            input: "a = { a& }",
            rule: PestRule::grammar_rules,
            positives: vec![
                PestRule::opening_brace,
                PestRule::closing_brace,
                PestRule::sequence_operator,
                PestRule::choice_operator,
                PestRule::optional_operator,
                PestRule::repeat_operator,
                PestRule::repeat_once_operator
            ],
            negatives: vec![],
            pos: 7
        };
    }

    #[test]
    fn ast() {
        let input =
            "rule = _{ a{1} ~ \"a\"{3,} ~ b{, 2} ~ \"b\"{1, 2} | !(^\"c\" | push('d'..'e'))?* }";

        let pairs = PestParser::parse(PestRule::grammar_rules, input).unwrap();
        let ast = consume_rules_with_spans(pairs);
        let ast: Vec<_> = ast.into_iter().map(|rule| convert_rule(rule)).collect();

        assert_eq!(
            ast,
            vec![
                Rule {
                    name: "rule",
                    ty: RuleType::Silent,
                    expr: Expr::Choice(
                        Box::new(Expr::Seq(
                            Box::new(Expr::Seq(
                                Box::new(Expr::Seq(
                                    Box::new(Expr::RepExact(
                                        Box::new(Expr::Ident("a")),
                                        1
                                    )),
                                    Box::new(Expr::RepMin(Box::new(Expr::Str("a".to_owned())), 3))
                                )),
                                Box::new(Expr::RepMax(Box::new(Expr::Ident("b")), 2))
                            )),
                            Box::new(Expr::RepMinMax(Box::new(Expr::Str("b".to_owned())), 1, 2))
                        )),
                        Box::new(Expr::NegPred(Box::new(Expr::Rep(Box::new(Expr::Opt(
                            Box::new(Expr::Choice(
                                Box::new(Expr::Insens("c".to_owned())),
                                Box::new(Expr::Push(Box::new(Expr::Range(
                                    "'d'".to_owned(),
                                    "'e'".to_owned()
                                ))))
                            ))
                        ))))))
                    )
                },
            ]
        );
    }
}
