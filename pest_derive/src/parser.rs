// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::iter::Peekable;
use std::rc::Rc;

use pest::inputs::{Input, Position, Span};
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::{self, Error, Parser, ParserState};

use quote::Ident;

use super::ast::{Expr, Rule, RuleType};
use super::validator;

#[allow(dead_code, non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum GrammarRule {
    grammar_rules,
    soi,
    eoi,
    grammar_rule,
    assignment_operator,
    silent_modifier,
    atomic_modifier,
    non_atomic_modifier,
    opening_brace,
    closing_brace,
    opening_paren,
    closing_paren,
    expression,
    primary,
    positive_predicate_operator,
    negative_predicate_operator,
    sequence_operator,
    choice_operator,
    optional_operator,
    repeat_operator,
    repeat_once_operator,
    push,
    identifier,
    string,
    quote,
    insensitive_string,
    range,
    range_operator,
    character,
    single_quote
}

pub struct GrammarParser;

impl Parser<GrammarRule> for GrammarParser {
    fn parse<I: Input>(
        rule: GrammarRule,
        input: Rc<I>
    ) -> Result<Pairs<GrammarRule, I>, Error<GrammarRule, I>> {
        fn grammar_rules<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.sequence(|pos| {
                soi(pos, state).and_then(|pos| {
                    skip(pos, state)
                }).and_then(|pos| {
                    grammar_rule(pos, state)
                }).and_then(|pos| {
                    pos.repeat(|pos| {
                        state.sequence(move |state| {
                            pos.sequence(|pos| {
                                skip(pos, state).and_then(|pos| {
                                    grammar_rule(pos, state)
                                })
                            })
                        })
                    })
                }).and_then(|pos| {
                    skip(pos, state)
                }).and_then(|pos| {
                    eoi(pos, state)
                })
            })
        }

        fn soi<I: Input>(
            pos: Position<I>,
            _: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.at_start()
        }

        fn eoi<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::eoi, pos, |_, pos| {
                pos.at_end()
            })
        }

        fn grammar_rule<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::grammar_rule, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        identifier(pos, state).and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            assignment_operator(pos, state)
                        }).and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            pos.optional(|pos| {
                                modifier(pos, state)
                            })
                        }).and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            opening_brace(pos, state)
                        }).and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            expression(pos, state)
                        }).and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            closing_brace(pos, state)
                        })
                    })
                })
            })
        }

        fn assignment_operator<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::assignment_operator, pos, |_, pos| {
                pos.match_string("=")
            })
        }

        fn modifier<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            silent_modifier(pos, state).or_else(|pos| {
                atomic_modifier(pos, state)
            }).or_else(|pos| {
                non_atomic_modifier(pos, state)
            })
        }

        fn silent_modifier<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::silent_modifier, pos, |_, pos| {
                pos.match_string("_")
            })
        }

        fn atomic_modifier<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::atomic_modifier, pos, |_, pos| {
                pos.match_string("@")
            })
        }

        fn non_atomic_modifier<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::non_atomic_modifier, pos, |_, pos| {
                pos.match_string("!@")
            })
        }

        fn opening_brace<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::opening_brace, pos, |_, pos| {
                pos.match_string("{")
            })
        }

        fn closing_brace<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::closing_brace, pos, |_, pos| {
                pos.match_string("}")
            })
        }

        fn opening_paren<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::opening_paren, pos, |_, pos| {
                pos.match_string("(")
            })
        }

        fn closing_paren<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::closing_paren, pos, |_, pos| {
                pos.match_string(")")
            })
        }

        fn expression<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::expression, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        primary(pos, state).and_then(|pos| {
                            pos.repeat(|pos| {
                                state.sequence(move |state| {
                                    pos.sequence(|pos| {
                                        skip(pos, state).and_then(|pos| {
                                            infix_operator(pos, state)
                                        }).and_then(|pos| {
                                            skip(pos, state)
                                        }).and_then(|pos| {
                                            primary(pos, state)
                                        })
                                    })
                                })
                            })
                        })
                    })
                })
            })
        }

        fn primary<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::primary, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        pos.repeat(|pos| {
                            pos.sequence(|pos| {
                                prefix_operator(pos, state).and_then(|pos| {
                                    skip(pos, state)
                                })
                            })
                        }).and_then(|pos| {
                            state.sequence(move |state| {
                                pos.sequence(|pos| {
                                    pos.match_string("(").and_then(|pos| {
                                        skip(pos, state)
                                    }).and_then(|pos| {
                                        expression(pos, state)
                                    }).and_then(|pos| {
                                        skip(pos, state)
                                    }).and_then(|pos| {
                                        pos.match_string(")")
                                    })
                                })
                            }).or_else(|pos| {
                                term(pos, state)
                            })
                        }).and_then(|pos| {
                            pos.repeat(|pos| {
                                pos.sequence(|pos| {
                                    skip(pos, state).and_then(|pos| {
                                        postfix_operator(pos, state)
                                    })
                                })
                            })
                        })
                    })
                })
            })
        }

        fn term<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            push(pos, state).or_else(|pos| {
                identifier(pos, state)
            }).or_else(|pos| {
                string(pos, state)
            }).or_else(|pos| {
                insensitive_string(pos, state)
            }).or_else(|pos| {
                range(pos, state)
            })
        }

        fn prefix_operator<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            positive_predicate_operator(pos, state).or_else(|pos| {
                negative_predicate_operator(pos, state)
            })
        }

        fn infix_operator<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            sequence_operator(pos, state).or_else(|pos| {
                choice_operator(pos, state)
            })
        }

        fn postfix_operator<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            optional_operator(pos, state).or_else(|pos| {
                repeat_operator(pos, state)
            }).or_else(|pos| {
                repeat_once_operator(pos, state)
            })
        }

        fn positive_predicate_operator<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::positive_predicate_operator, pos, |_, pos| {
                pos.match_string("&")
            })
        }

        fn negative_predicate_operator<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::negative_predicate_operator, pos, |_, pos| {
                pos.match_string("!")
            })
        }

        fn sequence_operator<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::sequence_operator, pos, |_, pos| {
                pos.match_string("~")
            })
        }

        fn choice_operator<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::choice_operator, pos, |_, pos| {
                pos.match_string("|")
            })
        }

        fn optional_operator<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::optional_operator, pos, |_, pos| {
                pos.match_string("?")
            })
        }

        fn repeat_operator<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::repeat_operator, pos, |_, pos| {
                pos.match_string("*")
            })
        }

        fn repeat_once_operator<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::repeat_once_operator, pos, |_, pos| {
                pos.match_string("+")
            })
        }

        fn push<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::push, pos, |state, pos| {
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

        fn identifier<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::identifier, pos, |state, pos| {
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

        fn alpha<I: Input>(
            pos: Position<I>,
            _: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.match_range('a'..'z').or_else(|pos| {
                pos.match_range('A'..'Z')
            })
        }

        fn alpha_num<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            alpha(pos, state).or_else(|pos| {
                pos.match_range('0'..'9')
            })
        }

        fn string<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::string, pos, |state, pos| {
                pos.sequence(|pos| {
                    quote(pos, state).and_then(|pos| {
                        pos.repeat(|pos| {
                            pos.sequence(|pos| {
                                pos.lookahead(false, |pos| {
                                    pos.match_string("\"").or_else(|pos| {
                                        pos.match_string("\\")
                                    })
                                }).and_then(|pos| {
                                    pos.skip(1)
                                })
                            }).or_else(|pos| {
                                escape(pos, state)
                            })
                        })
                    }).and_then(|pos| {
                        quote(pos, state)
                    })
                })
            })
        }

        fn quote<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::quote, pos, |_, pos| {
                pos.match_string("\"")
            })
        }

        fn insensitive_string<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::insensitive_string, pos, |state, pos| {
                pos.sequence(|pos| {
                    pos.match_string("^").and_then(|pos| {
                        skip(pos, state)
                    }).and_then(|pos| {
                        string(pos, state)
                    })
                })
            })
        }

        fn range<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::range, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        character(pos, state).and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            range_operator(pos, state)
                        }).and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            character(pos, state)
                        })
                    })
                })
            })
        }

        fn range_operator<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::range_operator, pos, |_, pos| {
                pos.match_string("..")
            })
        }

        fn character<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::character, pos, |state, pos| {
                pos.sequence(|pos| {
                    single_quote(pos, state).and_then(|pos| {
                        pos.sequence(|pos| {
                            pos.lookahead(false, |pos| {
                                pos.match_string("'").or_else(|pos| {
                                    pos.match_string("\\")
                                })
                            }).and_then(|pos| {
                                pos.skip(1)
                            })
                        }).or_else(|pos| {
                            escape(pos, state)
                        })
                    }).and_then(|pos| {
                        single_quote(pos, state)
                    })
                })
            })
        }

        fn single_quote<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::single_quote, pos, |_, pos| {
                pos.match_string("'")
            })
        }

        fn escape<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.sequence(|pos| {
                pos.match_string("\\").and_then(|pos| {
                    pos.match_string("n").or_else(|pos| {
                        pos.match_string("r")
                    }).or_else(|pos| {
                        pos.match_string("t")
                    }).or_else(|pos| {
                        pos.match_string("\\")
                    }).or_else(|pos| {
                        pos.match_string("0")
                    }).or_else(|pos| {
                        pos.match_string("'")
                    }).or_else(|pos| {
                        pos.match_string("\"")
                    }).or_else(|pos| {
                        unicode(pos, state)
                    }).or_else(|pos| {
                        code(pos, state)
                    })
                })
            })
        }

        fn unicode<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.sequence(|pos| {
                pos.match_string("u").and_then(|pos| {
                    pos.match_string("{")
                }).and_then(|pos| {
                    hex_digit(pos, state)
                }).and_then(|pos| {
                    hex_digit(pos, state)
                }).and_then(|pos| {
                    pos.optional(|pos| {
                        hex_digit(pos, state)
                    })
                }).and_then(|pos| {
                    pos.optional(|pos| {
                        hex_digit(pos, state)
                    })
                }).and_then(|pos| {
                    pos.optional(|pos| {
                        hex_digit(pos, state)
                    })
                }).and_then(|pos| {
                    pos.optional(|pos| {
                        hex_digit(pos, state)
                    })
                }).and_then(|pos| {
                    pos.match_string("}")
                })
            })
        }

        fn code<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.sequence(|pos| {
                pos.match_string("x").and_then(|pos| {
                    hex_digit(pos, state)
                }).and_then(|pos| {
                    hex_digit(pos, state)
                })
            })
        }

        fn hex_digit<I: Input>(
            pos: Position<I>,
            _: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.match_range('0'..'9').or_else(|pos| {
                pos.match_range('a'..'f')
            }).or_else(|pos| {
                pos.match_range('A'..'F')
            })
        }

        fn skip<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.sequence(|pos| {
                pos.repeat(|pos| {
                    whitespace(pos, state)
                }).and_then(|pos| {
                    pos.repeat(|pos| {
                        pos.sequence(|pos| {
                            pos.optional(|pos| {
                                comment(pos, state)
                            }).and_then(|pos| {
                                pos.sequence(|pos| {
                                    whitespace(pos, state).and_then(|pos| {
                                        pos.repeat(|pos| {
                                            whitespace(pos, state)
                                        })
                                    })
                                })
                            })
                        })
                    })
                })
            })
        }

        fn whitespace<I: Input>(
            pos: Position<I>,
            _: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.match_string(" ").or_else(|pos| {
                pos.match_string("\t")
            }).or_else(|pos| {
                pos.match_string("\r")
            }).or_else(|pos| {
                pos.match_string("\n")
            })
        }

        fn comment<I: Input>(
            pos: Position<I>,
            _: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.sequence(|pos| {
                pos.match_string("//").and_then(|pos| {
                    pos.repeat(|pos| {
                        pos.sequence(|pos| {
                            pos.lookahead(false, |pos| {
                                pos.match_string("\n")
                            }).and_then(|pos| {
                                pos.skip(1)
                            })
                        })
                    })
                })
            })
        }

        pest::state(input, move |mut state, pos| {
            match rule {
                GrammarRule::grammar_rules => grammar_rules(pos, &mut state),
                GrammarRule::soi => soi(pos, &mut state),
                GrammarRule::eoi => eoi(pos, &mut state),
                GrammarRule::grammar_rule => grammar_rule(pos, &mut state),
                GrammarRule::assignment_operator => assignment_operator(pos, &mut state),
                GrammarRule::silent_modifier => silent_modifier(pos, &mut state),
                GrammarRule::atomic_modifier => atomic_modifier(pos, &mut state),
                GrammarRule::non_atomic_modifier => non_atomic_modifier(pos, &mut state),
                GrammarRule::opening_brace => opening_brace(pos, &mut state),
                GrammarRule::closing_brace => closing_brace(pos, &mut state),
                GrammarRule::opening_paren => opening_paren(pos, &mut state),
                GrammarRule::closing_paren => closing_paren(pos, &mut state),
                GrammarRule::expression => expression(pos, &mut state),
                GrammarRule::primary => primary(pos, &mut state),
                GrammarRule::positive_predicate_operator => {
                    positive_predicate_operator(pos, &mut state)
                },
                GrammarRule::negative_predicate_operator => {
                    negative_predicate_operator(pos, &mut state)
                },
                GrammarRule::sequence_operator => sequence_operator(pos, &mut state),
                GrammarRule::choice_operator => choice_operator(pos, &mut state),
                GrammarRule::optional_operator => optional_operator(pos, &mut state),
                GrammarRule::repeat_operator => repeat_operator(pos, &mut state),
                GrammarRule::repeat_once_operator => repeat_once_operator(pos, &mut state),
                GrammarRule::push => push(pos, &mut state),
                GrammarRule::identifier => identifier(pos, &mut state),
                GrammarRule::string => string(pos, &mut state),
                GrammarRule::quote => quote(pos, &mut state),
                GrammarRule::insensitive_string => insensitive_string(pos, &mut state),
                GrammarRule::range => range(pos, &mut state),
                GrammarRule::range_operator => range_operator(pos, &mut state),
                GrammarRule::character => character(pos, &mut state),
                GrammarRule::single_quote => single_quote(pos, &mut state)
            }
        })
    }
}

pub fn consume_rules<I: Input>(pairs: Pairs<GrammarRule, I>) -> (Vec<Rule>, Vec<Ident>) {
    let defaults = validator::validate_pairs(pairs.clone());
    let rules = consume_rules_with_spans(pairs);

    validator::validate_ast(&rules);

    (rules.into_iter().map(|(rule, _)| rule).collect(), defaults)
}

fn consume_rules_with_spans<I: Input>(pairs: Pairs<GrammarRule, I>) -> Vec<(Rule, Span<I>)> {
    let climber = PrecClimber::new(vec![
        Operator::new(GrammarRule::choice_operator, Assoc::Left),
        Operator::new(GrammarRule::sequence_operator, Assoc::Left)
    ]);

    pairs.filter(|pair| pair.as_rule() == GrammarRule::grammar_rule).map(|pair| {
        let mut pairs = pair.into_inner().peekable();

        let span = pairs.next().unwrap().into_span();
        let name = Ident::new(span.capture());

        pairs.next().unwrap(); // assignment_operator

        let ty = if pairs.peek().unwrap().as_rule() != GrammarRule::opening_brace {
            match pairs.next().unwrap().as_rule() {
                GrammarRule::silent_modifier => RuleType::Silent,
                GrammarRule::atomic_modifier => RuleType::Atomic,
                GrammarRule::non_atomic_modifier => RuleType::NonAtomic,
                _ => unreachable!()
            }
        } else {
            RuleType::Normal
        };

        pairs.next().unwrap(); // opening_brace

        let expr = consume_expr(pairs.next().unwrap().into_inner().peekable(), &climber);

        let rule = Rule {
            name,
            ty,
            expr
        };

        (rule, span)
    }).collect()
}

fn consume_expr<I: Input>(
    pairs: Peekable<Pairs<GrammarRule, I>>,
    climber: &PrecClimber<GrammarRule>
) -> Expr {
    fn unaries<I: Input>(
        mut pairs: Peekable<Pairs<GrammarRule, I>>,
        climber: &PrecClimber<GrammarRule>
    ) -> Expr {
        let pair = pairs.next().unwrap();

        match pair.as_rule() {
            GrammarRule::positive_predicate_operator => {
                Expr::PosPred(Box::new(unaries(pairs, climber)))
            }
            GrammarRule::negative_predicate_operator => {
                Expr::NegPred(Box::new(unaries(pairs, climber)))
            }
            other_rule => {
                let expr = match other_rule {
                    GrammarRule::expression => consume_expr(pair.into_inner().peekable(), climber),
                    GrammarRule::push => {
                        let mut pairs = pair.into_inner();
                        pairs.next().unwrap(); // opening_paren
                        let pair = pairs.next().unwrap();

                        let expr = consume_expr(pair.into_inner().peekable(), climber);
                        Expr::Push(Box::new(expr))

                    }
                    GrammarRule::identifier => Expr::Ident(Ident::new(pair.into_span().capture())),
                    GrammarRule::string => {
                        let span = pair.into_span();
                        let string = span.capture();
                        Expr::Str(string[1..string.len() - 1].to_owned())
                    }
                    GrammarRule::insensitive_string => {
                        let span = pair.into_span();
                        let string = span.capture();
                        Expr::Insens(string[1..string.len() - 1].to_owned())
                    }
                    GrammarRule::range => {
                        let span = pairs.next().unwrap().into_span();
                        let start = span.capture();
                        let span = pairs.next().unwrap().into_span();
                        let end = span.capture();

                        Expr::Range(
                            start[1..start.len() - 1].to_owned(),
                            end[1..end.len() - 1].to_owned()
                        )
                    }
                    _ => unreachable!()
                };

                pairs.fold(expr, |expr, pair| {
                    match pair.as_rule() {
                        GrammarRule::optional_operator => Expr::Opt(Box::new(expr)),
                        GrammarRule::repeat_operator => Expr::Rep(Box::new(expr)),
                        GrammarRule::repeat_once_operator => Expr::RepOnce(Box::new(expr)),
                        _ => unreachable!()
                    }
                })
            }

        }
    }

    let primary = |pair: Pair<GrammarRule, I>| {
        unaries(pair.into_inner().peekable(), climber)
    };
    let infix = |lhs: Expr, op: Pair<GrammarRule, I>, rhs: Expr| {
        match op.as_rule() {
            GrammarRule::sequence_operator => Expr::Seq(Box::new(lhs), Box::new(rhs)),
            GrammarRule::choice_operator => Expr::Choice(Box::new(lhs), Box::new(rhs)),
            _ => unreachable!()
        }
    };

    climber.climb(pairs, primary, infix)
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use pest::inputs::StringInput;

    use super::*;

    #[test]
    fn rules() {
        parses_to! {
            parser: GrammarParser,
            input: "a = { b } c = { d }",
            rule: GrammarRule::grammar_rules,
            tokens: [
                grammar_rule(0, 9, [
                    identifier(0, 1),
                    assignment_operator(2, 3),
                    opening_brace(4, 5),
                    expression(6, 7, [
                        primary(6, 7, [
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
                        primary(16, 17, [
                            identifier(16, 17)
                        ])
                    ]),
                    closing_brace(18, 19)
                ]),
                eoi(19, 19)
            ]
        };
    }

    #[test]
    fn rule() {
        parses_to! {
            parser: GrammarParser,
            input: "a = !@ { b ~ c }",
            rule: GrammarRule::grammar_rule,
            tokens: [
                grammar_rule(0, 16, [
                    identifier(0, 1),
                    assignment_operator(2, 3),
                    non_atomic_modifier(4, 6),
                    opening_brace(7, 8),
                    expression(9, 14, [
                        primary(9, 10, [
                            identifier(9, 10)
                        ]),
                        sequence_operator(11, 12),
                        primary(13, 14, [
                            identifier(13, 14)
                        ])
                    ]),
                    closing_brace(15, 16)
                ])
            ]
        };
    }

    #[test]
    fn expression() {
        parses_to! {
            parser: GrammarParser,
            input: "_a | 'a'..'b' ~ !^\"abc\" ~ (d | e)*?",
            rule: GrammarRule::expression,
            tokens: [
                expression(0, 35, [
                    primary(0, 2, [
                        identifier(0, 2)
                    ]),
                    choice_operator(3, 4),
                    primary(5, 13, [
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
                    primary(16, 23, [
                        negative_predicate_operator(16, 17),
                        insensitive_string(17, 23, [
                            string(18, 23, [
                                quote(18, 19),
                                quote(22, 23)
                            ])
                        ])
                    ]),
                    sequence_operator(24, 25),
                    primary(26, 35, [
                        expression(27, 32, [
                            primary(27, 28, [
                                identifier(27, 28)
                            ]),
                            choice_operator(29, 30),
                            primary(31, 32, [
                                identifier(31, 32)
                            ])
                        ]),
                        repeat_operator(33, 34),
                        optional_operator(34, 35)
                    ])
                ])
            ]
        };
    }

    #[test]
    fn push() {
        parses_to! {
            parser: GrammarParser,
            input: "push ( a )",
            rule: GrammarRule::push,
            tokens: [
                push(0, 10, [
                    opening_paren(5, 6),
                    expression(7, 8, [
                        primary(7, 8, [
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
            parser: GrammarParser,
            input: "_a8943",
            rule: GrammarRule::identifier,
            tokens: [
                identifier(0, 6)
            ]
        };
    }

    #[test]
    fn string() {
        parses_to! {
            parser: GrammarParser,
            input: "\"aaaaa\\n\\r\\t\\\\\\0\\'\\\"\\x0F\\u{123abC}\\u{12}aaaaa\"",
            rule: GrammarRule::string,
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
            parser: GrammarParser,
            input: "^  \"\\\"hi\"",
            rule: GrammarRule::insensitive_string,
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
            parser: GrammarParser,
            input: "'\\n' .. '\\x1a'",
            rule: GrammarRule::range,
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
            parser: GrammarParser,
            input: "'\\u{123abC}'",
            rule: GrammarRule::character,
            tokens: [
                character(0, 12, [
                    single_quote(0, 1),
                    single_quote(11, 12)
                ])
            ]
        };
    }

    #[test]
    fn comment() {
        parses_to! {
            parser: GrammarParser,
            input: "a ~    // asda\n b",
            rule: GrammarRule::expression,
            tokens: [
                expression(0, 17, [
                    primary(0, 1, [
                        identifier(0, 1)
                    ]),
                    sequence_operator(2, 3),
                    primary(16, 17, [
                        identifier(16, 17)
                    ])
                ])
            ]
        };
    }

    #[test]
    fn ast() {
        let input = Rc::new(StringInput::new("rule = _{ a ~ b | !(c | push(d))?* }".to_owned()));

        let pairs = GrammarParser::parse(GrammarRule::grammar_rules, input.clone()).unwrap();
        let ast = consume_rules_with_spans(pairs);
        let ast: Vec<_> = ast.into_iter().map(|(rule, _)| rule).collect();

        assert_eq!(ast, vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Silent,
                expr: Expr::Choice(
                    Box::new(Expr::Seq(
                        Box::new(Expr::Ident(Ident::new("a"))),
                        Box::new(Expr::Ident(Ident::new("b")))
                    )),
                    Box::new(Expr::NegPred(
                        Box::new(Expr::Rep(
                            Box::new(Expr::Opt(
                                Box::new(Expr::Choice(
                                    Box::new(Expr::Ident(Ident::new("c"))),
                                    Box::new(Expr::Push(
                                        Box::new(Expr::Ident(Ident::new("d")))
                                    )
                                ))
                            ))
                        ))
                    ))
                ))
            }
        ]);
    }
}
