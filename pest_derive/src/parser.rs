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
            _: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.at_end()
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
                compound_atomic_modifier(pos, state)
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

        fn compound_atomic_modifier<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::compound_atomic_modifier, pos, |_, pos| {
                pos.match_string("$")
            })
        }

        fn non_atomic_modifier<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<GrammarRule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(GrammarRule::non_atomic_modifier, pos, |_, pos| {
                pos.match_string("!")
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
                        term(pos, state).and_then(|pos| {
                            pos.repeat(|pos| {
                                state.sequence(move |state| {
                                    pos.sequence(|pos| {
                                        skip(pos, state).and_then(|pos| {
                                            infix_operator(pos, state)
                                        }).and_then(|pos| {
                                            skip(pos, state)
                                        }).and_then(|pos| {
                                            term(pos, state)
                                        })
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
            state.rule(GrammarRule::term, pos, |state, pos| {
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
                                    opening_paren(pos, state).and_then(|pos| {
                                        skip(pos, state)
                                    }).and_then(|pos| {
                                        expression(pos, state)
                                    }).and_then(|pos| {
                                        skip(pos, state)
                                    }).and_then(|pos| {
                                        closing_paren(pos, state)
                                    })
                                })
                            }).or_else(|pos| {
                                terminal(pos, state)
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

        fn terminal<I: Input>(
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
                GrammarRule::compound_atomic_modifier => compound_atomic_modifier(pos, &mut state),
                GrammarRule::non_atomic_modifier => non_atomic_modifier(pos, &mut state),
                GrammarRule::opening_brace => opening_brace(pos, &mut state),
                GrammarRule::closing_brace => closing_brace(pos, &mut state),
                GrammarRule::opening_paren => opening_paren(pos, &mut state),
                GrammarRule::closing_paren => closing_paren(pos, &mut state),
                GrammarRule::expression => expression(pos, &mut state),
                GrammarRule::term => term(pos, &mut state),
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParserRule<I: Input> {
    pub name: Ident,
    pub span: Span<I>,
    pub ty: RuleType,
    pub node: ParserNode<I>
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParserNode<I: Input> {
    pub expr: ParserExpr<I>,
    pub span: Span<I>
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParserExpr<I: Input> {
    Str(String),
    Insens(String),
    Range(String, String),
    Ident(Ident),
    PosPred(Box<ParserNode<I>>),
    NegPred(Box<ParserNode<I>>),
    Seq(Box<ParserNode<I>>, Box<ParserNode<I>>),
    Choice(Box<ParserNode<I>>, Box<ParserNode<I>>),
    Opt(Box<ParserNode<I>>),
    Rep(Box<ParserNode<I>>),
    RepOnce(Box<ParserNode<I>>),
    Push(Box<ParserNode<I>>)
}

fn convert_rule<I: Input>(rule: ParserRule<I>) -> Rule {
    match rule {
        ParserRule { name, ty, node, .. } => {
            let expr = convert_node(node);

            Rule { name, ty, expr }
        }
    }
}

fn convert_node<I: Input>(node: ParserNode<I>) -> Expr {
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
        ParserExpr::Push(node) => Expr::Push(Box::new(convert_node(*node)))
    }
}

pub fn consume_rules<I: Input>(pairs: Pairs<GrammarRule, I>) -> (Vec<Rule>, Vec<Ident>) {
    let defaults = validator::validate_pairs(pairs.clone());
    let rules = consume_rules_with_spans(pairs);

    validator::validate_ast(&rules);

    (rules.into_iter().map(|rule| convert_rule(rule)).collect(), defaults)
}

fn consume_rules_with_spans<I: Input>(pairs: Pairs<GrammarRule, I>) -> Vec<ParserRule<I>> {
    let climber = PrecClimber::new(vec![
        Operator::new(GrammarRule::choice_operator, Assoc::Left),
        Operator::new(GrammarRule::sequence_operator, Assoc::Left)
    ]);

    pairs.filter(|pair| pair.as_rule() == GrammarRule::grammar_rule).map(|pair| {
        let mut pairs = pair.into_inner().peekable();

        let span = pairs.next().unwrap().into_span();
        let name = Ident::new(span.as_str());

        pairs.next().unwrap(); // assignment_operator

        let ty = if pairs.peek().unwrap().as_rule() != GrammarRule::opening_brace {
            match pairs.next().unwrap().as_rule() {
                GrammarRule::silent_modifier => RuleType::Silent,
                GrammarRule::atomic_modifier => RuleType::Atomic,
                GrammarRule::compound_atomic_modifier => RuleType::CompoundAtomic,
                GrammarRule::non_atomic_modifier => RuleType::NonAtomic,
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
    }).collect()
}

fn consume_expr<I: Input>(
    pairs: Peekable<Pairs<GrammarRule, I>>,
    climber: &PrecClimber<GrammarRule>
) -> ParserNode<I> {
    fn unaries<I: Input>(
        mut pairs: Peekable<Pairs<GrammarRule, I>>,
        climber: &PrecClimber<GrammarRule>
    ) -> ParserNode<I> {
        let pair = pairs.next().unwrap();

        match pair.as_rule() {
            GrammarRule::opening_paren => unaries(pairs, climber),
            GrammarRule::positive_predicate_operator => {
                let node = unaries(pairs, climber);
                let end = node.span.end_pos();

                ParserNode {
                    expr: ParserExpr::PosPred(Box::new(node)),
                    span: pair.into_span().start_pos().span(end)
                }
            }
            GrammarRule::negative_predicate_operator => {
                let node = unaries(pairs, climber);
                let end = node.span.end_pos();

                ParserNode {
                    expr: ParserExpr::NegPred(Box::new(node)),
                    span: pair.into_span().start_pos().span(end)
                }
            }
            other_rule => {
                let node = match other_rule {
                    GrammarRule::expression => consume_expr(pair.into_inner().peekable(), climber),
                    GrammarRule::push => {
                        let start = pair.clone().into_span().start_pos();
                        let mut pairs = pair.into_inner();
                        pairs.next().unwrap(); // opening_paren
                        let pair = pairs.next().unwrap();

                        let node = consume_expr(pair.into_inner().peekable(), climber);
                        let end = node.span.end_pos();

                        ParserNode {
                            expr: ParserExpr::Push(Box::new(node)),
                            span: start.span(end)
                        }

                    }
                    GrammarRule::identifier => {
                        ParserNode {
                            expr: ParserExpr::Ident(Ident::new(pair.as_str())),
                            span: pair.clone().into_span()
                        }
                    }
                    GrammarRule::string => {
                        let string = pair.as_str();
                        ParserNode {
                            expr: ParserExpr::Str(string[1..string.len() - 1].to_owned()),
                            span: pair.clone().into_span()
                        }
                    }
                    GrammarRule::insensitive_string => {
                        let string = pair.as_str();
                        ParserNode {
                            expr: ParserExpr::Insens(string[2..string.len() - 1].to_owned()),
                            span: pair.clone().into_span()
                        }
                    }
                    GrammarRule::range => {
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
                            span: start_pos.span(end_pos)
                        }
                    }
                    _ => unreachable!()
                };

                pairs.fold(node, |node, pair| {
                    match pair.as_rule() {
                        GrammarRule::optional_operator => {
                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::Opt(Box::new(node)),
                                span: start.span(pair.into_span().end_pos())
                            }
                        }
                        GrammarRule::repeat_operator => {
                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::Rep(Box::new(node)),
                                span: start.span(pair.into_span().end_pos())
                            }
                        }
                        GrammarRule::repeat_once_operator => {
                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::RepOnce(Box::new(node)),
                                span: start.span(pair.into_span().end_pos())
                            }
                        }
                        GrammarRule::closing_paren => node,
                        _ => unreachable!()
                    }
                })
            }

        }
    }

    let term = |pair: Pair<GrammarRule, I>| {
        unaries(pair.into_inner().peekable(), climber)
    };
    let infix = |lhs: ParserNode<I>, op: Pair<GrammarRule, I>, rhs: ParserNode<I>| {
        match op.as_rule() {
            GrammarRule::sequence_operator => {
                let start = lhs.span.start_pos();
                let end = rhs.span.end_pos();

                ParserNode {
                    expr: ParserExpr::Seq(Box::new(lhs), Box::new(rhs)),
                    span: start.span(end)
                }
            },
            GrammarRule::choice_operator => {
                let start = lhs.span.start_pos();
                let end = rhs.span.end_pos();

                ParserNode {
                    expr: ParserExpr::Choice(Box::new(lhs), Box::new(rhs)),
                    span: start.span(end)
                }
            },
            _ => unreachable!()
        }
    };

    climber.climb(pairs, term, infix)
}

#[cfg(test)]
mod tests {
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
            parser: GrammarParser,
            input: "a = ! { b ~ c }",
            rule: GrammarRule::grammar_rule,
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
            parser: GrammarParser,
            input: "_a | 'a'..'b' ~ !^\"abc\" ~ (d | e)*?",
            rule: GrammarRule::expression,
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
    fn push() {
        parses_to! {
            parser: GrammarParser,
            input: "push ( a )",
            rule: GrammarRule::push,
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
            parser: GrammarParser,
            input: "0",
            rule: GrammarRule::grammar_rules,
            positives: vec![GrammarRule::identifier],
            negatives: vec![],
            pos: 0
        };
    }

    #[test]
    fn missing_assignment_operator() {
        fails_with! {
            parser: GrammarParser,
            input: "a {}",
            rule: GrammarRule::grammar_rules,
            positives: vec![GrammarRule::assignment_operator],
            negatives: vec![],
            pos: 2
        };
    }

    #[test]
    fn wrong_modifier() {
        fails_with! {
            parser: GrammarParser,
            input: "a = *{}",
            rule: GrammarRule::grammar_rules,
            positives: vec![
                GrammarRule::silent_modifier,
                GrammarRule::atomic_modifier,
                GrammarRule::compound_atomic_modifier,
                GrammarRule::non_atomic_modifier,
                GrammarRule::opening_brace
            ],
            negatives: vec![],
            pos: 4
        };
    }

    #[test]
    fn missing_opening_brace() {
        fails_with! {
            parser: GrammarParser,
            input: "a = _",
            rule: GrammarRule::grammar_rules,
            positives: vec![GrammarRule::opening_brace],
            negatives: vec![],
            pos: 5
        };
    }

    #[test]
    fn empty_rule() {
        fails_with! {
            parser: GrammarParser,
            input: "a = {}",
            rule: GrammarRule::grammar_rules,
            positives: vec![GrammarRule::expression],
            negatives: vec![],
            pos: 5
        };
    }

    #[test]
    fn missing_rhs() {
        fails_with! {
            parser: GrammarParser,
            input: "a = { b ~ }",
            rule: GrammarRule::grammar_rules,
            positives: vec![GrammarRule::term],
            negatives: vec![],
            pos: 10
        };
    }

    #[test]
    fn wrong_op() {
        fails_with! {
            parser: GrammarParser,
            input: "a = { b % }",
            rule: GrammarRule::grammar_rules,
            positives: vec![
                GrammarRule::closing_brace,
                GrammarRule::sequence_operator,
                GrammarRule::choice_operator,
                GrammarRule::optional_operator,
                GrammarRule::repeat_operator,
                GrammarRule::repeat_once_operator
            ],
            negatives: vec![],
            pos: 8
        };
    }

    #[test]
    fn missing_closing_paren() {
        fails_with! {
            parser: GrammarParser,
            input: "a = { (b }",
            rule: GrammarRule::grammar_rules,
            positives: vec![
                GrammarRule::closing_paren,
                GrammarRule::sequence_operator,
                GrammarRule::choice_operator,
                GrammarRule::optional_operator,
                GrammarRule::repeat_operator,
                GrammarRule::repeat_once_operator
            ],
            negatives: vec![],
            pos: 9
        };
    }

    #[test]
    fn missing_term() {
        fails_with! {
            parser: GrammarParser,
            input: "a = { ! }",
            rule: GrammarRule::grammar_rules,
            positives: vec![
                GrammarRule::opening_paren,
                GrammarRule::positive_predicate_operator,
                GrammarRule::negative_predicate_operator,
                GrammarRule::push,
                GrammarRule::identifier,
                GrammarRule::quote,
                GrammarRule::insensitive_string,
                GrammarRule::single_quote
            ],
            negatives: vec![],
            pos: 8
        };
    }

    #[test]
    fn string_missing_ending_quote() {
        fails_with! {
            parser: GrammarParser,
            input: "a = { \" }",
            rule: GrammarRule::grammar_rules,
            positives: vec![GrammarRule::quote],
            negatives: vec![],
            pos: 9
        };
    }

    #[test]
    fn insensitive_missing_string() {
        fails_with! {
            parser: GrammarParser,
            input: "a = { ^ }",
            rule: GrammarRule::grammar_rules,
            positives: vec![GrammarRule::string],
            negatives: vec![],
            pos: 8
        };
    }

    #[test]
    fn char_missing_ending_single_quote() {
        fails_with! {
            parser: GrammarParser,
            input: "a = { \' }",
            rule: GrammarRule::grammar_rules,
            positives: vec![GrammarRule::single_quote],
            negatives: vec![],
            pos: 8
        };
    }

    #[test]
    fn range_missing_range_operator() {
        fails_with! {
            parser: GrammarParser,
            input: "a = { \'a\' }",
            rule: GrammarRule::grammar_rules,
            positives: vec![GrammarRule::range_operator],
            negatives: vec![],
            pos: 10
        };
    }

    #[test]
    fn ast() {
        let input = "rule = _{ a ~ b | !(c | push(d))?* }";

        let pairs = GrammarParser::parse_str(GrammarRule::grammar_rules, input).unwrap();
        let ast = consume_rules_with_spans(pairs);
        let ast: Vec<_> = ast.into_iter().map(|rule| convert_rule(rule)).collect();

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
