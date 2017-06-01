// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![feature(test)]

extern crate test;
extern crate pest;

use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

use test::Bencher;

use pest::inputs::{Input, Position, Span, StringInput};
use pest::iterators::{Pair, Pairs};
use pest::{Error, Parser, ParserState, state};

#[allow(non_camel_case_types)]
#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Rule {
    json,
    object,
    pair,
    array,
    value,
    string,
    escape,
    unicode,
    hex,
    number,
    int,
    exp,
    bool,
    null
}

struct JsonParser;

impl Parser<Rule> for JsonParser {
    fn parse<I: Input>(rule: Rule, input: Rc<I>) -> Result<Pairs<Rule, I>, Error<Rule, I>> {
        fn json<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            value(pos, state)
        }

        fn object<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::object, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        pos.match_string("{").and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            pair(pos, state)
                        }).and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            pos.repeat(|pos| {
                                state.sequence(move |state| {
                                    pos.sequence(|pos| {
                                        pos.match_string(",").and_then(|pos| {
                                            skip(pos, state)
                                        }).and_then(|pos| {
                                            pair(pos, state)
                                        }).and_then(|pos| {
                                            skip(pos, state)
                                        })
                                    })
                                })
                            })
                        }).and_then(|pos| {
                            pos.match_string("}")
                        })
                    })
                }).or_else(|pos| {
                    state.sequence(move |state| {
                        pos.sequence(|pos| {
                            pos.match_string("{").and_then(|pos| {
                                skip(pos, state)
                            }).and_then(|pos| {
                                pos.match_string("}")
                            })
                        })
                    })
                })
            })
        }

        fn pair<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::pair, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        string(pos, state).and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            pos.match_string(":")
                        }).and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            value(pos, state)
                        })
                    })
                })
            })
        }

        fn array<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::array, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        pos.match_string("[").and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            value(pos, state)
                        }).and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            pos.repeat(|pos| {
                                state.sequence(move |state| {
                                    pos.sequence(|pos| {
                                        pos.match_string(",").and_then(|pos| {
                                            skip(pos, state)
                                        }).and_then(|pos| {
                                            value(pos, state)
                                        }).and_then(|pos| {
                                            skip(pos, state)
                                        })
                                    })
                                })
                            })
                        }).and_then(|pos| {
                            pos.match_string("]")
                        })
                    })
                }).or_else(|pos| {
                    state.sequence(move |state| {
                        pos.sequence(|pos| {
                            pos.match_string("[").and_then(|pos| {
                                skip(pos, state)
                            }).and_then(|pos| {
                                pos.match_string("]")
                            })
                        })
                    })
                })
            })
        }

        fn value<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::value, pos, |state, pos| {
                string(pos, state).or_else(|pos| {
                    number(pos, state)
                }).or_else(|pos| {
                    object(pos, state)
                }).or_else(|pos| {
                    array(pos, state)
                }).or_else(|pos| {
                    bool(pos, state)
                }).or_else(|pos| {
                    null(pos, state)
                })
            })
        }

        fn string<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::string, pos, |state, pos| {
                pos.sequence(|pos| {
                    pos.match_string("\"").and_then(|pos| {
                        pos.repeat(|pos| {
                            escape(pos, state).or_else(|pos| {
                                pos.sequence(|pos| {
                                    state.lookahead(false, move |_| {
                                        pos.lookahead(false, |pos| {
                                            pos.match_string("\"").or_else(|pos| {
                                                pos.match_string("\\")
                                            })
                                        })
                                    }).and_then(|pos| {
                                        pos.skip(1)
                                    })
                                })
                            })
                        })
                    }).and_then(|pos| {
                        pos.match_string("\"")
                    })
                })
            })
        }

        fn escape<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.sequence(|pos| {
                pos.match_string("\\").and_then(|pos| {
                    pos.match_string("\"").or_else(|pos| {
                        pos.match_string("\\")
                    }).or_else(|pos| {
                        pos.match_string("/")
                    }).or_else(|pos| {
                        pos.match_string("b")
                    }).or_else(|pos| {
                        pos.match_string("f")
                    }).or_else(|pos| {
                        pos.match_string("n")
                    }).or_else(|pos| {
                        pos.match_string("r")
                    }).or_else(|pos| {
                        pos.match_string("t")
                    }).or_else(|pos| {
                        unicode(pos, state)
                    })
                })
            })
        }

        fn unicode<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.sequence(|pos| {
                pos.match_string("u").and_then(|pos| {
                    hex(pos, state)
                }).and_then(|pos| {
                    hex(pos, state)
                }).and_then(|pos| {
                    hex(pos, state)
                })
            })
        }

        fn hex<I: Input>(
            pos: Position<I>,
            _: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.match_range('0'..'9').or_else(|pos| {
                pos.match_range('a'..'f')
            }).or_else(|pos| {
                pos.match_range('A'..'F')
            })
        }

        fn number<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::number, pos, |state, pos| {
                pos.sequence(|pos| {
                    pos.optional(|pos| {
                        pos.match_string("-")
                    }).and_then(|pos| {
                        int(pos, state)
                    }).and_then(|pos| {
                        pos.optional(|pos| {
                            pos.sequence(|pos| {
                                pos.match_string(".").and_then(|pos| {
                                    pos.match_range('0'..'9')
                                }).and_then(|pos| {
                                    pos.repeat(|pos| {
                                        pos.match_range('0'..'9')
                                    })
                                }).and_then(|pos| {
                                    pos.optional(|pos| {
                                        exp(pos, state)
                                    })
                                }).or_else(|pos| {
                                    exp(pos, state)
                                })
                            })
                        })
                    })
                })
            })
        }

        fn int<I: Input>(
            pos: Position<I>,
            _: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.match_string("0").or_else(|pos| {
                pos.sequence(|pos| {
                    pos.match_range('1'..'9').and_then(|pos| {
                        pos.repeat(|pos| {
                            pos.match_range('0'..'9')
                        })
                    })
                })
            })
        }

        fn exp<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.sequence(|pos| {
                pos.match_string("E").or_else(|pos| {
                    pos.match_string("e")
                }).and_then(|pos| {
                    pos.optional(|pos| {
                        pos.match_string("+").or_else(|pos| {
                            pos.match_string("-")
                        })
                    })
                }).and_then(|pos| {
                    int(pos, state)
                })
            })
        }

        fn bool<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::bool, pos, |_, pos| {
                pos.match_string("true").or_else(|pos| {
                    pos.match_string("false")
                })
            })
        }

        fn null<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::null, pos, |_, pos| {
                pos.match_string("null")
            })
        }

        fn skip<I: Input>(
            pos: Position<I>,
            _: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.repeat(|pos| {
                pos.match_string(" ").or_else(|pos| {
                    pos.match_string("\t")
                }).or_else(|pos| {
                    pos.match_string("\r")
                }).or_else(|pos| {
                    pos.match_string("\n")
                })
            })
        }

        state(input, move |mut state, pos| {
            match rule {
                Rule::json => json(pos, &mut state),
                Rule::object => object(pos, &mut state),
                Rule::pair => pair(pos, &mut state),
                Rule::array => array(pos, &mut state),
                Rule::value => value(pos, &mut state),
                Rule::string => string(pos, &mut state),
                Rule::escape => escape(pos, &mut state),
                Rule::unicode => unicode(pos, &mut state),
                Rule::hex => hex(pos, &mut state),
                Rule::number => number(pos, &mut state),
                Rule::int => int(pos, &mut state),
                Rule::exp => exp(pos, &mut state),
                Rule::bool => bool(pos, &mut state),
                Rule::null => null(pos, &mut state)
            }
        })
    }
}

enum Json<I: Input> {
    Null,
    Bool(bool),
    Number(f64),
    String(Span<I>),
    Array(Vec<Json<I>>),
    Object(HashMap<Span<I>, Json<I>>)
}

fn consume<I: Input>(pair: Pair<Rule, I>) -> Json<I> {
    fn value<I: Input>(pair: Pair<Rule, I>) -> Json<I> {
        let pair = pair.into_inner().next().unwrap();

        match pair.as_rule() {
            Rule::null => Json::Null,
            Rule::bool => {
                match pair.into_span().capture() {
                    "false" => Json::Bool(false),
                    "true" => Json::Bool(true),
                    _ => unreachable!()
                }
            }
            Rule::number => {
                Json::Number(pair.into_span().capture().parse().unwrap())
            }
            Rule::string => {
                Json::String(pair.into_span())
            }
            Rule::array => {
                Json::Array(pair.into_inner().map(|pos| value(pos)).collect())
            }
            Rule::object => {
                let pairs = pair.into_inner().map(|pos| {
                    let mut pair = pos.into_inner();

                    let key = pair.next().unwrap().into_span();
                    let value = value(pair.next().unwrap());

                    (key, value)
                });

                Json::Object(pairs.collect())
            }
            _ => unreachable!()
        }
    }

    value(pair)
}

#[bench]
fn data(b: &mut Bencher) {
    let mut file = File::open("benches/data.json").unwrap();
    let mut data = String::new();

    file.read_to_string(&mut data).unwrap();

    let input = Rc::new(StringInput::new(data.to_owned()));

    b.iter(|| {
        consume(JsonParser::parse(Rule::json, input.clone()).unwrap().next().unwrap())
    });
}
