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
            state.rule(Rule::object, pos, |pos, state| {
                state.sequence(move |state| {
                    pos.sequence(|p| {
                        p.match_string("{").and_then(|p| {
                            skip(p, state)
                        }).and_then(|p| {
                            pair(p, state)
                        }).and_then(|p| {
                            skip(p, state)
                        }).and_then(|p| {
                            p.repeat(|p| {
                                state.sequence(move |state| {
                                    p.sequence(|p| {
                                        p.match_string(",").and_then(|p| {
                                            skip(p, state)
                                        }).and_then(|p| {
                                            pair(p, state)
                                        }).and_then(|p| {
                                            skip(p, state)
                                        })
                                    })
                                })
                            })
                        }).and_then(|p| {
                            p.match_string("}")
                        })
                    })
                }).or_else(|p| {
                    state.sequence(move |state| {
                        p.sequence(|p| {
                            p.match_string("{").and_then(|p| {
                                skip(p, state)
                            }).and_then(|p| {
                                p.match_string("}")
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
            state.rule(Rule::pair, pos, |pos, state| {
                state.sequence(move |state| {
                    pos.sequence(|p| {
                        string(p, state).and_then(|p| {
                            skip(p, state)
                        }).and_then(|p| {
                            p.match_string(":")
                        }).and_then(|p| {
                            skip(p, state)
                        }).and_then(|p| {
                            value(p, state)
                        })
                    })
                })
            })
        }

        fn array<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::array, pos, |pos, state| {
                state.sequence(move |state| {
                    pos.sequence(|p| {
                        p.match_string("[").and_then(|p| {
                            skip(p, state)
                        }).and_then(|p| {
                            value(p, state)
                        }).and_then(|p| {
                            skip(p, state)
                        }).and_then(|p| {
                            p.repeat(|p| {
                                state.sequence(move |state| {
                                    p.sequence(|p| {
                                        p.match_string(",").and_then(|p| {
                                            skip(p, state)
                                        }).and_then(|p| {
                                            value(p, state)
                                        }).and_then(|p| {
                                            skip(p, state)
                                        })
                                    })
                                })
                            })
                        }).and_then(|p| {
                            p.match_string("]")
                        })
                    })
                }).or_else(|p| {
                    state.sequence(move |state| {
                        p.sequence(|p| {
                            p.match_string("[").and_then(|p| {
                                skip(p, state)
                            }).and_then(|p| {
                                p.match_string("]")
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
            state.rule(Rule::value, pos, |pos, state| {
                string(pos, state).or_else(|p| {
                    number(p, state)
                }).or_else(|p| {
                    object(p, state)
                }).or_else(|p| {
                    array(p, state)
                }).or_else(|p| {
                    bool(p, state)
                }).or_else(|p| {
                    null(p, state)
                })
            })
        }

        fn string<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::string, pos, |pos, state| {
                pos.sequence(|p| {
                    p.match_string("\"").and_then(|p| {
                        p.repeat(|p| {
                            escape(p, state).or_else(|p| {
                                p.sequence(|p| {
                                    state.lookahead(false, move |_| {
                                        p.lookahead(false, |p| {
                                            p.match_string("\"").or_else(|p| {
                                                p.match_string("\\")
                                            })
                                        })
                                    }).and_then(|p| {
                                        p.skip(1)
                                    })
                                })
                            })
                        })
                    }).and_then(|p| {
                        p.match_string("\"")
                    })
                })
            })
        }

        fn escape<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.sequence(|p| {
                p.match_string("\\").and_then(|p| {
                    p.match_string("\"").or_else(|p| {
                        p.match_string("\\")
                    }).or_else(|p| {
                        p.match_string("/")
                    }).or_else(|p| {
                        p.match_string("b")
                    }).or_else(|p| {
                        p.match_string("f")
                    }).or_else(|p| {
                        p.match_string("n")
                    }).or_else(|p| {
                        p.match_string("r")
                    }).or_else(|p| {
                        p.match_string("t")
                    }).or_else(|p| {
                        unicode(p, state)
                    })
                })
            })
        }

        fn unicode<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.sequence(|p| {
                p.match_string("u").and_then(|p| {
                    hex(p, state)
                }).and_then(|p| {
                    hex(p, state)
                }).and_then(|p| {
                    hex(p, state)
                })
            })
        }

        fn hex<I: Input>(
            pos: Position<I>,
            _: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.match_range('0'..'9').or_else(|p| {
                p.match_range('a'..'f')
            }).or_else(|p| {
                p.match_range('A'..'F')
            })
        }

        fn number<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::number, pos, |pos, state| {
                pos.sequence(|p| {
                    p.optional(|p| {
                        p.match_string("-")
                    }).and_then(|p| {
                        int(p, state)
                    }).and_then(|p| {
                        p.optional(|p| {
                            p.sequence(|p| {
                                p.match_string(".").and_then(|p| {
                                    p.match_range('0'..'9')
                                }).and_then(|p| {
                                    p.repeat(|p| {
                                        p.match_range('0'..'9')
                                    })
                                }).and_then(|p| {
                                    p.optional(|p| {
                                        exp(p, state)
                                    })
                                }).or_else(|p| {
                                    exp(p, state)
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
            pos.match_string("0").or_else(|p| {
                p.sequence(|p| {
                    p.match_range('1'..'9').and_then(|p| {
                        p.repeat(|p| {
                            p.match_range('0'..'9')
                        })
                    })
                })
            })
        }

        fn exp<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.sequence(|p| {
                p.match_string("E").or_else(|p| {
                    p.match_string("e")
                }).and_then(|p| {
                    p.optional(|p| {
                        p.match_string("+").or_else(|p| {
                            p.match_string("-")
                        })
                    })
                }).and_then(|p| {
                    int(p, state)
                })
            })
        }

        fn bool<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::bool, pos, |pos, _| {
                pos.match_string("true").or_else(|p| {
                    p.match_string("false")
                })
            })
        }

        fn null<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::null, pos, |pos, _| {
                pos.match_string("null")
            })
        }

        fn skip<I: Input>(
            pos: Position<I>,
            _: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.repeat(|p| {
                p.match_string(" ").or_else(|p| {
                    p.match_string("\t")
                }).or_else(|p| {
                    p.match_string("\r")
                }).or_else(|p| {
                    p.match_string("\n")
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
        let pair = pair.consume().next().unwrap();

        match pair.rule() {
            Rule::null => Json::Null,
            Rule::bool => {
                match pair.span().capture() {
                    "false" => Json::Bool(false),
                    "true" => Json::Bool(true),
                    _ => unreachable!()
                }
            }
            Rule::number => {
                Json::Number(pair.span().capture().parse().unwrap())
            }
            Rule::string => {
                Json::String(pair.span())
            }
            Rule::array => {
                Json::Array(pair.consume().map(|p| value(p)).collect())
            }
            Rule::object => {
                let pairs = pair.consume().map(|p| {
                    let mut pair = p.consume();

                    let key = pair.next().unwrap().span();
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
