extern crate pest;

use std::io::{self, Write};
use std::rc::Rc;

use pest::inputs::{Input, Position};
use pest::iterators::Pairs;
use pest::{Error, Parser, ParserState, state};

#[allow(dead_code, non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Rule {
    expr,
    paren,
    paren_end
}

struct ParenParser;

impl Parser<Rule> for ParenParser {
    fn parse<'i, I: Input<'i>>(rule: Rule, input: Rc<I>) -> Result<Pairs<'i, Rule, I>, Error<'i, Rule, I>> {
        fn expr<'i, I: Input<'i>>(
            pos: Position<'i, I>,
            state: &mut ParserState<'i, Rule, I>
        ) -> Result<Position<'i, I>, Position<'i, I>> {
            state.sequence(move |state| {
                pos.sequence(|p| {
                    p.repeat(|p| {
                        paren(p, state)
                    }).and_then(|p| {
                        p.at_end()
                    })
                })
            })
        }

        fn paren<'i, I: Input<'i>>(
            pos: Position<'i, I>,
            state: &mut ParserState<'i, Rule, I>
        ) -> Result<Position<'i, I>, Position<'i, I>> {
            state.rule(Rule::paren, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|p| {
                        p.match_string("(").and_then(|p| {
                            p.optional(|p| {
                                state.sequence(move |state| {
                                    p.sequence(|p| {
                                        state.lookahead(true, move |_| {
                                            p.lookahead(true, |p| {
                                                p.match_string("(")
                                            })
                                        }).and_then(|p| {
                                            p.repeat(|p| {
                                                paren(p, state)
                                            })
                                        })
                                    })
                                })
                            })
                        }).and_then(|p| {
                            state.rule(Rule::paren_end, p, |_, pos| {
                                pos.match_string(")")
                            })
                        })
                    })
                })
            })
        }

        state(input, move |mut state, pos| {
            match rule {
                Rule::expr => expr(pos, &mut state),
                Rule::paren => paren(pos, &mut state),
                _ => unreachable!()
            }
        })
    }
}

#[derive(Debug)]
struct Paren(Vec<Paren>);

fn expr<'i, I: Input<'i>>(pairs: Pairs<'i, Rule, I>) -> Vec<Paren> {
    pairs.filter(|p| p.as_rule() == Rule::paren).map(|p| Paren(expr(p.into_inner()))).collect()
}

fn main() {
    loop {
        let mut line = String::new();

        print!("> ");
        io::stdout().flush().unwrap();

        io::stdin().read_line(&mut line).unwrap();
        line.pop();

        match ParenParser::parse_str(Rule::expr, &line) {
            Ok(pairs) => println!("{:?}", expr(pairs)),
            Err(e) => println!("\n{}", e)
        };
    }
}
