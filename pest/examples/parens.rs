extern crate pest;

use std::io::{self, Write};
use std::rc::Rc;

use pest::inputs::{Input, Position, StringInput};
use pest::iterators::Pairs;
use pest::{Error, Parser, ParserState, state};

#[allow(non_camel_case_types)]
#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Rule {
    expr,
    paren,
    paren_end
}

struct ParenParser;

impl Parser<Rule> for ParenParser {
    fn parse<I: Input>(rule: Rule, input: Rc<I>) -> Result<Pairs<Rule, I>, Error<Rule, I>> {
        fn expr<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
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

        fn paren<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
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

fn expr<I: Input>(pairs: Pairs<Rule, I>) -> Vec<Paren> {
    pairs.filter(|p| p.rule() == Rule::paren).map(|p| Paren(expr(p.consume()))).collect()
}

fn main() {
    loop {
        let mut line = String::new();

        print!("> ");
        io::stdout().flush().unwrap();

        io::stdin().read_line(&mut line).unwrap();
        line.pop();

        let input = Rc::new(StringInput::new(line));

        match ParenParser::parse(Rule::expr, input) {
            Ok(pairs) => println!("{:?}", expr(pairs)),
            Err(e) => println!("\n{}", e)
        };
    }
}
