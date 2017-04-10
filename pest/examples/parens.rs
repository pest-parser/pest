extern crate pest;

use std::io::{self, Write};
use std::rc::Rc;

use pest::inputs::{Input, Position, Span, StringInput};
use pest::iterators::{Pair, Pairs};
use pest::{Error, Parser, ParserState, state};

#[allow(non_camel_case_types)]
#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Rule {
    expr,
    paren
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
            state.rule(Rule::paren, pos, |pos, state| {
                state.sequence(move |state| {
                    pos.sequence(|p| {
                        p.match_string("(").and_then(|p| {
                            p.optional(|p| {
                                state.sequence(move |state| {
                                    p.sequence(|p| {
                                        state.lookahead(true, move |state| {
                                            p.lookahead(|p| {
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
                            p.match_string(")")
                        })
                    })
                })
            })
        }

        state(input, move |mut state| {
            match rule {
                Rule::expr => expr(state.start(), &mut state),
                Rule::paren => paren(state.start(), &mut state)
            }
        })
    }
}

#[derive(Debug)]
struct Paren(Vec<Paren>);

fn expr<I: Input>(mut pairs: Pairs<Rule, I>) -> Vec<Paren> {
    pairs.filter(|p| p.rule() == Rule::paren).map(|p| Paren(expr(p.consume()))).collect()
}

fn main() {
    loop {
        let mut line = String::new();

        print!("> ");
        io::stdout().flush();

        io::stdin().read_line(&mut line);
        line.pop();

        let input = Rc::new(StringInput::new(line));

        match ParenParser::parse(Rule::expr, input) {
            Ok(pairs) => println!("{:?}", expr(pairs)),
            Err(e) => println!("\n{}", e)
        };
    }
}
