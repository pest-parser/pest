extern crate pest;

use std::io::{self, Write};

use pest::{state, Error, Parser, ParserState};
use pest::Position;
use pest::iterators::Pairs;

#[allow(dead_code, non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Rule {
    expr,
    paren,
    paren_end
}

struct ParenParser;

impl Parser<Rule> for ParenParser {
    fn parse(rule: Rule, input: &str) -> Result<Pairs<Rule>, Error<Rule>> {
        fn expr<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.sequence(move |state| {
                pos.sequence(|p| p.repeat(|p| paren(p, state)).and_then(|p| p.at_end()))
            })
        }

        fn paren<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::paren, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|p| {
                        p.match_string("(")
                            .and_then(|p| {
                                p.optional(|p| {
                                    state.sequence(move |state| {
                                        p.sequence(|p| {
                                            state
                                                .lookahead(true, move |_| {
                                                    p.lookahead(true, |p| p.match_string("("))
                                                })
                                                .and_then(|p| p.repeat(|p| paren(p, state)))
                                        })
                                    })
                                })
                            })
                            .and_then(|p| {
                                state.rule(Rule::paren_end, p, |_, pos| pos.match_string(")"))
                            })
                    })
                })
            })
        }

        state(input, move |mut state, pos| match rule {
            Rule::expr => expr(pos, &mut state),
            Rule::paren => paren(pos, &mut state),
            _ => unreachable!()
        })
    }
}

#[derive(Debug)]
struct Paren(Vec<Paren>);

fn expr(pairs: Pairs<Rule>) -> Vec<Paren> {
    pairs
        .filter(|p| p.as_rule() == Rule::paren)
        .map(|p| Paren(expr(p.into_inner())))
        .collect()
}

fn main() {
    loop {
        let mut line = String::new();

        print!("> ");
        io::stdout().flush().unwrap();

        io::stdin().read_line(&mut line).unwrap();
        line.pop();

        match ParenParser::parse(Rule::expr, &line) {
            Ok(pairs) => println!("{:?}", expr(pairs)),
            Err(e) => println!("\n{}", e)
        };
    }
}
