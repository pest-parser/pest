extern crate pest;

use std::io::{self, Write};

use pest::{state, Error, Parser, ParserState, ParseResult};
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
        fn expr(state: ParserState<Rule>) -> ParseResult<Rule> {
            state.sequence(|state| {
                state.repeat(|state| paren(state)).and_then(|state| state.end_of_input())
            })
        }

        fn paren(state: ParserState<Rule>) -> ParseResult<Rule> {
            state.rule(Rule::paren, |s| {
                s.sequence(|s| {
                    s.match_string("(")
                        .and_then(|s| {
                            s.optional(|s| {
                                s.sequence(|s| {
                                    s.lookahead(true, |s| {
                                        s.match_string("(")
                                    })
                                    .and_then(|s| s.repeat(|s| paren(s)))
                                })
                            })
                        })
                        .and_then(|s| {
                            s.rule(Rule::paren_end, |s| s.match_string(")"))
                        })
                })
            })
        }

        state(input,|state| match rule {
            Rule::expr => expr(state),
            Rule::paren => paren(state),
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
