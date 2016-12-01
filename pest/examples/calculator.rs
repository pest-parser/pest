#[macro_use]
extern crate pest;

use pest::prelude::*;

impl_rdp! {
    grammar! {
        // precedence climbing
        expression = _{ // rule is silent because it's the rule we're matching
            { ["("] ~ expression ~ [")"] | number } // primary
            addition       = { plus  | minus } // precedence 0
            multiplication = { times | slash } // precedence 1
            exponention    = {< pow }          // precedence 2; NB: exponention is right associative.
        }
        number = @{ ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) } // atomic because it cannot
        plus   =  { ["+"] }                                       // accept white-space
        minus  =  { ["-"] }
        times  =  { ["*"] }
        slash  =  { ["/"] }
        pow    =  { ["^"] } 

        whitespace = _{ [" "] } // whitespce gets run between all rules
    }

    process! {
        compute(&self) -> i32 { // return an i32 in the end
            (&number: number) => number.parse::<i32>().unwrap(), // capture number as &str
            (_: addition, left: compute(), sign, right: compute()) => { // get left & right by
                match sign.rule {                                       // calling compute
                    Rule::plus  => left + right,
                    Rule::minus => left - right,
                    _ => unreachable!()
                }
            },
            (_: multiplication, left: compute(), sign, right: compute()) => {
                match sign.rule {
                    Rule::times => left * right,
                    Rule::slash => left / right,
                    _ => unreachable!()
                }
            },
            (_: exponention, left: compute(), sign, right: compute()) => {
                match sign.rule {
                    Rule::pow => left.pow(right as u32),
                    _ => unreachable!()
                }
            }
        }
    }
}

fn main() {
    let expressions = [
        "(3 + (9 + 3 * 4 + (3 + 1) / 2 - 4)) * 2",
        "2^2^2^2", // This should be 65536 and not 256 because it is right associative.
    ];
    for exp in expressions.into_iter() {
        let mut parser = Rdp::new(StringInput::new(exp.clone()));
        parser.expression();
        println!("{} = {}", exp, parser.compute());
    }
}
