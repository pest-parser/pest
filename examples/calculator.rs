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
        }
        number = @{ ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) } // atomic because it cannot
        plus   =  { ["+"] }                                       // accept white-space
        minus  =  { ["-"] }
        times  =  { ["*"] }
        slash  =  { ["/"] }

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
            }
        }
    }
}

fn main() {
    let mut parser = Rdp::new(StringInput::new("(3 + (9 + 3 * 4 + (3 + 1) / 2 - 4)) * 2"));

    parser.expression();

    println!("{}", parser.compute()); // prints 44
}
