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
        main(&self) -> i32 { // return an i32 in the end
            (&number: number) => { // capture number as &str
                number.parse::<i32>().unwrap()
            },
            (_: addition, left: main(), sign, right: main()) => { // get left & right by calling
                match sign.rule {                                 // main recursively
                    Rule::plus  => left + right,
                    Rule::minus => left - right,
                    _ => unreachable!()
                }
            },
            (_: multiplication, left: main(), sign, right: main()) => {
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

    println!("{}", parser.process()); // prints 44
}
