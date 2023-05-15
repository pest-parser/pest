#[macro_use]
extern crate pest_derive;
extern crate pest;

use pest::Parser;

#[derive(Parser)]
#[grammar = "../examples/help-menu.pest"]
struct HelpMenuGrammar;

const INPUT: &str = r"cli help
cli positional-command <required-single-argument> [optional-single-argument]
cli [choice | of | these | options]
cli <choice | of | these | required | options>
";

fn main() {
    match HelpMenuGrammar::parse(Rule::HelpMenu, &INPUT) {
        Ok(mut file) => {
            let file = file.next().expect("Infallible");
            for line in file.into_inner() {
                match line.as_rule() {
                    Rule::Command => {
                        println!("Line: {:#?}", line);
                    }
                    _ => (),
                }
            }
        }
        Err(e) => println!("Error parsing input: {}", e),
    }
}
