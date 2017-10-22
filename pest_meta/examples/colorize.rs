/// A simple syntax-highlighter for pest
extern crate pest;
extern crate pest_meta;
extern crate colored;

use std::io::{self, Read};
use std::rc::Rc;

use colored::Colorize;
use pest::Parser;
use pest::inputs::StringInput;
use pest::inputs::Input;
use pest_meta::parser::{PestParser, PestRule};

fn main() {
    // Read a pest grammar definition from stdin
    let input = Rc::new(StringInput::new({
        let input = io::stdin();
        let mut code = String::new();
        input.lock().read_to_string(&mut code).unwrap();
        code
    }));

    // Parse the input into a flattened Abstract Syntax Tree (AST). A flattened AST tree is the
    // list of all subtrees of an AST tree in depth-first pre-order of their root nodes (parents
    // occur before children and siblings occur in the same order that their spans occur in the
    // input).
    let ast = match PestParser::parse(PestRule::grammar_rules, input.clone()) {
        Ok(pairs) => pairs,
        Err(err) => {
            println!("{}:\n\n{}", "Parsing error".red(), err);
            return;
        }
    }.flatten();

    // If we limit our tool to not do any nested styles (e.g. no italicized word within a bold
    // sentence), then we can simply select a subsequence of non-overlapping elements from the
    // flattened AST and print their content with the desired style. In between, we print the
    // remaining text (straight from the input, without any special styling).
    
    // We keep track of the number of code points (characters) from the input that have been
    // processed (styled), in order to:
    // - skip sub-trees which overlap with already styled ones to avoid duplicating the text
    // - print the input spans between the styled sub-trees
    // .
    let mut processed = 0;
    let input = input.as_ref();
    // We traverse the flattened AST tree,
    for sub_tree in ast {
        let span = sub_tree.clone().into_span();
        // and we choose which of the sub-trees we want to style.

        // We skip sub-trees which overlap with the already processed part of the input.
        if span.start() < processed {
            continue
        }
        
        let style = match sub_tree.as_rule() {
            // We choose a style for the sub-trees we want to style
            PestRule::string => <&str>::magenta,
            PestRule::identifier => <&str>::cyan,
            PestRule::opening_brace | PestRule::closing_brace => <&str>::bold,
            PestRule::character => <&str>::blue,
            // and skip over the sub-trees that we don't really care about styling.
            _ => continue
        };
        // Now, we print everything between the last processed code point and the beginning of the
        // current sub-tree span using the default text output style. Then we print the span of the
        // sub-tree itself using the selected style.
        print!("{}{}", unsafe {input.slice(processed, span.start())}, style(span.as_str()));
        // We update the record of the last code point we processed.
        processed = span.end();
    }
    // To finish up, we print whatever is left from the input using the default style.
    println!("{}", unsafe {input.slice(processed, input.len())});
}
