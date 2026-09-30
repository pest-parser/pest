// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#![cfg_attr(not(feature = "std"), no_std)]
extern crate alloc;
use alloc::{format, vec::Vec};

#[macro_use]
extern crate pest;
#[macro_use]
extern crate pest_derive;

#[derive(Parser)]
#[grammar_inline = "string = { \"abc\" }"]
struct GrammarParser;

#[test]
fn inline_string() {
    parses_to! {
        parser: GrammarParser,
        input: "abc",
        rule: Rule::string,
        tokens: [
            string(0, 3)
        ]
    };
}

#[cfg(feature = "std")]
mod depth_limit {
    use core::num::NonZeroUsize;
    use std::{env, process::Command, thread};

    use pest::{error::ErrorVariant, Parser};

    #[derive(Parser)]
    #[grammar_inline = r#"
WHITESPACE = _{ " " | "\t" | "\n" | "\r" }
expression = { SOI ~ add_expr ~ EOI }
add_expr = { mul_expr ~ ("+" ~ mul_expr)* }
mul_expr = { primary ~ ("*" ~ primary)* }
primary = { number | "(" ~ add_expr ~ ")" }
number = @{ ASCII_DIGIT+ }
"#]
    struct Calc;

    struct ResetCallLimit;

    impl Drop for ResetCallLimit {
        fn drop(&mut self) {
            pest::set_call_limit(None);
        }
    }

    fn assert_parses(input: &str) {
        let expression = Calc::parse(Rule::expression, input)
            .unwrap_or_else(|error| {
                panic!("failed to parse {} bytes: {:?}", input.len(), error.variant)
            })
            .next()
            .unwrap();
        assert_eq!(expression.as_rule(), Rule::expression);
        assert_eq!(expression.as_span().start(), 0);
        assert_eq!(expression.as_span().end(), input.len());
    }

    fn check_depth_and_width() {
        let _reset = ResetCallLimit;
        pest::set_call_limit(NonZeroUsize::new(50));

        assert_parses("1");
        assert_parses("(1 + 2) * 3");
        for nesting in [300, 50_000] {
            let nested = format!("{}1{}", "(".repeat(nesting), ")".repeat(nesting));
            let flat = format!("{}1", "1+".repeat(nesting));
            assert_eq!(nested.len(), flat.len());
            assert_eq!(
                Calc::parse(Rule::expression, &nested).unwrap_err().variant,
                ErrorVariant::CustomError {
                    message: "call limit reached".into(),
                }
            );
            assert_parses("1 + 2");
            assert_parses(&flat);
        }

        assert_parses(&vec!["1"; 10_000].join("*"));
        assert_parses(&vec!["12 * (3 + 45)"; 10_000].join(" +\t\r\n"));
        assert_parses(&"1".repeat(10_000));
    }

    #[test]
    fn issue_1129_depth_and_width() {
        const CHILD_STACK: &str = "PEST_DEPTH_LIMIT_TEST_STACK";
        const COMPLETED: &str = "depth and width checks completed";

        if let Some(stack_size) = env::var_os(CHILD_STACK) {
            thread::Builder::new()
                .stack_size(stack_size.to_str().unwrap().parse().unwrap())
                .spawn(check_depth_and_width)
                .unwrap()
                .join()
                .unwrap();
            println!("{COMPLETED}");
            return;
        }

        for stack_size in [1024 * 1024, 8 * 1024 * 1024] {
            let output = Command::new(env::current_exe().unwrap())
                .args([
                    "--exact",
                    "depth_limit::issue_1129_depth_and_width",
                    "--nocapture",
                ])
                .env(CHILD_STACK, stack_size.to_string())
                .output()
                .unwrap();
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                output.status.success() && stdout.contains(COMPLETED),
                "depth checks on a {stack_size}-byte stack exited with {}\n{stdout}\n{stderr}",
                output.status
            );
        }
    }
}
