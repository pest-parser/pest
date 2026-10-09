// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#[cfg(feature = "std")]
use pest::Parser;
#[cfg(feature = "std")]
#[macro_use]
extern crate pest_derive;

#[cfg(feature = "std")]
#[derive(Parser)]
#[grammar = "tests/nested_expr.pest"]
struct Calc;

#[cfg(feature = "std")]
mod depth_limit {
    use std::{env, process::Command, thread};

    use pest::error::ErrorVariant;

    use super::*;

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

    fn check_depth() {
        assert_parses("1");
        assert_parses("(1 + 2) * 3");
        let nesting = 50_000;
        let nested = format!("{}1{}", "(".repeat(nesting), ")".repeat(nesting));
        assert_eq!(
            Calc::parse(Rule::expression, &nested).unwrap_err().variant,
            ErrorVariant::CustomError {
                message: "stack limit reached".into(),
            }
        );
        assert_parses("1 + 2");
    }

    fn check_width() {
        for nesting in [300, 50_000] {
            let nested = format!("{}1{}", "(".repeat(nesting), ")".repeat(nesting));
            let flat = format!("{}1", "1+".repeat(nesting));
            assert_eq!(nested.len(), flat.len());
            assert_parses(&flat);
        }
        assert_parses(&vec!["1"; 10_000].join("*"));
        assert_parses(&vec!["12 * (3 + 45)"; 10_000].join(" +\t\r\n"));
        assert_parses(&"1".repeat(10_000));
    }

    fn on_fixed_stacks(test_name: &str, check: fn()) {
        const CHILD_TEST: &str = "PEST_DEPTH_LIMIT_TEST";
        const CHILD_STACK: &str = "PEST_DEPTH_LIMIT_TEST_STACK";
        const COMPLETED: &str = "fixture depth checks completed";

        if env::var(CHILD_TEST).as_deref() == Ok(test_name) {
            thread::Builder::new()
                .stack_size(env::var(CHILD_STACK).unwrap().parse().unwrap())
                .spawn(move || {
                    let _reset = ResetCallLimit;
                    pest::set_call_limit(None);
                    for details in [false, true] {
                        pest::set_error_detail(details);
                        check();
                    }
                })
                .unwrap()
                .join()
                .unwrap();
            println!("{COMPLETED}");
            return;
        }

        for stack_size in [256 * 1024, 1024 * 1024, 8 * 1024 * 1024] {
            let output = Command::new(env::current_exe().unwrap())
                .args(["--exact", test_name, "--nocapture"])
                .env(CHILD_TEST, test_name)
                .env(CHILD_STACK, stack_size.to_string())
                .output()
                .unwrap();
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                output.status.success() && stdout.contains(COMPLETED),
                "{test_name} on a {stack_size}-byte stack exited with {}\n{stdout}\n{stderr}",
                output.status
            );
        }
    }

    #[test]
    fn issue_1129_deep_nesting() {
        on_fixed_stacks("depth_limit::issue_1129_deep_nesting", check_depth);
    }

    #[test]
    fn issue_1129_flat_expressions() {
        on_fixed_stacks("depth_limit::issue_1129_flat_expressions", check_width);
    }
}
