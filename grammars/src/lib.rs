// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! # pest grammars
//!
//! Contains a series of default grammars.

#![doc(
    html_root_url = "https://docs.rs/pest_grammars",
    html_logo_url = "https://raw.githubusercontent.com/pest-parser/pest/master/pest-logo.svg",
    html_favicon_url = "https://raw.githubusercontent.com/pest-parser/pest/master/pest-logo.svg"
)]
#![warn(missing_docs, rust_2018_idioms, unused_qualifications)]

#[macro_use]
extern crate pest_derive;

pub use pest::Parser;

/// Grammar rules of a simplified HTTP request parser
#[allow(missing_docs)]
pub mod http {
    /// HTTP parser.
    #[derive(Parser)]
    #[grammar = "grammars/http.pest"]
    pub struct HttpParser;
}

/// Grammar rules of a sample JSON parser
#[allow(missing_docs)]
pub mod json {
    /// JSON parser.
    #[derive(Parser)]
    #[grammar = "grammars/json.pest"]
    pub struct JsonParser;
}

/// Grammar rules of a sample TOML parser
#[allow(missing_docs)]
pub mod toml {
    /// TOML parser.
    #[derive(Parser)]
    #[grammar = "grammars/toml.pest"]
    pub struct TomlParser;
}

/// Grammar rules of an SQL parser
#[allow(missing_docs)]
pub mod sql {
    /// SQL parser.
    /// Grammar is a tinkered version of the one used in distributed SQL executor module named
    /// [sbroad](https://git.picodata.io/picodata/picodata/sbroad/-/blob/main/sbroad-core/src/frontend/sql/query.pest).
    /// Being a submodule of [Picodata](https://git.picodata.io/picodata/picodata/picodata) (that
    /// operates with Tarantool database) it tries to simulate SQLite flavour (Tarantool uses
    /// SQLite to execute SQL queries).
    #[derive(Parser)]
    #[grammar = "grammars/sql.pest"]
    pub struct SqlParser;
}

#[cfg(test)]
mod tests {
    use pest::iterators::Pairs;
    use std::convert::TryInto;

    use pest::pratt_parser::PrattParser;
    use pest::Parser;

    use crate::{json, sql, toml};

    fn test_toml_deep_nesting(input: &str) {
        const ERROR: &str = "call limit reached";
        pest::set_call_limit(Some(5_000usize.try_into().unwrap()));
        let s = toml::TomlParser::parse(toml::Rule::toml, input);
        assert!(s.is_err());
        assert_eq!(s.unwrap_err().variant.message(), ERROR);
    }

    #[test]
    fn toml_handles_deep_nesting() {
        let sample1 = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/resources/test/tomlfuzzsample1.toml"
        ));
        test_toml_deep_nesting(sample1);
    }

    #[test]
    #[ignore = "this sometimes crashes in the debug mode"]
    fn toml_handles_deep_nesting_unstable() {
        let sample2 = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/resources/test/tomlfuzzsample2.toml"
        ));
        test_toml_deep_nesting(sample2);
    }

    #[test]
    fn json_handles_deep_nesting() {
        let sample1 = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/resources/test/jsonfuzzsample1.json"
        ));
        let sample2 = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/resources/test/jsonfuzzsample2.json"
        ));
        const ERROR: &str = "call limit reached";
        pest::set_call_limit(Some(5_000usize.try_into().unwrap()));
        let s1 = json::JsonParser::parse(json::Rule::json, sample1);
        assert!(s1.is_err());
        assert_eq!(s1.unwrap_err().variant.message(), ERROR);
        let s2 = json::JsonParser::parse(json::Rule::json, sample2);
        assert!(s2.is_err());
        assert_eq!(s2.unwrap_err().variant.message(), ERROR);
    }

    #[test]
    fn sql_check_expressions_priorities() {
        lazy_static::lazy_static! {
            static ref PRATT_PARSER: PrattParser<sql::Rule> = {
                use pest::pratt_parser::{Assoc::{Left, Right}, Op};
                use sql::Rule::{Add, And, Between, ConcatInfixOp, Divide, Eq, Gt, GtEq, In,
                                IsNullPostfix, Lt, LtEq, Multiply, NotEq, Or, Subtract, UnaryNot};

                // Precedence is defined lowest to highest.
                PrattParser::new()
                    .op(Op::infix(Or, Left))
                    .op(Op::infix(Between, Left))
                    .op(Op::infix(And, Left))
                    .op(Op::prefix(UnaryNot))
                    .op(
                        Op::infix(Eq, Right) | Op::infix(NotEq, Right) | Op::infix(NotEq, Right)
                            | Op::infix(Gt, Right) | Op::infix(GtEq, Right) | Op::infix(Lt, Right)
                            | Op::infix(LtEq, Right) | Op::infix(In, Right)
                    )
                    .op(Op::infix(Add, Left) | Op::infix(Subtract, Left))
                    .op(Op::infix(Multiply, Left) | Op::infix(Divide, Left) | Op::infix(ConcatInfixOp, Left))
                    .op(Op::postfix(IsNullPostfix))
            };
        }

        #[derive(Debug, PartialEq, Eq)]
        enum ArithOp {
            Add,
            Mult,
        }

        #[derive(Debug, PartialEq, Eq)]
        enum BoolOp {
            And,
            Or,
            Eq,
            In,
        }

        #[derive(Debug, PartialEq, Eq)]
        enum InfixOp {
            ArithInfix(ArithOp),
            BoolInfix(BoolOp),
        }

        #[derive(Debug, PartialEq, Eq)]
        enum Expr {
            SubQuery,
            Infix {
                left: Box<Expr>,
                op: InfixOp,
                right: Box<Expr>,
            },
            ArithValue(u64),
            BoolConst(bool),
            Not {
                child: Box<Expr>,
            },
            IsNull {
                child: Box<Expr>,
            },
        }

        // Example of SQL expression containing many operators with different priorities.
        // Should be interpreted as
        // `(not ((1 + 1 * 2) = 3)) or ((false is null) and (1 in (select * from t where true)))`
        let input = r#"not 1 + 1 * 2 = 3
                            or false is null
                            and 1 in (
                                select "name", avg("grade") from students
                                where "age" > 14
                                group by "class"
                            )"#;

        let res_pairs = sql::SqlParser::parse(sql::Rule::Expr, input).unwrap();
        fn parse_expr(expression_pairs: Pairs<'_, sql::Rule>) -> Expr {
            PRATT_PARSER
                .map_primary(|primary| match primary.as_rule() {
                    sql::Rule::Expr => parse_expr(primary.into_inner()),
                    sql::Rule::SubQuery => Expr::SubQuery,
                    sql::Rule::Unsigned => {
                        let u64_value = primary.as_str().parse::<u64>().unwrap();
                        Expr::ArithValue(u64_value)
                    }
                    sql::Rule::True | sql::Rule::False => {
                        let bool_value = primary.as_str().parse::<bool>().unwrap();
                        Expr::BoolConst(bool_value)
                    }
                    rule => unreachable!("Expr::parse expected atomic rule, found {:?}", rule),
                })
                .map_infix(|lhs, op, rhs| {
                    let op = match op.as_rule() {
                        sql::Rule::And => InfixOp::BoolInfix(BoolOp::And),
                        sql::Rule::Or => InfixOp::BoolInfix(BoolOp::Or),
                        sql::Rule::Eq => InfixOp::BoolInfix(BoolOp::Eq),
                        sql::Rule::In => InfixOp::BoolInfix(BoolOp::In),
                        sql::Rule::Multiply => InfixOp::ArithInfix(ArithOp::Mult),
                        sql::Rule::Add => InfixOp::ArithInfix(ArithOp::Add),
                        rule => {
                            unreachable!("Expr::parse expected infix operation, found {:?}", rule)
                        }
                    };
                    Expr::Infix {
                        left: Box::new(lhs),
                        op,
                        right: Box::new(rhs),
                    }
                })
                .map_prefix(|op, child| match op.as_rule() {
                    sql::Rule::UnaryNot => Expr::Not {
                        child: Box::new(child),
                    },
                    rule => unreachable!("Expr::parse expected prefix operator, found {:?}", rule),
                })
                .map_postfix(|child, op| match op.as_rule() {
                    sql::Rule::IsNullPostfix => Expr::IsNull {
                        child: Box::new(child),
                    },
                    rule => unreachable!("Expr::parse expected postfix operator, found {:?}", rule),
                })
                .parse(expression_pairs)
        }

        let actual_expr = parse_expr(res_pairs);
        let expected_expr = Expr::Infix {
            op: InfixOp::BoolInfix(BoolOp::Or),
            left: Box::new(Expr::Not {
                child: Box::new(Expr::Infix {
                    left: Box::new(Expr::Infix {
                        left: Box::new(Expr::ArithValue(1)),
                        op: InfixOp::ArithInfix(ArithOp::Add),
                        right: Box::new(Expr::Infix {
                            left: Box::new(Expr::ArithValue(1)),
                            op: InfixOp::ArithInfix(ArithOp::Mult),
                            right: Box::new(Expr::ArithValue(2)),
                        }),
                    }),
                    op: InfixOp::BoolInfix(BoolOp::Eq),
                    right: Box::new(Expr::ArithValue(3)),
                }),
            }),
            right: Box::new(Expr::Infix {
                left: Box::new(Expr::IsNull {
                    child: Box::new(Expr::BoolConst(false)),
                }),
                op: InfixOp::BoolInfix(BoolOp::And),
                right: Box::new(Expr::Infix {
                    left: Box::new(Expr::ArithValue(1)),
                    op: InfixOp::BoolInfix(BoolOp::In),
                    right: Box::new(Expr::SubQuery),
                }),
            }),
        };
        assert_eq!(expected_expr, actual_expr);
    }

    #[test]
    fn sql_parse_attempts_error() {
        pest::set_error_detail(true);

        fn is_whitespace(string: String) -> bool {
            string == "\r\n"
                || (string.len() == 1 && string.chars().next().unwrap().is_whitespace())
        }

        fn rule_to_message(r: &sql::Rule) -> Option<String> {
            match r {
                sql::Rule::CreateTable => Some(String::from("Expected table creation.")),
                sql::Rule::PrimaryKey => Some(String::from(
                    "Add primary key consisting of non nullable table columns.",
                )),
                sql::Rule::CreateUser => Some(String::from("Expected user creation.")),
                sql::Rule::SingleQuotedString => {
                    Some(String::from("Add a string in single qoutes."))
                }
                sql::Rule::Query => Some(String::from("DML query expected.")),
                sql::Rule::Expr => Some(String::from("Expected expression.")),
                _ => None,
            }
        }

        type RuleToMessageBoxed = Box<dyn Fn(&sql::Rule) -> Option<String>>;
        type IsWhiteSpaceBoxed = Box<dyn Fn(String) -> bool>;

        let rule_to_message_boxed: RuleToMessageBoxed = Box::new(rule_to_message);
        let is_whitespace_boxed: IsWhiteSpaceBoxed = Box::new(is_whitespace);

        let retrieve_parse_attempts_error_string = |input| {
            let e = sql::SqlParser::parse(sql::Rule::Command, input).unwrap_err();
            let parse_attempt_error = e
                .parse_attempts_error(input, &rule_to_message_boxed, &is_whitespace_boxed)
                .unwrap();
            format!("{parse_attempt_error}")
        };

        let table_creation_without_primary_key =
            r#"create table t(col_1 int,) distributed by (col_1)"#;
        assert_eq!(
            retrieve_parse_attempts_error_string(table_creation_without_primary_key),
            [
                " --> 1:26",
                "  |",
                "1 | create table t(col_1 int,) distributed by (col_1)",
                "  |                          ^---",
                "  |",
                "  = error: parsing error occurred.",
                r#"    note: expected one of tokens: WHITESPACE, `"`, `-`, `A..Z`, `PRIMARY`, `_`, `a..z`, `А..Я`, `а..я`"#,
                "    help: Expected table creation.",
                "          - Add primary key consisting of non nullable table columns.",
            ]
            .join("\n")
        );

        let user_creation_password_without_single_qoutes = r#"create user
                                                                   Bob password "wrong""#;
        assert_eq!(
            retrieve_parse_attempts_error_string(user_creation_password_without_single_qoutes),
            [
                " --> 2:81",
                "  |",
                r#"2 |                                                                    Bob password "wrong""#,
                "  |                                                                                 ^---",
                "  |",
                "  = error: parsing error occurred.",
                "    note: expected one of tokens: WHITESPACE, `''`, `'`",
                "    help: Expected user creation.",
                "          - Add a string in single qoutes.",
            ]
                .join("\n")
        );

        let invalid_expression_in_projection = r#"select 1 + from t"#;
        assert_eq!(
            retrieve_parse_attempts_error_string(invalid_expression_in_projection),
            [
                " --> 1:12",
                "  |",
                "1 | select 1 + from t",
                "  |            ^---",
                "  |",
                "  = error: parsing error occurred.",
                r#"    note: expected one of tokens: WHITESPACE, `"`, `$`, `''`, `'`, `(`, `+`, `-`, `0..9`, `?`, `CAST`, `EXISTS`, `FALSE`, `NOT`, `NULL`, `TRUE`"#,
                "    note: unexpected token: `FROM`",
                "    help: DML query expected.",
                "          - Expected expression.",
            ]
                .join("\n")
        );
    }
}
