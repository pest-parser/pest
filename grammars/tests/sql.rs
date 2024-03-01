// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#[macro_use]
extern crate pest;
extern crate pest_grammars;

use pest_grammars::sql::*;

#[test]
fn sql_simple_select() {
    parses_to! {
        parser: SqlParser,
        input: "select * from table",
        rule: Rule::Command,
        tokens: [
            Query (0, 19, [
                SelectWithOptionalContinuation(0, 19, [
                    Select(0, 19, [
                        Projection(7, 9, [
                            Asterisk(7, 8)
                        ]),
                        Scan(14, 19, [
                            Identifier(14, 19)
                        ])
                    ])
                ])
            ]),
            EOF(19, 19, [EOI(19, 19)])
        ]
    }
}

#[test]
fn sql_create_user() {
    parses_to! {
        parser: SqlParser,
        input: r#"create user "my_user" with password 'strong_password123'"#,
        rule: Rule::Command,
        tokens: [
            CreateUser(0, 56, [
                Identifier(12, 21),
                SingleQuotedString(36, 56)
            ]),
            EOF(56, 56, [EOI(56, 56)])
        ]
    }
}

#[test]
fn sql_insert_from_select() {
    parses_to! {
        parser: SqlParser,
        input: r#"insert into "my_table" ("col_1", "col_2")
                  select "name", "class", avg("age")
                  from "students"
                  where "age" > 15
                  group by "age""#,
        rule: Rule::Command,
        tokens: [
            Query(0, 196, [
                Insert(0, 196, [
                    Identifier(12, 22),
                    TargetColumns(24, 40, [
                        Identifier(24, 31),
                        Identifier(33, 40)
                    ]),
                    Select(60, 196, [
                        Projection(67, 113, [
                            Column(67, 73, [Expr(67, 73, [
                                    IdentifierWithOptionalContinuation(67, 73, [Identifier(67, 73)])
                                ])
                            ]),
                            Column(75, 82, [Expr(75, 82, [
                                    IdentifierWithOptionalContinuation(75, 82, [Identifier(75, 82)])
                                ])
                            ]),
                            Column(84, 113, [
                                Expr(84, 113, [
                                    IdentifierWithOptionalContinuation(84, 94, [
                                        Identifier(84, 87),
                                        FunctionInvocationContinuation(87, 94, [
                                            FunctionArgs(88, 93, [
                                                Expr(88, 93, [
                                                    IdentifierWithOptionalContinuation(88, 93, [
                                                        Identifier(88, 93)
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ]),
                        Scan(118, 147, [Identifier(118, 128)]),
                        Selection(153, 182, [
                            Expr(153, 182, [
                                IdentifierWithOptionalContinuation(153, 159, [Identifier(153, 158)]),
                                Gt(159, 160),
                                Unsigned(161, 163)]
                            )]),
                        GroupBy(191, 196, [
                            Expr(191, 196, [
                                IdentifierWithOptionalContinuation(191, 196, [Identifier(191, 196)])
                            ])
                        ])
                    ])
                ])
            ]),
            EOF(196, 196, [EOI(196, 196)])]
    }
}
