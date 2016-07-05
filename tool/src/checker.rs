// pest. Elegant, efficient grammars
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use pest::prelude::*;

use meta::parser::{Rdp, Rule};

pub struct Checker<T> {
    parser: Rdp<T>
}

impl<'a, T: Input<'a>> Checker<T> {
    pub fn new(parser: Rdp<T>) -> Checker<T> {
        Checker {
            parser: parser
        }
    }

    pub fn check(&mut self) -> Result<(), String> {
        let result = self.parser.file();

        try!(self.check_parsing(result));

        Ok(())
    }

    fn check_parsing(&mut self, result: bool) -> Result<(), String> {
        let (expected, pos) = self.parser.expected();
        let (line, col) = self.parser.input().line_col(pos);

        let expected = expected.into_iter().filter(|rule| rule != &Rule::any).collect::<Vec<_>>();

        if result {
            return Ok(());
        }

        if expected.is_empty() {
            return Err(format!("Unexpected input at line {}, column {}.", line, col));
        }

        if expected.len() >= 3 {
            let first = &expected[..expected.len() - 2];
            let first = first.iter().map(|rule| format!("{:?}", rule))
                                    .collect::<Vec<_>>().join(", ");

            let len = expected.len();
            let last = format!(", {:?}, or {:?}", expected[len - 2], expected[len - 1]);

            return Err(format!("Expected {}{} at line {}, column {}.", first, last, line, col));
        } else if expected.len() == 2 {
            return Err(format!("Expected {:?} or {:?} at line {}, column {}.",
                               expected[0], expected[1], line, col));
        } else {
            return Err(format!("Expected {:?} at line {:?}, column {}.", expected[0], line, col));
        }
    }
}
