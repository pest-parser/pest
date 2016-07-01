// pest. Elegant, efficient grammars
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![recursion_limit = "120"]

extern crate clap;
#[macro_use]
extern crate pest;

use std::fs::File;
use std::io::Read;

use clap::{Arg, App, SubCommand};

use pest::prelude::*;

mod meta;

use meta::parser::Rdp;

fn main() {
    let matches = App::new("pest")
                          .about("Grammar multi-tool")
                          .version(env!("CARGO_PKG_VERSION"))
                          .subcommand(SubCommand::with_name("check")
                                      .about("checks grammar file for common mistakes")
                                      .arg(Arg::with_name("FILE")
                                          .required(true)
                                          .help("file to check")))
                          .get_matches();

    if let Some(matches) = matches.subcommand_matches("check") {
        let file_name = matches.value_of("FILE").unwrap();

        let mut file = File::open(file_name).unwrap();
        let mut data = String::new();

        file.read_to_string(&mut data).unwrap();

        let mut parser = Rdp::new(StringInput::new(&data));

        if !parser.file() {
            let (expected, pos) = parser.expected();
            panic!("{:?} at {:?}", expected, parser.input().line_col(pos));
        }
    }
}
