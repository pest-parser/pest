// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

//! # pest grammars
//!
//! Contains a series of default grammars.

#![doc(html_root_url = "https://docs.rs/pest_grammars")]

extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod json {
    /// JSON parser.
    #[derive(Parser)]
    #[grammar = "grammars/json.pest"]
    pub struct JsonParser;
}

pub mod toml {
    /// TOML parser.
    #[derive(Parser)]
    #[grammar = "grammars/toml.pest"]
    pub struct TomlParser;
}
