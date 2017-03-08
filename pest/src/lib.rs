// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate futures;

mod error;
mod inputs;
mod parser;
mod parser_state;
mod streams_private;
pub mod tokens;

/// A `mod` containing `Stream` implementations used in `Token` processing.
pub mod streams {
    pub use super::streams_private::{ExpandedStream, ParserStream, TokenDataFuture, TokenStream,
                                     TailStream};
}

pub use error::Error;
pub use inputs::Input;
pub use inputs::StringInput;
pub use parser::Parser;
pub use parser_state::{state, ParserState};
