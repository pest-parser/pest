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
