#[macro_use]
extern crate quote;
extern crate pest_generator;

use pest_generator::derive_parser;
use std::{fs::File, io::prelude::*, path::Path};

fn main() {
    let pest = Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../meta/src/grammar.pest"
    ));
    let rs = Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../meta/src/grammar.rs"
    ));

    let derived = {
        let path = pest.to_string_lossy();
        let pest = quote! {
            #[grammar = #path]
            pub struct PestParser;
        };
        derive_parser(pest, false)
    };

    let mut file = File::create(rs).unwrap();

    writeln!(file, "pub struct PestParser;\n{}", derived,).unwrap();
}
