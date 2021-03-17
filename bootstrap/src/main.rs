#[macro_use]
extern crate quote;
extern crate pest_generator;

use pest_generator::derive_parser;
use std::{fs::File, io::prelude::*, env, path::{Path, PathBuf}};

fn main() {
    let pest = Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../meta/src/grammar.pest"
    ));
    let rs = if should_bootstrap_in_src() {
        PathBuf::from(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../meta/src/grammar.rs"
        ))
    } else {
        // the path is passed via command-line arguments
        PathBuf::from(&env::args().nth(1).unwrap())
    };

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

#[cfg(feature = "bootstrap-in-src")]
fn should_bootstrap_in_src() -> bool { true }

#[cfg(not(feature = "bootstrap-in-src"))]
fn should_bootstrap_in_src() -> bool { false }