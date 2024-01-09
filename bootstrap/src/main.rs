#[macro_use]
extern crate quote;
extern crate pest_generator;

use pest_generator::derive_parser;
use std::{
    env,
    fs::File,
    io::prelude::*,
    path::{Path, PathBuf},
};

fn main() {
    let pest = Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../meta/src/grammar.pest"
    ));
    // Path on which we should write generated grammar file.
    // In case `not-bootstrap-in-src` is:
    // * OFF -> in `grammar.rs` next to `grammar.pest`
    // * ON  -> in `target/build/.../__pest_grammar.rs` directory as specified in `meta/build.rs`
    let rs: PathBuf = if should_bootstrap_in_src() {
        Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../meta/src/grammar.rs"
        ))
        .to_owned()
    } else {
        // the path is passed via command-line arguments
        let path = env::args().nth(1).expect("path to grammar.rs");
        PathBuf::from(path)
    };

    let derived = {
        let path = pest.to_string_lossy();
        let pest = quote! {
            #[grammar = #path]
            pub struct PestParser;
        };
        // Passing value to `derive_parser` as if there was a derive annotation
        // next to the struct above.
        derive_parser(pest, false)
    };

    let mut file = File::create(rs).unwrap();

    writeln!(file, "pub struct PestParser;\n{}", derived,).unwrap();
}

#[cfg(not(feature = "not-bootstrap-in-src"))]
fn should_bootstrap_in_src() -> bool {
    true
}

#[cfg(feature = "not-bootstrap-in-src")]
fn should_bootstrap_in_src() -> bool {
    false
}
