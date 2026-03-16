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

    // workaround for Windows
    // TODO: use `normpath` or a different workaround on Windows?
    let pest_ref = pest.to_string_lossy();
    let normalized_path = pest_ref
        .strip_prefix(r#"\\?\"#)
        .unwrap_or_else(|| &pest_ref);
    let pest = Path::new(&normalized_path);

    let derived = {
        let path = pest.to_string_lossy();
        let pest = quote! {
            #[grammar = #path]
            pub struct PestParser;
        };
        derive_parser(pest, false)
    };

    let rs = Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../meta/src/grammar.rs"
    ));
    let mut file = File::create(rs).unwrap();

    writeln!(file, "pub struct PestParser;\n{}", derived).unwrap();
}
