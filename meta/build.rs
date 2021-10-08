extern crate sha1;

use sha1::{Digest, Sha1};
use std::env;
use std::fs::{self, File};
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::process::Command;

fn display_digest(digest: &[u8]) -> String {
    digest.iter().map(|byte| format!("{:02x}", byte)).collect()
}

fn main() {
    println!("rerun-if-changed=src/grammar.pest");

    // Yes; build.rs is supposed to treat `src` as read-only; however:
    // We want to publish `grammar.rs` and not `grammar.pest`,
    // so putting it in `src` is the simplest way to do so.
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let grammar_pest_path = manifest_dir.join("src/grammar.pest");
    let grammar_rs_path = manifest_dir.join("src/grammar.rs");
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let hash_path = out_dir.join("pest_hash.sha1");

    // If `grammar.pest` exists (we're building from git sources)
    if grammar_pest_path.exists() {
        let mut sha = Sha1::default();

        let old_hash = File::open(&hash_path).ok().map(|mut file| {
            let mut s = String::new();
            file.read_to_string(&mut s).unwrap();
            s
        });
        let current_grammar = fs::read_to_string(grammar_pest_path).unwrap();
        sha.update(current_grammar.as_bytes());
        let current_hash = display_digest(&sha.finalize());

        // If `grammar.pest` has changed
        if !grammar_rs_path.exists()
            || old_hash.as_ref().map(|it| it.trim()) != Some(current_hash.trim())
        {
            println!("Bootstrapping `meta/src/grammar.rs`");

            let mut hash_file = File::create(hash_path).unwrap();
            writeln!(hash_file, "{}", current_hash).unwrap();

            // This "dynamic linking" is probably so fragile I don't even want to hear it
            let status = Command::new(manifest_dir.join("../target/debug/pest_bootstrap"))
                .spawn()
                .unwrap_or_else(|_| {
                    panic!(
                        "Bootstrap failed because no bootstrap executable was found. \
                         Please run `cargo build --package pest_bootstrap` or `cargo bootstrap` \
                         and then try again.",
                    )
                })
                .wait()
                .unwrap();
            if !status.success() {
                panic!("Bootstrap failed");
            }
        } else {
            println!("       Fresh `meta/src/grammar.rs`");
        }
    } else {
        assert!(
            grammar_rs_path.exists(),
            "package is broken; does not contain grammar.rs"
        );
    }
}
