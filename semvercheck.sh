#!/usr/bin/env bash
export CURRENT_GIT_SHA=`git rev-parse HEAD`
cargo clean
cargo install cargo-semver-checks || true
export RUSTDOC_LATE_FLAGS="--document-private-items -Zunstable-options --output-format json"
cargo build --package pest_bootstrap
cargo run --package pest_bootstrap

# current
for crate in "pest_derive" "pest_generator" "pest_grammars" "pest_meta" "pest" "pest_vm" "pest_debugger"; do
    cargo +nightly-2026-06-20 rustdoc -p $crate -- $RUSTDOC_LATE_FLAGS
    mv target/doc/$crate.json /tmp/current-$crate.json
done

mv Cargo.lock Cargo.lock.current
# the 2.5.7 release
export BASELINE_GIT_SHA="f668fcc865965b0eeae6f19ee907bc4c9ce17967"
# baseline
git fetch origin
git checkout "$BASELINE_GIT_SHA"
cargo clean
perl -pi -e 's/^pest_generator = "[^"]+"/pest_generator = "= 2.5.7"/' bootstrap/Cargo.toml
cargo build --package pest_bootstrap
cargo run --package pest_bootstrap
for crate in "pest_derive" "pest_generator" "pest_grammars" "pest_meta" "pest" "pest_vm" "pest_debugger"; do
    cargo +nightly-2026-06-20 rustdoc -p $crate -- $RUSTDOC_LATE_FLAGS
    mv target/doc/$crate.json /tmp/baseline-$crate.json
    echo "Checking $crate"
    cargo semver-checks check-release --current /tmp/current-$crate.json --baseline /tmp/baseline-$crate.json
done
git checkout bootstrap/Cargo.toml
git checkout "$CURRENT_GIT_SHA"
