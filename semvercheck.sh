#!/usr/bin/env bash
export CURRENT_GIT_SHA=`git rev-parse HEAD`
cargo clean
cargo install cargo-semver-checks || true
export RUSTDOC_LATE_FLAGS="--document-private-items -Zunstable-options --output-format json"
cargo build --package pest_bootstrap
cargo run --package pest_bootstrap

# current
for crate in "pest_derive" "pest_generator" "pest_grammars" "pest_meta" "pest" "pest_vm" "pest_debugger"; do
    cargo +nightly-2023-12-31 rustdoc -p $crate -- $RUSTDOC_LATE_FLAGS
    mv target/doc/$crate.json /tmp/current-$crate.json
done

# the 2.5.0 release
export BASELINE_GIT_SHA="8c602d832e625a0965701618626166e2ffbd94bb"
# baseline
git fetch origin
git checkout "$BASELINE_GIT_SHA"
cargo clean
cargo build --package pest_bootstrap
cargo run --package pest_bootstrap
for crate in "pest_derive" "pest_generator" "pest_grammars" "pest_meta" "pest" "pest_vm" "pest_debugger"; do
    cargo +nightly-2023-12-31 rustdoc -p $crate -- $RUSTDOC_LATE_FLAGS
    mv target/doc/$crate.json /tmp/baseline-$crate.json
    echo "Checking $crate"
    cargo semver-checks check-release --current /tmp/current-$crate.json --baseline /tmp/baseline-$crate.json
done
git checkout "$CURRENT_GIT_SHA"
