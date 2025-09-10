#!/usr/bin/env bash
set -u

cargo install ucd-generate || true

rm -r ./target/ucd/
mkdir ./target/ucd/
cd ./target/ucd/
curl -LO https://www.unicode.org/Public/$UNICODE_VERSION/ucd/UCD.zip
unzip UCD.zip
cd ../../

ucd-generate property-bool    --trie-set ./target/ucd/ > ./pest/src/unicode/binary.rs
ucd-generate general-category --trie-set ./target/ucd/ > ./pest/src/unicode/category.rs
ucd-generate script           --trie-set ./target/ucd/ > ./pest/src/unicode/script.rs
