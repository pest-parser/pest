#!/usr/bin/env bash

cargo install ucd-generate || true

rm -r ./target/ucd/
mkdir ./target/ucd/
cd ./target/ucd/
curl -LO https://www.unicode.org/Public/zipped/latest/UCD.zip
unzip UCD.zip
cd ../../

ucd-generate property-bool    --trie-set ./target/ucd/ > ./pest/src/unicode/binary.rs
ucd-generate general-category --trie-set ./target/ucd/ > ./pest/src/unicode/category.rs
