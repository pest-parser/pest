#![recursion_limit = "80"]
#![feature(test)]

extern crate test;

#[macro_use]
extern crate pest;

use std::fs::File;
use std::io::Read;

use test::Bencher;

use pest::Parser;
use pest::Input;
use pest::StringInput;

impl_rdp! {
    grammar! {
        object = { ["{"] ~ pair ~ ([","] ~ pair)* ~ ["}"] | ["{"] ~ ["}"] }
        pair = { string ~ [":"] ~ value }

        array = { ["["] ~ value ~ ([","] ~ value)* ~ ["]"] | ["["] ~ ["]"] }

        value = { string | number | object | array | ["true"] | ["false"] | ["null"] }

        string = { ["\""] ~ (escape | !(["\""] | ["\\"]) ~ any)* ~ ["\""] }
        escape = { ["\\"] ~ (["\""] | ["\\"] | ["/"] | ["b"] | ["f"] | ["n"] | ["r"] | ["t"] | unicode) }
        unicode = { ["u"] ~ hex ~ hex ~ hex ~ hex }
        hex = { ['0'..'9'] | ['a'..'f'] | ['A'..'F'] }

        number = { ["-"]? ~ int ~ ["."] ~ ['0'..'9']+ ~ exp? | ["-"]? ~ int ~ exp | ["-"]? ~ int }
        int = { ["0"] | ['1'..'9'] ~ ['0'..'9']* }
        exp = { (["E"] | ["e"]) ~ (["+"] | ["-"])? ~ int }

        ws = _{ [" "] | ["\t"] | ["\r"] | ["\n"] }
    }
}

#[bench]
fn data(b: &mut Bencher) {
    let mut file = File::open("benches/data.json").unwrap();
    let mut data = String::new();

    file.read_to_string(&mut data).unwrap();

    b.iter(|| {
        let mut parser = Rdp::new(Box::new(StringInput::new(&data)));

        parser.value();
    });
}
