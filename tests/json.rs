#[macro_use]
extern crate pest;

use pest::Parser;
use pest::Input;
use pest::StringInput;

impl_rdp! {
    grammar! {
        number = { ["-"]? ~ int ~ ["."] ~ ['0'..'9']+ ~ exp? | ["-"]? ~ int ~ exp | ["-"]? ~ int }
        int = { ["0"] | ['1'..'9'] ~ ['0'..'9']* }
        exp = { (["E"] | ["e"]) ~ (["+"] | ["-"])? ~ int }

        ws = _{ [" "] | ["\t"] | ["\r"] | ["\n"] }
    }
}

#[test]
fn int_zero() {
    let mut parser = Rdp::new(Box::new(StringInput::new("0")));

    assert!(parser.int());
    assert!(parser.end());

    let queue = vec![
        Rules::int(0, 1)
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn int_long() {
    let mut parser = Rdp::new(Box::new(StringInput::new("234239923610")));

    assert!(parser.int());
    assert!(parser.end());

    let queue = vec![
        Rules::int(0, 12)
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn exp() {
    let mut parser = Rdp::new(Box::new(StringInput::new("e10")));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Rules::exp(0, 3),
        Rules::int(1, 2)
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn exp_signed() {
    let mut parser = Rdp::new(Box::new(StringInput::new("E-0")));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Rules::exp(0, 3),
        Rules::int(2, 1)
    ];

    assert!(parser.queue().iter().eq(&queue));
}
