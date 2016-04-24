# pest. Smart PEGs in Rust

pest is a parser generator that works with
[PEGs](https://en.wikipedia.org/wiki/Parsing_expression_grammar).

It relies exclusively on macros to create an efficient parser at compile-time.

## Example
```rust
impl_rdp!(MyRdp);

impl MyRdp {
    grammar! {
        exp = { paren ~ exp | [""] }
        paren = { ["("] ~ exp ~ [")"] }
    }
}

let mut parser = MyRdp::new(Box::new(StringInput::new("(())((())())()")));

assert!(parser.exp());
assert!(parser.end());
```
