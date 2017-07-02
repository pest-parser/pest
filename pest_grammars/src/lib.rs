extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod json {
    use super::pest;

    #[derive(Parser)]
    #[grammar = "grammars/json.pest"]
    pub struct JsonParser;
}
