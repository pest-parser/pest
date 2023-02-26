mod parser {
    use pest::Span;
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "../examples/hook.pest"]
    #[custom_state(crate::parser::CustomState)]
    pub struct Parser;

    #[derive(Default)]
    pub struct CustomState {}

    impl Parser {
        fn hook__HOOK_INT<'a>(state: &mut CustomState, span: Span<'a>) -> bool {
            true
        }
    }
}

fn main() {}
