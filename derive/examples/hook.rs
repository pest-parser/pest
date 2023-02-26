mod parser {
    use pest::{Span, StateCheckpoint};
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "../examples/hook.pest"]
    #[custom_state(crate::parser::CustomState)]
    pub struct Parser;

    #[derive(Default)]
    pub struct CustomState {}

    impl StateCheckpoint for CustomState {
        fn snapshot(&mut self) {}
        fn clear_snapshot(&mut self) {}
        fn restore(&mut self) {
            unimplemented!(
                "this state cannot restore -- please check your grammar to avoid restoring"
            );
        }
    }

    impl Parser {
        fn hook__HOOK_INT<'a>(state: &mut CustomState, span: Span<'a>) -> bool {
            true
        }
    }
}

fn main() {}
