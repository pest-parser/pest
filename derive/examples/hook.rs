use pest::StateParser;

mod parser {
    use pest::{Span, StateCheckpoint};
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "../examples/hook.pest"]
    #[custom_state(crate::parser::CustomState)]
    pub struct Parser;

    pub struct CustomState {
        pub max_int_visited: usize,
        is_snapshot_cleared: bool,
    }

    impl StateCheckpoint for CustomState {
        fn snapshot(&mut self) {}
        fn clear_snapshot(&mut self) {
            self.is_snapshot_cleared = true;
        }
        fn restore(&mut self) {}
    }

    impl CustomState {
        pub fn create() -> Self {
            Self {
                max_int_visited: 0,
                is_snapshot_cleared: true,
            }
        }
    }

    impl Parser {
        #[allow(non_snake_case)]
        fn hook__HOOK_INT<'a>(state: &mut CustomState, span: Span<'a>) -> bool {
            if !state.is_snapshot_cleared {
                println!("this state cannot operate with snapshot, please check your grammar to avoid hook in unexpected location",);
                return false;
            }
            let val: usize = span.as_str().parse().unwrap();
            println!("hook called with val={}", val);
            if val >= state.max_int_visited {
                state.is_snapshot_cleared = false;
                state.max_int_visited = val;
                true
            } else {
                false
            }
        }
    }
}

fn main() {
    // parser::Rule::ints parses a non-decreasing sequence of integers.

    println!("parser::Rule::ints");

    // should parse successfully.
    let (state, _) = parser::Parser::parse_with_state(
        parser::Rule::ints,
        "1\n2\n3\n4\n",
        parser::CustomState::create(),
    )
    .unwrap();
    assert_eq!(state.max_int_visited, 4);

    println!("parser::Rule::ints");

    // custom state hook will reject this case
    assert!(parser::Parser::parse_with_state(
        parser::Rule::ints,
        "1\n2\n2\n0\n",
        parser::CustomState::create()
    )
    .is_err());

    // parser::Rule::ints2 passes a non-decreasing sequence of integers to HOOK_INT, while allowing
    // other numbers in the sequence.

    println!("parser::Rule::ints2");

    // should parse successfully.
    let (state, _) = parser::Parser::parse_with_state(
        parser::Rule::ints2,
        "1 yes\n2 yes\n3 yes\n4 yes\n",
        parser::CustomState::create(),
    )
    .unwrap();
    assert_eq!(state.max_int_visited, 4);

    println!("parser::Rule::ints2");

    // custom state hook will still be called with val = 3, but it will be restored.
    assert!(parser::Parser::parse_with_state(
        parser::Rule::ints2,
        "1 yes\n2 yes\n3 no\n4 yes\n",
        parser::CustomState::create()
    )
    .is_err());
}
