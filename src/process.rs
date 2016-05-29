// pest. Smart PEGs in Rust
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[macro_export]
macro_rules! process {
    // handle patterns; increment $index automatically
    // _ : rule
    ( @pattern $slf:ident $index:ident ($block:expr) _ : $typ:ident ) => {
        {
            if $slf.queue()[$index].rule == Rule::$typ {
                let $index = $index + 1;

                Some(($block, $index))
            } else {
                None
            }
        }
    };
    ( @pattern $slf:ident $index:ident ($block:expr) _ : $typ:ident, $( $tail:tt )* ) => {
        {
            if $slf.queue()[$index].rule == Rule::$typ {
                let $index = $index + 1;

                process!(@pattern $slf $index ($block) $( $tail )*)
            } else {
                None
            }
        }
    };
    // _
    ( @pattern $slf:ident $index:ident ($block:expr) _ ) => {
        {
            let $index = $index + 1;

            Some(($block, $index))
        }
    };
    ( @pattern $slf:ident $index:ident ($block:expr) _, $( $tail:tt )* ) => {
        {
            let $index = $index + 1;

            process!(@pattern $slf $index ($block) $( $tail )*)
        }
    };
    // &name : rule
    ( @pattern $slf:ident $index:ident ($block:expr) &$head:ident : $typ:ident ) => {
        {
            if $slf.queue()[$index].rule == Rule::$typ {
                let $head = $slf.queue()[$index];
                let $head = $slf.slice_input($head.start, $head.end);

                let $index = $index + 1;

                Some(($block, $index))
            } else {
                None
            }
        }
    };
    ( @pattern $slf:ident $index:ident ($block:expr) &$head:ident : $typ:ident, $( $tail:tt )* ) => {
        {
            if $slf.queue()[$index].rule == Rule::$typ {
                let $head = $slf.queue()[$index];
                let $head = $slf.slice_input($head.start, $head.end);

                let $index = $index + 1;

                process!(@pattern $slf $index ($block) $( $tail )*)
            } else {
                None
            }
        }
    };
    // &name
    ( @pattern $slf:ident $index:ident ($block:expr) &$head:ident ) => {
        {
            let $head = $slf.queue()[$index];
            let $head = $slf.slice_input($head.start, $head.end);

            let $index = $index + 1;

            Some(($block, $index))
        }
    };
    ( @pattern $slf:ident $index:ident ($block:expr) &$head:ident, $( $tail:tt )* ) => {
        {
            let $head = $slf.queue()[$index];
            let $head = $slf.slice_input($head.start, $head.end);

            let $index = $index + 1;

            process!(@pattern $slf $index ($block) $( $tail )*)
        }
    };
    // @recurse
    ( @pattern $slf:ident $index:ident ($block:expr) @$head:ident ) => {
        {
            let ($head, index) = $slf.next($index);
            let $index = index;

            Some(($block, $index))
        }
    };
    ( @pattern $slf:ident $index:ident ($block:expr) @$head:ident, $( $tail:tt )* ) => {
        {
            let ($head, index) = $slf.next($index);
            let $index = index;

            process!(@pattern $slf $index ($block) $( $tail )*)
        }
    };
    // name : rule
    ( @pattern $slf:ident $index:ident ($block:expr) $head:ident : $typ:ident ) => {
        {
            if $slf.queue()[$index].rule == Rule::$typ {
                let $head = $slf.queue()[$index];

                let $index = $index + 1;

                Some(($block, $index))
            } else {
                None
            }
        }
    };
    ( @pattern $slf:ident $index:ident ($block:expr) $head:ident : $typ:ident, $( $tail:tt )* ) => {
        {
            if $slf.queue()[$index].rule == Rule::$typ {
                let $head = $slf.queue()[$index];

                let $index = $index + 1;

                process!(@pattern $slf $index ($block) $( $tail )*)
            } else {
                None
            }
        }
    };
    // name
    ( @pattern $slf:ident $index:ident ($block:expr) $head:ident ) => {
        {
            let $head = $slf.queue()[$index];

            let $index = $index + 1;

            Some(($block, $index))
        }
    };
    ( @pattern $slf:ident $index:ident ($block:expr) $head:ident, $( $tail:tt )* ) => {
        {
            let $head = $slf.queue()[$index];

            let $index = $index + 1;

            process!(@pattern $slf $index ($block) $( $tail )*)
        }
    };

    // handle branches; panic if no branch matches
    ( @branches $slf:ident $index:ident ( $( $pattern:tt )* ) => $block:expr) => {
        if let Some(result) = process!(@pattern $slf $index ($block) $( $pattern )*) {
            result
        } else {
            panic!("no rules matched")
        }
    };
    ( @branches $slf:ident $index:ident ( $( $pattern:tt )* ) => $block:expr, $( $tail:tt )* ) => {
        if let Some(result) = process!(@pattern $slf $index ($block) $( $pattern )*) {
            result
        } else {
            process!(@branches $slf $index $( $tail )*)
        }
    };

    ( (&$slf:ident) -> $typ:ty { $( $ts:tt )* } ) => {
        fn next(&$slf, index: usize) -> ($typ, usize) {
            process!(@branches $slf index $( $ts )*)
        }

        pub fn process(&self) -> $typ {
            self.next(0).0
        }
    }
}
