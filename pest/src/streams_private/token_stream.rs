use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use futures::stream::Stream;

use super::expandable_stream::ExpandableStream;
use super::expanded_stream as es;
use super::tail_stream as ts;
use super::token_data_future as tdf;
use super::super::error::Error;
use super::super::tokens::Token;

/// A `trait` that defines common methods on `Token` `Stream`s.
pub trait TokenStream<Rule: Copy + Debug + Eq> {
    /// Expands a matching `Token` pair to a `TokenDataFuture`, containing the `TokenData` crated
    /// from the pair, and an `ExpandedStream`, containing all the `Token`s between the matching
    /// pair.
    ///
    /// A matching `Token` pair is formed by a `Token::Start` followed by a `Token::End` with the
    /// same `Rule` with the condition that all `Token`s between them can form such pairs as well.
    /// This is similar to the
    /// [brace matching problem](https://en.wikipedia.org/wiki/Brace_matching) in editors.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate futures;
    /// # extern crate pest;
    /// # use futures::future::Future;
    /// # use futures::stream::Stream;
    /// # use pest::{state, StringInput};
    /// # use pest::streams::TokenStream;
    /// # use pest::tokens::Token;
    /// # fn main() {
    /// # #[allow(non_camel_case_types)]
    /// #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a,
    ///     b
    /// }
    ///
    /// let input = StringInput::new("");
    /// let (mut state, stream) = state(&input);
    ///
    /// state.send(Token::Start { rule: Rule::a, pos: 0 });
    /// state.send(Token::Start { rule: Rule::b, pos: 1 });
    /// state.send(Token::End   { rule: Rule::b, pos: 2 });
    /// state.send(Token::End   { rule: Rule::a, pos: 3 });
    ///
    /// stream.expand(Rule::a, |data, stream| {
    ///     let data = data.wait().unwrap();
    ///
    ///     assert_eq!(data.rule, Rule::a);
    ///     assert_eq!(data.start, 0);
    ///     assert_eq!(data.end, 3);
    ///
    ///     let inner = stream.collect().wait().unwrap();
    ///
    ///     assert_eq!(inner, vec![
    ///         Token::Start { rule: Rule::b, pos: 1 },
    ///         Token::End   { rule: Rule::b, pos: 2 }
    ///     ]);
    /// });
    /// # }
    /// ```
    fn expand<F, T>(self, rule: Rule, f: F) -> (T, ts::TailStream<Rule, Self>)
        where F: FnOnce(tdf::TokenDataFuture<Rule, Self>, es::ExpandedStream<Rule, Self>) -> T,
              Self: Stream<Item=Token<Rule>, Error=Error<Rule>> + Sized {

        let stream = Rc::new(RefCell::new(ExpandableStream::new(self, rule)));

        let token_data_future = tdf::new(stream.clone());
        let expanded_stream = es::new(stream.clone());
        let tail_stream = ts::new(stream.clone());

        (f(token_data_future, expanded_stream), tail_stream)
    }
}

#[cfg(test)]
mod tests {
    use std::thread;
    use std::time::Duration;

    use futures::future::Future;
    use futures::stream::Stream;
    use futures::sync::mpsc::unbounded;

    use super::TokenStream;
    use super::super::parser_stream;
    use super::super::super::error::Error;
    use super::super::super::tokens::{Token, TokenData};


    #[allow(non_camel_case_types)]
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    enum Rule {
        a,
        b,
        c
    }

    #[test]
    fn expand_sleep() {
        let r = {
            let (s, r) = unbounded();

            thread::spawn(move || {
                thread::sleep(Duration::from_millis(10));

                s.send(Ok(Token::Start { rule: Rule::a, pos: 0 })).unwrap();
                s.send(Ok(Token::Start { rule: Rule::b, pos: 1 })).unwrap();
                s.send(Ok(Token::End   { rule: Rule::b, pos: 2 })).unwrap();
                s.send(Ok(Token::End   { rule: Rule::a, pos: 3 })).unwrap();
                s.send(Ok(Token::Start { rule: Rule::c, pos: 4 })).unwrap();
                s.send(Ok(Token::End   { rule: Rule::c, pos: 5 })).unwrap();
            });

            r
        };

        let stream = parser_stream::new(r);

        let (result, stream) = stream.expand(Rule::a, |data, stream| {
            assert_eq!(data.wait().unwrap(), TokenData { rule: Rule::a, start: 0, end: 3 });

            let (result, _) = stream.expand(Rule::b, |data, _| {
                assert_eq!(data.wait().unwrap(), TokenData { rule: Rule::b, start: 1, end: 2 });

                2
            });

            3 + result
        });

        assert_eq!(result, 5);

        stream.expand(Rule::c, |data, _| {
            assert_eq!(data.wait().unwrap(), TokenData { rule: Rule::c, start: 4, end: 5 });
        });
    }

    #[test]
    fn expand_tail() {
        let r = {
            let (s, r) = unbounded();

            s.send(Ok(Token::Start { rule: Rule::a, pos: 0 })).unwrap();
            s.send(Ok(Token::Start { rule: Rule::b, pos: 1 })).unwrap();
            s.send(Ok(Token::End   { rule: Rule::b, pos: 2 })).unwrap();
            s.send(Ok(Token::End   { rule: Rule::a, pos: 3 })).unwrap();
            s.send(Ok(Token::Start { rule: Rule::c, pos: 4 })).unwrap();
            s.send(Ok(Token::End   { rule: Rule::c, pos: 5 })).unwrap();

            r
        };

        let stream = parser_stream::new(r);

        let (_, stream) = stream.expand(Rule::a, |_, _| {});

        assert_eq!(stream.collect().wait().unwrap(), vec![
            Token::Start { rule: Rule::c, pos: 4 },
            Token::End   { rule: Rule::c, pos: 5 }
        ]);
    }

    #[test]
    fn expand_streams() {
        let r = {
            let (s, r) = unbounded();

            s.send(Ok(Token::Start { rule: Rule::a, pos: 0 })).unwrap();
            s.send(Ok(Token::Start { rule: Rule::a, pos: 1 })).unwrap();
            s.send(Ok(Token::End   { rule: Rule::a, pos: 2 })).unwrap();
            s.send(Ok(Token::End   { rule: Rule::a, pos: 3 })).unwrap();
            s.send(Ok(Token::Start { rule: Rule::c, pos: 4 })).unwrap();
            s.send(Ok(Token::End   { rule: Rule::c, pos: 5 })).unwrap();

            r
        };

        let stream = parser_stream::new(r);

        let (_, stream) = stream.expand(Rule::a, |_, stream| {
            assert_eq!(stream.collect().wait().unwrap(), vec![
                Token::Start { rule: Rule::a, pos: 1 },
                Token::End   { rule: Rule::a, pos: 2 }
            ]);
        });

        stream.expand(Rule::c, |_, stream| {
            assert_eq!(stream.collect().wait().unwrap(), vec![]);
        });
    }

    #[test]
    #[should_panic(expected = "expected Token::Start { rule: b, .. }, \
                               but found Start { rule: a, pos: 0 } instead")]
    fn expand_wrong_start_future() {
        let r = {
            let (s, r) = unbounded();

            s.send(Ok(Token::Start { rule: Rule::a, pos: 0 })).unwrap();
            s.send(Ok(Token::End   { rule: Rule::a, pos: 1 })).unwrap();

            r
        };

        let stream = parser_stream::new(r);

        stream.expand(Rule::b, |data, _| {
            data.wait().unwrap();
        });
    }

    #[test]
    #[should_panic(expected = "expected Token::Start { rule: a, .. }, \
                               but found nothing")]
    fn expand_no_start_future() {
        let r = {
            let (_, r) = unbounded();

            r
        };

        let stream = parser_stream::new(r);

        stream.expand(Rule::a, |data, _| {
            data.wait().unwrap();
        });
    }

    #[test]
    #[should_panic(expected = "expected Token::End { rule: a, .. }, \
                               but found nothing")]
    fn expand_no_end_future() {
        let r = {
            let (s, r) = unbounded();

            s.send(Ok(Token::Start { rule: Rule::a, pos: 0 })).unwrap();

            r
        };

        let stream = parser_stream::new(r);

        stream.expand(Rule::a, |data, _| {
            data.wait().unwrap();
        });
    }

    #[test]
    #[should_panic(expected = "expected Token::Start { rule: b, .. }, \
                               but found Start { rule: a, pos: 0 } instead")]
    fn expand_wrong_start_expanded_stream() {
        let r = {
            let (s, r) = unbounded();

            s.send(Ok(Token::Start { rule: Rule::a, pos: 0 })).unwrap();
            s.send(Ok(Token::End   { rule: Rule::a, pos: 1 })).unwrap();

            r
        };

        let stream = parser_stream::new(r);

        stream.expand(Rule::b, |_, stream| {
            stream.collect().wait().unwrap();
        });
    }

    #[test]
    #[should_panic(expected = "expected Token::Start { rule: a, .. }, \
                               but found nothing")]
    fn expand_no_start_expanded_stream() {
        let r = {
            let (_, r) = unbounded();

            r
        };

        let stream = parser_stream::new(r);

        stream.expand(Rule::a, |_, stream| {
            stream.collect().wait().unwrap();
        });
    }

    #[test]
    #[should_panic(expected = "expected Token::End { rule: a, .. }, \
                               but found nothing")]
    fn expand_no_end_expanded_stream() {
        let r = {
            let (s, r) = unbounded();

            s.send(Ok(Token::Start { rule: Rule::a, pos: 0 })).unwrap();

            r
        };

        let stream = parser_stream::new(r);

        stream.expand(Rule::a, |_, stream| {
            stream.collect().wait().unwrap();
        });
    }

    #[test]
    #[should_panic(expected = "expected Token::Start { rule: b, .. }, \
                               but found Start { rule: a, pos: 0 } instead")]
    fn expand_wrong_start_tail_stream() {
        let r = {
            let (s, r) = unbounded();

            s.send(Ok(Token::Start { rule: Rule::a, pos: 0 })).unwrap();
            s.send(Ok(Token::End   { rule: Rule::a, pos: 1 })).unwrap();

            r
        };

        let stream = parser_stream::new(r);

        let (_, stream) = stream.expand(Rule::b, |_, _| {});

        stream.collect().wait().unwrap();
    }

    #[test]
    #[should_panic(expected = "expected Token::Start { rule: a, .. }, \
                               but found nothing")]
    fn expand_no_start_tail_stream() {
        let r = {
            let (_, r) = unbounded();

            r
        };

        let stream = parser_stream::new(r);

        let (_, stream) = stream.expand(Rule::a, |_, _| {});

        stream.collect().wait().unwrap();
    }

    #[test]
    #[should_panic(expected = "expected Token::End { rule: a, .. }, \
                               but found nothing")]
    fn expand_no_end_tail_stream() {
        let r = {
            let (s, r) = unbounded();

            s.send(Ok(Token::Start { rule: Rule::a, pos: 0 })).unwrap();

            r
        };

        let stream = parser_stream::new(r);

        let (_, stream) = stream.expand(Rule::a, |_, _| {});

        stream.collect().wait().unwrap();
    }

    #[test]
    fn expand_incomplete_future() {
        let (s, r) = unbounded();

        s.send(Ok(Token::Start { rule: Rule::a, pos: 0 })).unwrap();
        s.send(Ok(Token::Start { rule: Rule::b, pos: 1 })).unwrap();
        s.send(Ok(Token::End { rule: Rule::b, pos: 2 })).unwrap();

        let stream = parser_stream::new(r);

        stream.expand(Rule::a, |_, stream| {
            assert_eq!(stream.take(2).collect().wait().unwrap(), vec![
                Token::Start { rule: Rule::b, pos: 1 },
                Token::End   { rule: Rule::b, pos: 2 }
            ]);
        });
    }

    #[test]
    fn expand_future_first() {
        let r = {
            let (s, r) = unbounded();

            s.send(Ok(Token::Start { rule: Rule::a, pos: 0 })).unwrap();
            s.send(Ok(Token::Start { rule: Rule::a, pos: 1 })).unwrap();
            s.send(Ok(Token::End   { rule: Rule::a, pos: 2 })).unwrap();
            s.send(Ok(Token::End   { rule: Rule::a, pos: 3 })).unwrap();

            r
        };

        let stream = parser_stream::new(r);

        stream.expand(Rule::a, |data, stream| {
            assert_eq!(data.wait().unwrap(), TokenData { rule: Rule::a, start: 0, end: 3 });
            assert_eq!(stream.collect().wait().unwrap(), vec![
                Token::Start { rule: Rule::a, pos: 1 },
                Token::End   { rule: Rule::a, pos: 2 }
            ]);
        });
    }

    #[test]
    fn expand_tail_first() {
        let r = {
            let (s, r) = unbounded();

            s.send(Ok(Token::Start { rule: Rule::a, pos: 0 })).unwrap();
            s.send(Ok(Token::Start { rule: Rule::a, pos: 1 })).unwrap();
            s.send(Ok(Token::End   { rule: Rule::a, pos: 2 })).unwrap();
            s.send(Ok(Token::End   { rule: Rule::a, pos: 3 })).unwrap();

            r
        };

        let stream = parser_stream::new(r);

        let ((data, expanded_stream), stream) = stream.expand(Rule::a, |data, stream| {
            (data, stream)
        });

        assert_eq!(stream.collect().wait().unwrap(), vec![]);

        assert_eq!(data.wait().unwrap(), TokenData { rule: Rule::a, start: 0, end: 3 });
        assert_eq!(expanded_stream.collect().wait().unwrap(), vec![
            Token::Start { rule: Rule::a, pos: 1 },
            Token::End   { rule: Rule::a, pos: 2 }
        ]);
    }

    #[test]
    fn expand_error() {
        let r = {
            let (s, r) = unbounded();

            s.send(Err(Error::CustomErrorPos("e".to_owned(), 2))).unwrap();

            r
        };

        let stream = parser_stream::new(r);

        let (_, stream) = stream.expand(Rule::a, |data, stream| {
//            assert_eq!(data.wait(), Err(Error::CustomErrorPos("e".to_owned(), 2)));
//            assert_eq!(stream.collect().wait(), Err(Error::CustomErrorPos("e".to_owned(), 2)));
        });


        assert_eq!(stream.collect().wait(), Err(Error::CustomErrorPos("e".to_owned(), 2)));
    }
}
