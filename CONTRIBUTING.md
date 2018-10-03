# Contributing to pest

## Getting to know the project

Before diving into the whys and hows, it's best one got started with the whats. The best place to learn about what pest does and what its limits are is the [book]. Feel free to try any of the examples in the [fiddle editor] as well.

[book]: https://pest-parser.github.io/book
[fiddle editor]: https://pest-parser.github.io/#editor

With that out of the way, let's go through *pest's* crate structure:

* `pest` - contains bare-bones parsing functionality and error types
* `derive` - automatically generates code that uses the above crate from a grammar file
  * `meta` - parses, validates, optimizes, and converts grammars to ASTs
  * `generator` - generates code from an AST
* `vm` - run ASTs on-the-fly and is used by the fiddle and debugger


## Where to start

It's always a good practice to start with something that drives you, but if you're not inspired at the moment, you can go for a [good-first-issue]. These are the kind of issues more tailored to people with less *pest* experience, but they are not necessarily easier; they can offer a fair challenge.

[good-first-issue]: https://github.com/pest-parser/pest/issues?q=is%3Aissue+is%3Aopen+label%3Agood-first-issue

## Mentoring

We're happy to mentor any issues as long as we have the time, but issues with a [mentored] tag should generally be considered when looking for ways to learn, grow, and get some honest feedback on your work.

[mentored]: https://github.com/pest-parser/pest/issues?q=is%3Aissue+is%3Aopen+label%3Amentored

## RFCs

For those of you looking for a more philosophical challenge, feel free to give [these] a try. A lot of the work ahead of us is hard and we need great thinkers to lay the foundation on which to build forward. Not for the faint of heart.

[these]: https://github.com/pest-parser/pest/issues?q=is%3Aissue+is%3Aopen+label%3Aneeds-rfc

## Website and book

Our [website] and [book] are in constant need of attention. While not as well organized, they should be more approachable to the general popultion.

[website]:https://github.com/pest-parser/site
[book]: https://github.com/pest-parser/book

## Gitter

Sometimes it's best to just say what you want. For that, there's our [Gitter] room. Leave feedback, help out, learn what people are up to, go off-topic for hours, or complain that compile times are terrible—seriously, please don't.

[Gitter]: https://gitter.im/pest-parser/pest

