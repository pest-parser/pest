# Contributing to pest

## Getting to know the project

Before diving into the whys and hows, it's best one got started with the what's. The best place to learn about what pest does and what its limits are is the [book]. Feel free to try any of the examples in the [fiddle editor] as well.

[book]: https://pest.rs/book
[fiddle editor]: https://pest.rs/#editor

With that out of the way, let's go through *pest's* crate structure:

* `pest` - contains bare-bones parsing functionality and error types
* `derive` - automatically generates code that uses the above crate from a grammar file
  * `meta` - parses, validates, optimizes, and converts grammars to ASTs
  * `generator` - generates code from an AST
* `vm` - run ASTs on-the-fly and is used by the fiddle and debugger


## Where to start

It's always a good practice to start with something that drives you, but if you're not inspired at the moment, you can go for a [good-first-issue]. These are the kind of issues more tailored to people with less *pest* experience, but they are not necessarily easier; they can offer a fair challenge.

[good-first-issue]: https://github.com/pest-parser/pest/issues?q=is%3Aissue+is%3Aopen+label%3Agood-first-issue

## AI-assisted contributions

This policy applies to all contributions to *pest*—code, issues, reviews, comments, and documentation—whether you are a maintainer or an external contributor. AI tools are allowed and can be useful, but using them does not lower our standards or transfer responsibility for your contribution.

### Discuss the problem before opening a PR

Before opening a PR, open an issue describing the actual problem, relevant evidence or a reproduction where applicable, and a possible solution. Discuss the scope and approach there before submitting a patch. If an existing issue already covers the problem, join that discussion instead of opening a duplicate. Apply this workflow whether or not you use AI.

Link the issue in your PR and explain what the change does and what validation you performed. Do not post sensitive vulnerability details in a public issue; follow the process in our [security policy](SECURITY.md) instead.

### Take responsibility and protect reviewers' time

You are responsible for everything submitted under your name, regardless of the tools used to produce it. Understand every line of code you submit and be able to explain why each change is needed. “The AI wrote it” is not a justification. Verify claims, references, APIs, tests, and behavior against the repository, and report only validation you actually performed.

Carefully review AI output yourself before asking others to review it. Submit only a focused, useful patch you would reasonably ask a colleague to review; cheap code generation is not a reason to create unnecessary volume or scope. The same applies to review feedback: do not post AI-generated comments unless you have read and verified them and are prepared to explain them.

Submissions with verifiable quality problems may be closed without further discussion. Examples include padded text, nonexistent references, incorrect claims, or security reports that do not reproduce against the actual code. Decisions are based on concrete quality problems, not writing style or speculative AI detection. Maintainers are not obligated to debate such submissions, and repeated submissions may result in a repository block. If you believe a closure was mistaken, one comment with a concrete, verifiable correction is welcome; further discussion is at maintainers’ discretion.

Good-first-issue and onboarding tasks are partly intended to help you learn the codebase. Use AI to support that learning, not replace engaging with the code and the community.

### Disclose AI use

Every PR must state whether AI tools were used. If they were, name the tool or tools, briefly describe their contribution (for example, drafting code, documentation, or tests; debugging; or review), and say what you checked or tested. For AI-assisted issues, reviews, comments, and documentation outside a PR, include a brief disclosure in the contribution or its accompanying description. For code or documentation submitted in a PR, use the PR disclosure rather than marking every line or file. No prompts, transcripts, or sensitive information are required. If asked how a contribution was produced, answer honestly.

The following are templates, not factual declarations:

```text
No AI tools were used.
```

```text
AI tools used: [tool(s)]
Contribution: [how they assisted]
Human review and validation: [what I reviewed, checked, or tested]
```

## Contributing in the cloud

You can open a ready-to-go development workspace with [Gitpod](https://gitpod.io) by clicking [this link](https://gitpod.io/#https://github.com/pest-parser/pest).


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

## Gitter, Discord and GitHub Discussions

Sometimes it's best to just say what you want. For that, there's our [Gitter] room or [Discord] server. Leave feedback, help out, learn what people are up to, go off-topic for hours, or complain that compile times are terrible—seriously, please don't.

For more long-living threads and common questions, you can use [GitHub Discussions].

[Gitter]: https://gitter.im/pest-parser/pest

[Discord]: https://discord.gg/XEGACtWpT2

[GitHub Discussions]: https://github.com/pest-parser/pest/discussions