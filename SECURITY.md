# Security Policy

## Supported Versions

Only the most recent minor version is supported.

| Version | Supported          |
| ------- | ------------------ |
| 2.7.x   | :white_check_mark: |
| < 2.7.x | :x:                |


## Reporting a Vulnerability

Please use the [GitHub private reporting functionality](https://github.com/pest-parser/pest/security/advisories/new)
to submit potential security bug reports. If the bug report is reproduced and valid, we'll then:

- Prepare a fix and regression tests.
- Make a patch release for the most recent release.
- Submit an advisory to [rustsec/advisory-db](https://github.com/RustSec/advisory-db).
- Refer to the advisory in the release notes.

If you're *looking* for security bugs, [this crate is set up for
`cargo fuzz`](https://github.com/pest-parser/pest/blob/master/FUZZING.md) but would benefit from more runtime, targets and corpora.
