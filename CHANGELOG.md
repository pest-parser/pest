# Change Log

## [v0.3.3](https://github.com/dragostis/pest/tree/v0.3.3) (2016-08-03)
[Full Changelog](https://github.com/dragostis/pest/compare/v0.3.2...v0.3.3)

**Implemented enhancements:**

- Case insensitive keywords [\#71](https://github.com/dragostis/pest/issues/71)

**Closed issues:**

- Cannot use multiple mut result in Matchers [\#75](https://github.com/dragostis/pest/issues/75)
- Add a queue\_debug on parser [\#72](https://github.com/dragostis/pest/issues/72)

**Merged pull requests:**

- Add queue\_with\_captures\(\), similar to queue\(\) except it contains the value [\#73](https://github.com/dragostis/pest/pull/73) ([Keats](https://github.com/Keats))

## [v0.3.2](https://github.com/dragostis/pest/tree/v0.3.2) (2016-06-28)
[Full Changelog](https://github.com/dragostis/pest/compare/v0.3.1...v0.3.2)

**Closed issues:**

- line\_col causing stack overflow when optimization turned off [\#67](https://github.com/dragostis/pest/issues/67)

## [v0.3.1](https://github.com/dragostis/pest/tree/v0.3.1) (2016-06-25)
[Full Changelog](https://github.com/dragostis/pest/compare/v0.3.0...v0.3.1)

**Implemented enhancements:**

- Add changelog [\#56](https://github.com/dragostis/pest/issues/56)

## [v0.3.0](https://github.com/dragostis/pest/tree/v0.3.0) (2016-06-22)
[Full Changelog](https://github.com/dragostis/pest/compare/v0.2.9...v0.3.0)

**Implemented enhancements:**

- Add new\(\) on Token [\#63](https://github.com/dragostis/pest/issues/63)
- Make all process methods visible and remove main constraint. [\#59](https://github.com/dragostis/pest/issues/59)
- Add input\(\) reference to Parser. [\#57](https://github.com/dragostis/pest/issues/57)

**Fixed bugs:**

- line\_col entered unreachable code when input has unicode [\#60](https://github.com/dragostis/pest/issues/60)

**Closed issues:**

- Error handling during process!-matchers [\#51](https://github.com/dragostis/pest/issues/51)

**Merged pull requests:**

- Adding some lifetime back [\#62](https://github.com/dragostis/pest/pull/62) ([sunng87](https://github.com/sunng87))
- v0.3.0 [\#61](https://github.com/dragostis/pest/pull/61) ([dragostis](https://github.com/dragostis))

## [v0.2.9](https://github.com/dragostis/pest/tree/v0.2.9) (2016-06-13)
[Full Changelog](https://github.com/dragostis/pest/compare/v0.2.8...v0.2.9)

## [v0.2.8](https://github.com/dragostis/pest/tree/v0.2.8) (2016-06-13)
[Full Changelog](https://github.com/dragostis/pest/compare/v0.2.7...v0.2.8)

**Merged pull requests:**

- Implement support for references within StringInput [\#55](https://github.com/dragostis/pest/pull/55) ([steffengy](https://github.com/steffengy))

## [v0.2.7](https://github.com/dragostis/pest/tree/v0.2.7) (2016-06-13)
[Full Changelog](https://github.com/dragostis/pest/compare/v0.2.6...v0.2.7)

**Implemented enhancements:**

- Change process to accept lifetimes. [\#53](https://github.com/dragostis/pest/issues/53)

**Fixed bugs:**

- Only leaf errors should be reported. [\#54](https://github.com/dragostis/pest/issues/54)

## [v0.2.6](https://github.com/dragostis/pest/tree/v0.2.6) (2016-06-12)
[Full Changelog](https://github.com/dragostis/pest/compare/v0.2.5...v0.2.6)

## [v0.2.5](https://github.com/dragostis/pest/tree/v0.2.5) (2016-06-11)
[Full Changelog](https://github.com/dragostis/pest/compare/v0.2.4...v0.2.5)

## [v0.2.4](https://github.com/dragostis/pest/tree/v0.2.4) (2016-06-11)
[Full Changelog](https://github.com/dragostis/pest/compare/v0.2.3...v0.2.4)

**Fixed bugs:**

- try! fails inside of process! matchers. [\#52](https://github.com/dragostis/pest/issues/52)

**Closed issues:**

- List parsing [\#49](https://github.com/dragostis/pest/issues/49)

## [v0.2.3](https://github.com/dragostis/pest/tree/v0.2.3) (2016-06-10)
[Full Changelog](https://github.com/dragostis/pest/compare/v0.2.2...v0.2.3)

**Implemented enhancements:**

- process! support mutability-specifier for bindings [\#50](https://github.com/dragostis/pest/issues/50)

**Merged pull requests:**

- Add a Gitter chat badge to README.md [\#48](https://github.com/dragostis/pest/pull/48) ([gitter-badger](https://github.com/gitter-badger))

## [v0.2.2](https://github.com/dragostis/pest/tree/v0.2.2) (2016-06-10)
[Full Changelog](https://github.com/dragostis/pest/compare/v0.2.1...v0.2.2)

**Implemented enhancements:**

- Add examples for process! [\#44](https://github.com/dragostis/pest/issues/44)

**Closed issues:**

- Right-associative operator [\#45](https://github.com/dragostis/pest/issues/45)

**Merged pull requests:**

- Added separate matchers. [\#47](https://github.com/dragostis/pest/pull/47) ([dragostis](https://github.com/dragostis))

## [v0.2.1](https://github.com/dragostis/pest/tree/v0.2.1) (2016-06-08)
[Full Changelog](https://github.com/dragostis/pest/compare/v0.2.0...v0.2.1)

## [v0.2.0](https://github.com/dragostis/pest/tree/v0.2.0) (2016-06-08)
[Full Changelog](https://github.com/dragostis/pest/compare/v0.1.0...v0.2.0)

**Implemented enhancements:**

- Mention recursion limit in the docs. [\#39](https://github.com/dragostis/pest/issues/39)
- Mention Rule in impl\_rdp! and Token docs. [\#38](https://github.com/dragostis/pest/issues/38)
- Add a prelude. [\#37](https://github.com/dragostis/pest/issues/37)
- Rethink empty string match \[""\] in examples. [\#35](https://github.com/dragostis/pest/issues/35)
- Rename exp to expression in all examples. [\#33](https://github.com/dragostis/pest/issues/33)
- Add calculator example. [\#31](https://github.com/dragostis/pest/issues/31)
- Add rule to Input for position to \(line, column\) conversion. [\#29](https://github.com/dragostis/pest/issues/29)
- Parser end\(\) should be replaced by eoi\(\). [\#27](https://github.com/dragostis/pest/issues/27)
- Implement macro to handle output. [\#23](https://github.com/dragostis/pest/issues/23)
- Remove Box from Rdp's Input. [\#22](https://github.com/dragostis/pest/issues/22)
- Add calculator example. [\#20](https://github.com/dragostis/pest/issues/20)
- Rename ws to whitespace. [\#19](https://github.com/dragostis/pest/issues/19)
- Add atomic rule. [\#17](https://github.com/dragostis/pest/issues/17)
- Comparison with other parsers [\#16](https://github.com/dragostis/pest/issues/16)
- Move Token outside of macro. [\#15](https://github.com/dragostis/pest/issues/15)
- Add benchmarks to README.md. [\#14](https://github.com/dragostis/pest/issues/14)
- Add Lua example. [\#13](https://github.com/dragostis/pest/issues/13)
- Improve documentation & host it. [\#12](https://github.com/dragostis/pest/issues/12)
- Add CI. [\#11](https://github.com/dragostis/pest/issues/11)
- Add error reporting. [\#8](https://github.com/dragostis/pest/issues/8)
- Add capturing. [\#5](https://github.com/dragostis/pest/issues/5)
- Improved documentation. Fixes \#12. [\#26](https://github.com/dragostis/pest/pull/26) ([dragostis](https://github.com/dragostis))

**Fixed bugs:**

- process will reach out of bounds instead of failing a pattern. [\#28](https://github.com/dragostis/pest/issues/28)
- ws should not skip\_ws. [\#18](https://github.com/dragostis/pest/issues/18)

**Closed issues:**

- Readme: Speedup should be slowdown? [\#42](https://github.com/dragostis/pest/issues/42)
- Rething empty string match \[\] [\#34](https://github.com/dragostis/pest/issues/34)

**Merged pull requests:**

- Fixed minor typo. [\#40](https://github.com/dragostis/pest/pull/40) ([alexandrusebastian](https://github.com/alexandrusebastian))
- Fix some docs + thoughts on the doc [\#32](https://github.com/dragostis/pest/pull/32) ([Keats](https://github.com/Keats))
- Token processing constructs. [\#24](https://github.com/dragostis/pest/pull/24) ([dragostis](https://github.com/dragostis))

## [v0.1.0](https://github.com/dragostis/pest/tree/v0.1.0) (2016-05-22)
[Full Changelog](https://github.com/dragostis/pest/compare/v0.0.1...v0.1.0)

**Implemented enhancements:**

- Consider inlining rules. [\#9](https://github.com/dragostis/pest/issues/9)
- Add optional rule. [\#4](https://github.com/dragostis/pest/issues/4)
- Add interrogation rules. [\#3](https://github.com/dragostis/pest/issues/3)
- Add repetition rules. [\#2](https://github.com/dragostis/pest/issues/2)
- Restyle grammar. [\#1](https://github.com/dragostis/pest/issues/1)

**Merged pull requests:**

- Performance [\#10](https://github.com/dragostis/pest/pull/10) ([dragostis](https://github.com/dragostis))

## [v0.0.1](https://github.com/dragostis/pest/tree/v0.0.1) (2016-04-24)


\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*