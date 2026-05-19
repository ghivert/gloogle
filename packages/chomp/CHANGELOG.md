# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog],
and this project adheres to [Semantic Versioning].

## [Unreleased]

## [0.4.1] - 2024-12-31

### Fixed

- Updated `gleam_stdlib` to version `0.51.0`.
- Added `gleam_regexp` to replace the removed `gleam/regex` module.
- Replaced `string.drop_left` with `string.drop_start`.
- Replaced `string.drop_right` with `string.drop_end`.

## [0.4.0] - 2024-09-21

### Added

- A new `map_error` parser. See its description in the docs for more information.

## [0.3.1] - 2024-09-15

### Fixed

- Fixed `comment` lexer to work with comments at the end of the input

## [0.3.0] - 2024-09-07

### Added

- `Skip` lexer match can transition the lexer into a new mode.

## [0.2.1] - 2024-08-21

### Fixed

- (@TheOnlyTails) Bug with new pratt parsers.

## [0.2.0] - 2024-08-21

### Added

- (@TheOnlyTails) `_custom` variants to `prefix`, `infix`, and `postfix` pratt parsers, as well as a new `operator_custom` function. `infix_left` and `infix_right` have been merged into a single `infix` function that accepts a parameter of type `Precedence`.

## [0.1.0] - 2024-05-16

- Initial release

<!-- Links -->
[keep a changelog]: https://keepachangelog.com/en/1.1.0/
[semantic versioning]: https://semver.org/spec/v2.0.0.html

<!-- Versions -->
[unreleased]: https://github.com/MystPi/chomp/compare/v0.4.1...HEAD
[0.1.0]: https://github.com/MystPi/chomp/releases/v0.1.0
[0.2.0]: https://github.com/MystPi/chomp/releases/v0.2.0
[0.2.1]: https://github.com/MystPi/chomp/releases/v0.2.1
[0.3.0]: https://github.com/MystPi/chomp/releases/v0.3.0
[0.3.1]: https://github.com/MystPi/chomp/releases/v0.3.1
[0.4.0]: https://github.com/MystPi/chomp/releases/v0.4.0
[0.4.1]: https://github.com/MystPi/chomp/releases/v0.4.1