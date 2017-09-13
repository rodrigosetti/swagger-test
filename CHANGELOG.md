# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.7] - 2017-09-12
### Fixed
- `Host` header was being generated without port
- `curl` request output format was broken

## [0.2.6] - 2017-09-12
### Added
- Add "generated time" to reports

## [0.2.5] - 2017-09-12
### Added
- `report` command also generates (json) files with the report data
- HTML report operations summary are color coded depending on success or failure
### Fixed
- Updates to new version of *swagger2* package containing fix for `allOf`

## [0.2.4] - 2017-09-07
### Added
- Generate `User-Agent` header with `swagger-test/<version>`

## [0.2.3] - 2017-09-07
### Fixed
- Catch HTTP exceptions into report errors, avoiding them to halt the program
- Make path concatenation smarter (avoid duplicated `/`)

## [0.2.2] - 2017-09-07
### Changed
- Improves the look of HTML reports
### Fixed
- If scheme is undefined, try to figure out based on port

## [0.2.1] - 2017-09-04
### Fixed
- Fix bug in validation of empty bodies

## [0.2.0] - 2017-09-03
### Added
- `report` command: generates HTML reports of a set of schema

## [0.1.0] - 2017-09-01
### Added
- Initial version: `generate`, `validate`, and `request` commands
