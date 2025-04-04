# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.6.4](https://github.com/knutwalker/sessionizer/compare/0.6.3...0.6.4) - 2025-04-03

### Changes

- The --config commands operate on the session root by default ([#62](https://github.com/knutwalker/sessionizer/pull/62))
- Smaller clippy updates ([#61](https://github.com/knutwalker/sessionizer/pull/61))

## [0.6.3](https://github.com/knutwalker/sessionizer/compare/0.6.2...0.6.3) - 2025-04-02

### Changes

- Trim the result of calling a command when setting env vars ([#59](https://github.com/knutwalker/sessionizer/pull/59))

## [0.6.2](https://github.com/knutwalker/sessionizer/compare/0.6.1...0.6.2) - 2025-04-02

### Changes

- Allow CDPATH paths no not exist ([#57](https://github.com/knutwalker/sessionizer/pull/57))

## [0.6.1](https://github.com/knutwalker/sessionizer/compare/0.6.0...0.6.1) - 2025-04-02

### Changes

- Fully qualify the parent pane ([#55](https://github.com/knutwalker/sessionizer/pull/55))
- Always provide the root path for a layout ([#54](https://github.com/knutwalker/sessionizer/pull/54))

## [0.6.0](https://github.com/knutwalker/sessionizer/compare/0.5.1...0.6.0) - 2025-03-30

### Changes

- Allow configuration of pane layout of initial window ([#53](https://github.com/knutwalker/sessionizer/pull/53))
- Default to remain=shell ([#52](https://github.com/knutwalker/sessionizer/pull/52))
- Add flag to skip running the init file ([#51](https://github.com/knutwalker/sessionizer/pull/51))
- Create init file when running edit and it doesn't exist ([#50](https://github.com/knutwalker/sessionizer/pull/50))
- Fmt
- Move to 2024 edition ([#48](https://github.com/knutwalker/sessionizer/pull/48))

## [0.5.1](https://github.com/knutwalker/sessionizer/compare/0.5.0...0.5.1) - 2025-01-23

### Changes

- Read the application config from a file called config.toml ([#46](https://github.com/knutwalker/sessionizer/pull/46))

## [0.5.0](https://github.com/knutwalker/sessionizer/compare/0.4.0...0.5.0) - 2024-09-05

### Changes

- Add an actual config over a path env variable ([#44](https://github.com/knutwalker/sessionizer/pull/44))

## [0.4.0](https://github.com/knutwalker/sessionizer/compare/0.3.5...0.4.0) - 2024-09-05

### Changes

- Add mode to use sessionizer as fake shell ([#42](https://github.com/knutwalker/sessionizer/pull/42))
- Sort items initially by their path order ([#41](https://github.com/knutwalker/sessionizer/pull/41))

## [0.3.5](https://github.com/knutwalker/sessionizer/compare/0.3.4...0.3.5) - 2024-06-25

### Changes

- Fixup issue-url filter and error render on failed edit ([#40](https://github.com/knutwalker/sessionizer/pull/40))
- Apply var expansion for cmd args as well ([#38](https://github.com/knutwalker/sessionizer/pull/38))

## [0.3.4](https://github.com/knutwalker/sessionizer/compare/0.3.3...0.3.4) - 2024-06-24

### Changes

- Don't use env filter anymore ([#36](https://github.com/knutwalker/sessionizer/pull/36))

## [0.3.3](https://github.com/knutwalker/sessionizer/compare/0.3.2...0.3.3) - 2024-06-24

### Changes

- Use custom parser for expanding shell vars ([#34](https://github.com/knutwalker/sessionizer/pull/34))
- Refactor init module into init and config ([#33](https://github.com/knutwalker/sessionizer/pull/33))

## [0.3.2](https://github.com/knutwalker/sessionizer/compare/0.3.1...0.3.2) - 2024-06-21

### Changes

- Don't require a git repo for build files ([#31](https://github.com/knutwalker/sessionizer/pull/31))

## [0.3.1](https://github.com/knutwalker/sessionizer/compare/0.3.0...0.3.1) - 2024-06-21

### Changes

- Make root project folders searchable ([#29](https://github.com/knutwalker/sessionizer/pull/29))

## [0.3.0](https://github.com/knutwalker/sessionizer/compare/0.2.1...0.3.0) - 2024-06-21

### Changes

- Allow the configuration of the search path ([#28](https://github.com/knutwalker/sessionizer/pull/28))
- Reduce dependencies surface, esp. regarding syn ([#26](https://github.com/knutwalker/sessionizer/pull/26))

## [0.2.1](https://github.com/knutwalker/sessionizer/compare/0.2.0...0.2.1) - 2024-06-11

### Changes

- Add info log after user selection ([#24](https://github.com/knutwalker/sessionizer/pull/24))
- Don't log warning when tmux is not running ([#23](https://github.com/knutwalker/sessionizer/pull/23))
- Apply simple variable expansion for env vars ([#21](https://github.com/knutwalker/sessionizer/pull/21))

## [0.2.0](https://github.com/knutwalker/sessionizer/compare/0.1.6...0.2.0) - 2024-06-07

### Changes

- Update rstest dependency ([#20](https://github.com/knutwalker/sessionizer/pull/20))
- Add commands for dealing with config/init files ([#17](https://github.com/knutwalker/sessionizer/pull/17))
- Use more structured internal error types ([#19](https://github.com/knutwalker/sessionizer/pull/19))
- Preparatory refactoring for upcoming config command ([#18](https://github.com/knutwalker/sessionizer/pull/18))
- Restore previous public API
- Update readme ([#15](https://github.com/knutwalker/sessionizer/pull/15))
- Rewrite some tests with rstest
- Refactor action to only represent possible actions
- Update toml spec
- Add some tests
- Split into modules
- Add different meaning for window.run
- Ignore the .cargoinstalled sentinel

## [0.1.6](https://github.com/knutwalker/sessionizer/compare/0.1.5...0.1.6) - 2024-02-17

### Changes

- Set $SESSION_ based env vars in a newly spawned tmux session
- Update changelog to remove newlines between items
- Update changelog template yet again
- Unignore tags files/folders
- Update changelog again
- Fix changelog
- Update changelog template

## [0.1.5](https://github.com/knutwalker/sessionizer/compare/0.1.4...0.1.5) - 2024-02-10

### Changes

- Update readme
- Update release-plz config
- Fix changelog

## [0.1.4](https://github.com/knutwalker/sessionizer/compare/0.1.3...0.1.4) - 2024-02-10

### Changes

- Add release-plz config ([#10](https://github.com/knutwalker/sessionizer/pull/10))
- Move everything into a lib ([#9](https://github.com/knutwalker/sessionizer/pull/9))
- Make clap types and fields pub ([#8](https://github.com/knutwalker/sessionizer/pull/8))
- Stricter permission checks for init files ([#7](https://github.com/knutwalker/sessionizer/pull/7))
- Run session initialization from a toml file ([#5](https://github.com/knutwalker/sessionizer/pull/5))

## [0.1.3](https://github.com/knutwalker/sessionizer/compare/v0.1.2...v0.1.3) - 2024-01-15

### Changes

- Add --select-1 equivalent
- Collect all trailing args into the initial query

## [0.1.2](https://github.com/knutwalker/sessionizer/compare/v0.1.1...v0.1.2) - 2024-01-12

### Changes

- Run an init file when a new session is created

## [0.1.1](https://github.com/knutwalker/sessionizer/compare/v0.1.0...v0.1.1) - 2024-01-09

### Changes

- Set correct category slug

## [0.1.0](https://github.com/knutwalker/sessionizer/releases/tag/v0.1.0) - 2024-01-09

### Changes

- Use variable directly
- Use released versions
- optional initial query
- Improve cargo usage
- Use fuzzy-select
