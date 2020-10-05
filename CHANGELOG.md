# Revision history for stack2cabal

## 1.0.11 -- ????-??-??

- Fix bug in `cabal.project.freeze` when git-package in extra-deps doesn't have a subdir
- Run hpack for git deps as well
- Fix `--output-file` option to use it as full filepath (instead of base directory)

## 1.0.10 -- 2020-10-04

- Allow to pin hackage state wrt #20
- Fix missing flags in cabal.project.freeze wrt #24
- fix bug in ghc-options conversion

## 1.0.9 -- 2020-10-04

- Inspect remote repository package names and exclude them from freeze file (fixes #1), disable with `--no-inspect-remotes`
- Add `--no-pin-ghc` option
- Add `--no-run-hpack` option
- Add `--output-file` option
- Also parse ghc-options wrt #9
- Fix path separators on windows wrt #5

## 1.0.8 -- 2020-09-25

- fix not parsing local deps [by d86leader](https://gitlab.com/d86leader/stack2cabal/-/commit/bd2370c8a453d2dec5546ab936604b2d7d9f6be2)
- add cli argument parses for specifying stack file
- Make stack2cabal comaptible with HsYAML>=2.0 [by Roman Melnikov](https://gitlab.com/serokell/morley/stack2cabal/-/commit/9c352382788c6f0c1917d877f6b7abdf3f96484a)
- Relax upper bound on Cabal
- Allow resolver to point to remote url
