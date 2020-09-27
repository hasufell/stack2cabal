![Haskell CI](https://github.com/hasufell/stack2cabal/workflows/Haskell%20CI/badge.svg)
[![license](https://img.shields.io/github/license/hasufell/stack2cabal.svg)](LICENSE)

# stack2cabal

This is a fork of [tseenshe/stack2cabal](https://gitlab.com/tseenshe/stack2cabal),
which seems abandoned/inactive.

## Installation

Clone the repo and build with either cabal or stack or see the [release page](https://github.com/hasufell/stack2cabal/releases)
for binaries.

## Usage

To convert a `stack.yaml` to `cabal.project` simply cd to the project directory and run:

```
stack2cabal
```

This will also create a `cabal.project.freeze` based on the stack resolver.

Also see `stack2cabal --help` for further options.

## Limitations

Hackage packages that are specified as git repositories in e.g. `extra-deps` might
have a different version than the stack resolver. `stack2cabal` will still record the
version of the stack resolver in `cabal.project.freeze`. In order to fix that remove
the constraint of such dependencies entirely from the freeze file.

Also see [#1](https://github.com/hasufell/stack2cabal/issues/1).
