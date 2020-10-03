![Haskell CI](https://github.com/hasufell/stack2cabal/workflows/Haskell%20CI/badge.svg)
![Docker build](https://github.com/hasufell/stack2cabal/workflows/Docker%20build/badge.svg)
[![license](https://img.shields.io/github/license/hasufell/stack2cabal.svg)](LICENSE)

# stack2cabal

This is a fork of [tseenshe/stack2cabal](https://gitlab.com/tseenshe/stack2cabal),
which seems abandoned/inactive.

## Installation

Clone the repo and build with either cabal or stack or see the [release page](https://github.com/hasufell/stack2cabal/releases)
for binaries.

### Docker

[![Docker pulls](https://img.shields.io/docker/pulls/hasufell/stack2cabal.svg)](https://hub.docker.com/repository/docker/hasufell/stack2cabal)
[![Docker stars](https://img.shields.io/docker/stars/hasufell/stack2cabal.svg)](https://hub.docker.com/repository/docker/hasufell/stack2cabal)
[![Docker image size](https://img.shields.io/docker/image-size/hasufell/stack2cabal/latest.svg)](https://hub.docker.com/repository/docker/hasufell/stack2cabal)

```sh
docker pull hasufell/stack2cabal
```

## Usage

To convert a `stack.yaml` to `cabal.project` simply cd to the project directory and run:

```sh
stack2cabal
```

This will also create a `cabal.project.freeze` based on the stack resolver.

Also see `stack2cabal --help` for further options.

### Docker

```sh
docker run --rm \
  -v /etc/passwd:/etc/passwd \
  -u `id -u`:`id -g` \
  -v `pwd`:`pwd` \
  -w `pwd` \
  --tmpfs "$HOME/.cache" \
  hasufell/stack2cabal
```

## Notes

- `stack2cabal` reads the local hackage index state (and caches the results in `XDG_CACHE/cabal-parsers`). This requires that `cabal update` was run at least once. Initial load may take a few seconds, subsequent ones should be faster.

- Hackage packages that are specified as git repositories in e.g. `extra-deps` might
have a different version than the stack resolver. `stack2cabal` will still record the
version of the stack resolver in `cabal.project.freeze` unless you pass `--freeze-remotes`
(which takes longer time).
