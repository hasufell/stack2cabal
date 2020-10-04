[![Gitter](https://badges.gitter.im/hasufell/stack2cabal.svg)](https://gitter.im/hasufell/stack2cabal?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)
[![Haskell CI](https://github.com/hasufell/stack2cabal/workflows/Haskell%20CI/badge.svg)](https://github.com/hasufell/stack2cabal/actions)
[![Docker build](https://github.com/hasufell/stack2cabal/workflows/Docker%20build/badge.svg)](https://github.com/hasufell/stack2cabal/actions)
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

- Hackage packages that are specified as git repositories in e.g. `extra-deps` might
have a different version than the stack resolver. Therefore `stack2cabal` will download
all repos and ignore their package names when generating `cabal.project.freeze`.
This can take some time depending on your project. Pass `--no-inspect-remotes` to skip this.
