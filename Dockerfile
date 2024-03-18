FROM alpine:3.12 as builder

ARG GHC=9.6.4

# install ghc and stack
RUN \
  apk add --no-cache git curl gcc g++ gmp-dev ncurses-dev libffi-dev make xz tar perl && \
  apk add --no-cache zlib zlib-dev zlib-static ncurses-static && \
  curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
  chmod +x /usr/bin/ghcup && \
  ghcup -v install ghc --set ${GHC} && \
  ghcup -v install cabal

COPY . /app

# install app
RUN \
  cd /app && \
  mkdir -p ~/.local/bin && \
  export PATH="/root/.ghcup/bin:$PATH" && \
  cabal update && \
  cabal install --installdir="$HOME/.local/bin" --install-method=copy --overwrite-policy=always --ghc-options='-split-sections -optl-static'

# strip binary
RUN strip -s /root/.local/bin/stack2cabal

FROM alpine:3.12

COPY --from=builder /root/.local/bin/stack2cabal /usr/bin/stack2cabal

RUN \
  apk add --no-cache git

ENTRYPOINT ["/usr/bin/stack2cabal"]
