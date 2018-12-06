FROM haskell:8.4.4

# TODO image with multiple ghc

# docker build . -t registry.gitlab.com/tseenshe/stackage-to-hackage:8.4.4 --squash
# docker push registry.gitlab.com/tseenshe/stackage-to-hackage:8.4.4

RUN apt-get -y update &&\
    apt-get -y install libssl-dev &&\
    apt-get clean

# adds a base cache: gitlab caches are unreliable
COPY . /workdir
RUN cd /workdir &&\
    cabal v2-update &&\
    cabal v2-build &&\
    cd /root && rm -rf /workdir
