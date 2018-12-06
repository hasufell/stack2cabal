FROM haskell:8.4.4

# TODO image with multiple ghc

# docker build -t registry.gitlab.com/tseenshe/stackage-to-hackage:8.4.4 .
# docker push registry.gitlab.com/tseenshe/stackage-to-hackage:8.4.4

RUN apt-get -y update &&\
    apt-get -y install libssl-dev &&\
    apt-get clean
