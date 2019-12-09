#!/bin/bash

cabal v2-build all &&
STACK2CABAL=$(cabal v2-exec -v0 which -- stack2cabal) &&
for TEST in tests/* ; do
  pushd $TEST &&
  LC_ALL=C $STACK2CABAL &&
  popd ;
done &&
git diff tests
