#!/bin/bash

cabal v2-build all &&
STACK2CABAL=$(cabal v2-exec --verbose=0 --offline sh -- -c 'command -v stack2cabal') &&
for TEST in tests/* ; do
  pushd $TEST &&
  LC_ALL=C $STACK2CABAL &&
  popd ;
done &&
git diff tests
