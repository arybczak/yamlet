#!/bin/sh
#
# For this to work you need to:
#
# - Put "write-ghc-environment-files: always" in your cabal.project.local.
#
# - Compile doctest with the same GHC version the project currently uses.
#

set -eu

for dir in src yamlet-aeson/src; do
  doctest \
    "$dir" \
    -XGHC2021 \
    -XDataKinds \
    -XDeriveAnyClass \
    -XDerivingStrategies \
    -XDerivingVia \
    -XDuplicateRecordFields \
    -XLambdaCase \
    -XMultiWayIf \
    -XNoFieldSelectors \
    -XOverloadedRecordDot \
    -XOverloadedStrings \
    -XTypeFamilies \
    -XUndecidableInstances
done
