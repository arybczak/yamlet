#!/usr/bin/env bash
# Download the data of the official YAML test suite into
# tests/fixtures/yaml-test-suite.
set -euo pipefail

cd "$(dirname "$0")/.."
rm -rf tests/fixtures/yaml-test-suite
git clone --quiet --depth 1 --branch data-2022-01-17 \
  https://github.com/yaml/yaml-test-suite.git tests/fixtures/yaml-test-suite
rm -rf tests/fixtures/yaml-test-suite/.git
# The directories name and tags only index the test cases with symbolic links.
# Cabal follows the links, so the source distribution would get each test case
# several times.
rm -rf tests/fixtures/yaml-test-suite/name tests/fixtures/yaml-test-suite/tags
