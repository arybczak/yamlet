#!/usr/bin/env bash
# Download the data of the official YAML test suite into tests/yaml-test-suite.
set -euo pipefail

cd "$(dirname "$0")/.."
rm -rf tests/yaml-test-suite
git clone --quiet --depth 1 --branch data-2022-01-17 \
  https://github.com/yaml/yaml-test-suite.git tests/yaml-test-suite
rm -rf tests/yaml-test-suite/.git
