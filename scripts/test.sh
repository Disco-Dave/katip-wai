#!/bin/bash

set -e

repo_path="$(realpath "$0" | xargs dirname | xargs dirname)"
cd "$repo_path"

ghc_versions=("9.2" "9.4" "9.6")

cabal update

for ghc_version in "${ghc_versions[@]}"; do
  rm -rf ./dist-newstyle ./cabal.project.freeze
  cabal build --with-compiler="ghc-${ghc_version}" -O0 --flags pedantic all
  cabal test --with-compiler="ghc-${ghc_version}" -O0 --flags pedantic all
done

resolvers=("lts-20" "lts-21" "nightly")

for resolver in "${resolvers[@]}"; do
  rm -rf stack.yaml stack.yaml.lock .stack-work
  stack init --resolver="$resolver" --ignore-subdirs ./example
  stack build --fast --pedantic
  stack test --fast --pedantic
done
