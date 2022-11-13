#!/bin/bash

set -e

ghc_version="ghc-8.10.7"

repo_path="$(realpath "$0" | xargs dirname | xargs dirname)"
cd "$repo_path"

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal sdist --builddir="$dir"
cabal haddock --with-compiler="$ghc_version" --builddir="$dir" --haddock-for-hackage --enable-doc

cabal upload --documentation "$@" "$dir"/*-docs.tar.gz
