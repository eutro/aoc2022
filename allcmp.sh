#!/usr/bin/env sh

DIR="$(readlink -f "$(dirname "$0")")"
cd "$DIR" || exit 1
tup
