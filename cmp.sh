#!/usr/bin/env sh

DIR="$(readlink -f "$(dirname "$0")")"

if [ "$1" = "--time" ]
then GOAL="use_module(library(statistics)), time(main)."; shift
else GOAL="main."
fi

if [ -z "$1" ]
then DAYN="$(date +%d)"
else
    DAYN="$1"
    shift
fi
DAY="$(echo "$DAYN" | sed 's/^0*//')"
DAYP="$(printf "%02d" "$DAY")"

mkdir -p "$DIR/out"
swipl "$@" -o "$DIR/out/day$DAYP" -g "$GOAL" -c "$DIR/src/day$DAYP.pl"
exit $?
