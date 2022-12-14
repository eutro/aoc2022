#!/usr/bin/env sh

DIR="$(readlink -f "$(dirname "$0")")"

if [ "$1" = "--time" ]
then GOAL="use_module(library(statistics)), time(main)."; shift
else GOAL="main."
fi

if [ -z "$1" ]
then DAYN="$(date +%d)"
else DAYN="$1"; shift
fi
DAY="$(echo "$DAYN" | sed 's/^0*//')"
DAYP="$(printf "%02d" "$DAY")"

SUMFILE="$DIR/hashes/day$DAYP.sha256"
if [ -f "$SUMFILE" ] && (echo "$GOAL" "$@" | sha256sum -c "$SUMFILE")
then exit 0
fi

mkdir -p "$DIR/out"
mkdir -p "$DIR/hashes"
echo "$GOAL" "$@" | sha256sum "$DIR/src/day$DAYP.pl" /dev/stdin > "$SUMFILE"
swipl "$@" -o "$DIR/out/day$DAYP" -g "$GOAL" -c "$DIR/src/day$DAYP.pl"
exit $?
