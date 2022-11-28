#!/usr/bin/env sh

DIR="$(readlink -f $(dirname "$0"))"
if [ -z "$1" ]
then DAYN="$(date +%d)"
else
    DAYN="$1"
    shift
fi
DAY="$(echo "$DAYN" | sed 's/^0*//')"
DAYP="$(printf "%02d" "$DAY")"

mkdir -p "$DIR/out"
cd "$DIR/src"
swipl "$@" -o "$DIR/out/day$DAYP" -g "main." -c "$DIR/src/day$DAYP.pl"
exit $?
