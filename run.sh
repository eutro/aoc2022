#!/usr/bin/env sh

DIR="$(readlink -f $(dirname "$0"))"

if [ "$1" = "--time" ]
then TIME="1"; shift
fi

re='^[0-9]+$'
if ! [ -z "$1" ] && test "$(echo "$1" | grep -E '^[0-9]+$')"
then DAYN="$1"; shift
else DAYN="$(date +%d)"
fi
DAY="$(echo "$DAYN" | sed 's/^0*//')"
DAYP="$(printf "%02d" "$DAY")"

if [ "$1" = "--" ]
then INPUT="/dev/stdin"; shift
else if [ ! -f "$DIR/input/day$DAYP.txt" ]
     then "$DIR/fetch.sh" "$DAY" || exit 1
     fi
     INPUT="$DIR/input/day$DAYP.txt"
fi

"$DIR/cmp.sh" "$DAY" "$@" || exit 1

if [ "$TIME" = 1 ]
then time "$DIR/out/day$DAYP" < "$INPUT"
else exec "$DIR/out/day$DAYP" < "$INPUT"
fi
