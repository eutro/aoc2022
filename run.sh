#!/usr/bin/env sh

DIR="$(readlink -f "$(dirname "$0")")"

if [ "$1" = "--time" ]
then TIME="--time"; shift
fi

if [ -n "$1" ] && test "$(echo "$1" | grep -E '^[0-9]+$')"
then DAYN="$1"; shift
else DAYN="$(date +%d)"
fi
DAY="$(echo "$DAYN" | sed 's/^0*//')"
DAYP="$(printf "%02d" "$DAY")"

export AOC_INPUT
if [ "$1" = "--" ]
then AOC_INPUT="/dev/stdin"; shift
elif [ "$1" = "-i" ]
then AOC_INPUT="$2"; shift; shift
else
    if [ ! -f "$DIR/input/day$DAYP.txt" ]
    then "$DIR/fetch.sh" "$DAY" || exit 1
    fi
    AOC_INPUT="$DIR/input/day$DAYP.txt"
fi

"$DIR/cmp.sh" $TIME "$DAY" "$@" || exit 1

exec "$DIR/out/day$DAYP"
