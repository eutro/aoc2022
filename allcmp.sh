#!/usr/bin/env sh

DIR="$(readlink -f $(dirname "$0"))"
CMP="$DIR/cmp.sh"

cd "$DIR/src/"
DAYS_DONE="$(find -name "day*.pl" | sort | tail -1 | sed -E 's/[^0-9]//g')"

EXIT_CODE=0
for i in $(seq "$DAYS_DONE")
do echo "Day $i"
   if ! "$CMP" --time $i -O
   then EXIT_CODE=1
   fi
   echo
done

exit "$EXIT_CODE"
