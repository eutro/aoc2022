# AOC 2022

[![Run All](https://github.com/eutro/aoc2022/actions/workflows/run.yml/badge.svg)](https://github.com/eutro/aoc2022/actions/workflows/run.yml)

These are my solutions to [Advent of Code
2022](https://adventofcode.com/2022), written in Prolog.

# Building

Solutions are written for [SWI-Prolog](https://www.swi-prolog.org/),
and it (the `swipl` command) is required to run these solutions. To
run the solution for a day, consult the source file and query tha
`main.` goal. This will read the input from `current_input` (stdin by
default, or `AOC_INPUT` when linked using `cmp.sh`/Tup), and output
answers to `current_output` (stdout by default). Some days also
require linking against their C source file.

Builds are automated using [Tup](https://gittup.org/tup/) (and the
scripts use it), but I'm sure you can compile things manually too
(yeehaw).

Here's what the scripts do:

- `./fetch.sh [day]`: fetch input for the given day (defaults to
  today), putting it in the `inputs` directory.
  - Requires `session.key` in the containing directory, which must
    contain your `session` cookie from the Advent of Code website.
- `./cmp.sh [day]`: compile the solution for the given day (or today),
  outputting to the `out` directory.
  - This just calls `tup` for the specified day.
- `./run.sh [day] [--]`: run the solution for the given day.
  - Recompiles by calling `cmp.sh`.
  - If `--` is not provided, fetch input (with `fetch.sh`) if not yet
    downloaded.
  - With `--`, just read input from stdin.
