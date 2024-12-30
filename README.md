# Advent of Code 2024

Solutions for [Advent of Code 2024](https://adventofcode.com/2024) in [Lean 4](https://leanprover-community.github.io/get_started.html).

## Fetching input

Copy your session token and save it to the file `env.nu`:

```shell
use scripts.nu

$env.AOC_SESSION_TOKEN = $YOUR_SESSION_TOKEN
```

Source the environment with `source env.nu`

To fetch all 25 days run:
```shell
scripts fetch-aoc-input
```

To fetch a specific day run:
```shell
scripts fetch-aoc-day $day
```

## Executing solutions

Solutions for indvidual days can be executed by running `lean exec` on a day  and passing in an array of directories or files

The following will solve day 1 for all files found in `.data/day01`:

```shell
lake exec day01 .data/day01
```

