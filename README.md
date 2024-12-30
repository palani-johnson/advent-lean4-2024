# Advent of Code 2024

Solutions for [Advent of Code 2024](https://adventofcode.com/2024) in [Lean 4](https://leanprover-community.github.io/get_started.html).

## Fetching input

Copy your session token and save it to the file `env.nu`:

```shell
use scripts.nu

$env.AOC_SESSION_TOKEN = $YOUR_SESSION_TOKEN
```

Source the environment with `source env.nu`

Fetch all 25 days `scripts fetch-aoc-input`

Fetch a specific day `scripts fetch-aoc-day $day`

