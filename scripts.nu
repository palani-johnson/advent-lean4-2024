
export def fetch-aoc-input [] {
  for day in (1..25) {
    let day_padded = $day | fill -a right -c '0' -w 2
    let dir = $".data/day($day_padded)"

    mkdir $dir

    echo $"Fetching input for Day ($day_padded)..."

    http get -H ["Cookie" $"session=($env.AOC_SESSION_COOKIE)"] --raw $"https://adventofcode.com/2024/day/($day)/input"
      | str trim
      | save -f $"($dir)/input.txt"
  }
}