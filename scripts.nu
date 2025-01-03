export def fetch-aoc-day [session_token : string, day : int] {
  let day_padded = $day | fill -a right -c '0' -w 2
  let dir = $"data/day($day_padded)"

  mkdir $dir

  http get -H ["Cookie" $"session=($session_token)"] --raw $"https://adventofcode.com/2024/day/($day)/input"
    | str trim
    | save -f $"($dir)/input.txt"
}

export def fetch-aoc-all [session_token : string] {
  for day in (1..25) {
    fetch-aoc-day $session_token $day
  }
}