import Std
import Advent.Utils

open Std.Internal.Parsec.String

def Reports := List (List Int)

def inputParser : Parser Reports := sepBy (sepBy (digits) (skipChar ' ')) (skipChar '\n')

inductive Run where
  | incresing
  | decresing
  | notsafe
deriving BEq

def compareTwo (a b : Int) :=
  let diff := (a - b).natAbs

  if [1, 2, 3].contains diff then
    if a > b then
      Run.decresing
    else
      Run.incresing
  else
    Run.notsafe

def isSafe (report : List Int) :=
  let runs := report.zipWith compareTwo report.tail!

  runs.all (· == Run.decresing) || runs.all (· == Run.incresing)

def solve1 (reports : Reports) := (reports.map isSafe).count true

def solve2 (reports : Reports) :=
  "Not Implemented"

def main (args : List String) : IO Unit := do
  match args with
  | [] => return
  | filePath :: rest =>
    let fileContent <- IO.FS.readFile filePath
    let parseResult := inputParser.run fileContent

    match parseResult with
    | .error _ => IO.eprintln s!"Failed to parse {filePath}"
    | .ok problemInput =>
      String.intercalate "\n  " [
        s!"Solution for {filePath}:",
        s!"Part 1: {problemInput |> solve1}",
        s!"Part 2: {problemInput |> solve2}"
      ] |> IO.println

    main rest
