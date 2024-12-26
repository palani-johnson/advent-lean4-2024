import Std
import Advent.Utils

open Std.Internal.Parsec.String

def ProblemInput := List (List Int)

def inputParser : Parser ProblemInput := sepBy (sepBy (digits) (skipChar '\n')) (skipChar '\n')

def solve1 (problemInput : ProblemInput) :=
  "Not Implemented"

def solve2 (problemInput : ProblemInput) :=
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
