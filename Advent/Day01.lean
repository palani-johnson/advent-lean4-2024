import Std
import Advent.Utils

open Std.Internal.Parsec.String

def ProblemInput := List (Int × Int)

def lineParser : Parser (Int × Int) := do
  let a ← digits
  let _ ← ws
  let b ← digits
  pure (a, b)

def inputParser : Parser ProblemInput := sepBy lineParser (skipChar '\n')

def solve1 (problemInput : ProblemInput) :=
  let (lefts, rights) := problemInput.unzip

  lefts.mergeSort.zipWith (λ a b : Int => (a - b).natAbs) rights.mergeSort |> List.sum

def solve2 (problemInput : ProblemInput) :=
  let (lefts, rights) := problemInput.unzip

  lefts.foldl (λ acc left => acc + left * rights.count left) 0

def main (args : List String) : IO Unit := do
  match args with
  | [] => return
  | filePath :: rest =>
    let fileContent <- IO.FS.readFile filePath
    let parseResult := inputParser.run fileContent.trim

    match parseResult with
    | .error _ => IO.eprintln s!"Failed to parse {filePath}"
    | .ok problemInput =>
      String.intercalate "\n  " [
        s!"Solution for {filePath}:",
        s!"Part 1: {problemInput |> solve1}",
        s!"Part 2: {problemInput |> solve2}"
      ] |> IO.println

    main rest
