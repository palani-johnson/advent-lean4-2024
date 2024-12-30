import Std
import Utils

-- Types

def ProblemInput := List (Int × Int)

--Parsing

section
  open Std.Internal.Parsec.String

  def lineParser : Parser (Int × Int) := do
    let a <- digits
    let _ <- ws
    let b <- digits
    pure (a, b)

  def inputParser : Parser ProblemInput := sepBy (do
      let a <- digits
      let _ <- ws
      let b <- digits
      pure (a, b)
    ) (skipChar '\n')
end

-- Functions

def solve1 (problemInput : ProblemInput) :=
  let (lefts, rights) := problemInput.unzip

  lefts.mergeSort.zipWith (λ a b : Int => (a - b).natAbs) rights.mergeSort |> List.sum

def solve2 (problemInput : ProblemInput) :=
  let (lefts, rights) := problemInput.unzip

  lefts.foldl (λ acc left => acc + left * rights.count left) 0

-- Main

def main (args : List String) : IO Unit := do
  match args with
  | [] => return
  | filePath :: rest =>
    let fileContent <- IO.FS.readFile filePath
    let parseResult := inputParser.run fileContent.trim

    match parseResult with
    | .error _ =>
      IO.eprintln s!"Failed to parse {filePath}"
    | .ok problemInput =>
      IO.println s!"Solution for {filePath}:"
      IO.println s!"Part 1: {solve1 problemInput}"
      IO.println s!"Part 2: {solve2 problemInput}"


    main rest
