import Std
import Utils

-- Types

def ProblemInput := List (Int × Int)

--Parsing

section
  open Std.Internal.Parsec.String

  def inputParser : Parser ProblemInput := sepBy (do
      let a <- digits
      let _ <- ws
      let b <- digits
      return (a, b)
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

def main := aocMain λ file => do
  let parseResult := inputParser.run file.content

  match parseResult with
  | .error _ =>
    IO.eprintln s!"Failed to parse {file.path}"
  | .ok problemInput =>
    IO.println s!"Solution for {file.path}:"
    IO.println s!"Part 1: {solve1 problemInput}"
    IO.println s!"Part 2: {solve2 problemInput}"
