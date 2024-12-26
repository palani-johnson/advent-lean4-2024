import Std
import Advent.Utils

open Std.Internal.Parsec.String
open Std.Internal.Parsec


def ProblemInput := Array (Int × Int)

def parseMul : Parser (Int × Int) := do
  let _ <- pstring "mul("
  let a <- digits
  let _ <- pchar ','
  let b <- digits
  let _ <- pchar ')'

  return (a, b)

partial def parseFirstMul : Parser (Int × Int) := attempt parseMul <|> skip *> parseFirstMul

def parseInput : Parser ProblemInput := many parseFirstMul

#eval parseInput.run " d m mul(11,2) mul(3,4)nfsdgkj\nmul(5,6)mul(8, 9)mmul(1,2)"
#eval parseInput.run "mul(1,2)mul(1,2)mul(1,2)g"

def solve1 (problemInput : ProblemInput) := problemInput
  |>.map (λ (a, b) => a * b)
  |>.toList
  |>.sum

def solve2 (_ : ProblemInput) :=
  "Not Implemented"

def main (args : List String) : IO Unit := do
  match args with
  | [] => return
  | filePath :: rest =>
    let fileContent <- IO.FS.readFile filePath
    let parseResult := parseInput.run fileContent

    match parseResult with
    | .error _ => IO.eprintln s!"Failed to parse {filePath}"
    | .ok problemInput =>
      String.intercalate "\n  " [
        s!"Solution for {filePath}:",
        s!"Part 1: {problemInput |> solve1}",
        s!"Part 2: {problemInput |> solve2}"
      ] |> IO.println

    main rest
