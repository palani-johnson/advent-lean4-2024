import Std
import Advent.Utils

open Std.Internal.Parsec.String
open Std.Internal.Parsec

inductive Instruction where
  | iMul (a b : Int)
  | iDo
  | iDont
deriving BEq, Repr

def ProblemInput := Array Instruction

def parseMul : Parser Instruction := do
  let _ <- pstring "mul("
  let a <- digits
  let _ <- pchar ','
  let b <- digits
  let _ <- pchar ')'

  return .iMul a b

partial def parseFirstMul : Parser Instruction := attempt parseMul <|> attempt (skip *> parseFirstMul)

def parseInput : Parser ProblemInput := many parseFirstMul

#eval parseInput.run " d m mul(11,2) mul(3,4)nfsdgkj\nmul(5,6)mul(8, 9)mmul(1,2)ggggg"
#eval parseInput.run "mul(1,2)mul(1,2)mul(1,2)"

def solve1 (problemInput : ProblemInput) := problemInput.toList
  |>.map ( λ
    | .iMul a b => a * b
    | _ => 0
  )
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
