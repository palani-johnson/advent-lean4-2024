import Std
import Utils

-- Types

inductive Instruction where
  | iMul (a b : Int)
  | iDo
  | iDont
deriving BEq, Repr

def ProblemInput := Array Instruction

-- Parsing

section
  open Std.Internal.Parsec.String
  open Std.Internal.Parsec

  def parseMul : Parser Instruction := do
    let _ <- pstring "mul("
    let a <- digits
    let _ <- pchar ','
    let b <- digits
    let _ <- pchar ')'

    return .iMul a b

  def parseDo : Parser Instruction := do
    let _ <- pstring "do()"
    return .iDo

  def parseDont : Parser Instruction := do
    let _ <- pstring "don't()"
    return .iDont

  partial def parseFirstMul : Parser Instruction := attempt parseMul
    <|> attempt parseDo
    <|> attempt parseDont
    <|> attempt (skip *> parseFirstMul)

  def parseInput : Parser ProblemInput := many parseFirstMul
end

-- Functions

def solve1 (problemInput : ProblemInput) := problemInput.toList
  |>.map ( λ
    | .iMul a b => a * b
    | _ => 0
  )
  |>.sum

def solve2 (problemInput : ProblemInput) := problemInput.toList
  |>.foldl (init := (Instruction.iDo, 0)) ( λ
    | (_, acc), .iDo => (.iDo, acc)
    | (_, acc), .iDont => (.iDont, acc)
    | (.iDont, acc), _ => (.iDont, acc)
    | (_, acc), .iMul a b => (.iDo, acc + a * b)
  )
  |>.2

-- Main

def main (args : List String) : IO Unit := do
  match args with
  | [] => return
  | filePath :: rest =>
    let fileContent <- IO.FS.readFile filePath
    let parseResult := parseInput.run fileContent.trim

    match parseResult with
    | .error _ =>
      IO.eprintln s!"Failed to parse {filePath}"
    | .ok problemInput =>
      IO.println  s!"Solution for {filePath}:"
      IO.println s!"Part 1: {solve1 problemInput}"
      IO.println s!"Part 2: {solve2 problemInput}"

    main rest
