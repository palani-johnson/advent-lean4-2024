import Std
import Utils

-- Types

def Reports := List (List Int)

inductive Run where
  | incresing
  | decresing
  | notsafe
deriving BEq

-- Parsing

section
  open Std.Internal.Parsec.String

  def inputParser : Parser Reports := sepBy (sepBy digits (skipChar ' ')) (skipChar '\n')
end

-- Functions

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

def solve1 (reports : Reports) := reports.map isSafe |>.count true

def solve2 (reports : Reports) :=
  let isSafe2 (report : List Int) :=
    List.range report.length
    |>.map (report.eraseIdx · |> isSafe)
    |>.any (·)

  (reports.map isSafe2).count true

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
