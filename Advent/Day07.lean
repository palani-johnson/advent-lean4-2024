import Std
import Advent.Utils

open Std.Internal.Parsec.String

structure Calibration where
  check : Nat
  values : List Nat

def calibrate (check : Nat) : List Nat -> Bool
| [] => false
| [v] => check == v
| a :: b :: rest =>
  calibrate check ((a + b) :: rest) || calibrate check ((a * b) :: rest)
termination_by l => l.length
decreasing_by all_goals simp


def lineParser : Parser Calibration := do
  let check <- digits
  let _ <- pstring ": "
  let values <- sepBy digits (skipChar ' ')
  return { check, values }

def inputParser : Parser (List Calibration) := sepBy lineParser (skipChar '\n')


def solve1 (calibrations : List Calibration) :=
  let rec calibrate (check : Nat) : List Nat -> Bool
  | [] => false
  | [v] => check == v
  | a :: b :: rest =>
    calibrate check ((a + b) :: rest)
    || calibrate check ((a * b) :: rest)
  termination_by l => l.length
  decreasing_by all_goals simp

  calibrations.foldl (init := 0) (λ acc c =>
    if calibrate c.check c.values
    then acc + c.check
    else acc
  )

def solve2 (calibrations : List Calibration) :=
  let rec calibrate (check : Nat) : List Nat -> Bool
  | [] => false
  | [v] => check == v
  | a :: b :: rest =>
    calibrate check ((a + b) :: rest)
    || calibrate check ((a * b) :: rest)
    || calibrate check (s!"{a}{b}".toNat! :: rest)
  termination_by l => l.length
  decreasing_by all_goals simp

  calibrations.foldl (init := 0) (λ acc c =>
    if calibrate c.check c.values
    then acc + c.check
    else acc
  )

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
      IO.println  s!"Solution for {filePath}:"
      IO.println  s!"Part 1: {problemInput |> solve1}"
      IO.println  s!"Part 2: {problemInput |> solve2}"


    main rest
