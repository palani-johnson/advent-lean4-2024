import Std
import Utils

-- Types

structure Calibration where
  check : Nat
  values : List Nat

-- Parsing

section
  open Std.Internal.Parsec.String

  def lineParser : Parser Calibration := do
    let check <- digits
    let _ <- pstring ": "
    let values <- sepBy digits (skipChar ' ')
    return { check, values }

  def inputParser : Parser (List Calibration) := sepBy lineParser (skipChar '\n')
end

-- Functions

def calibrate1 (check : Nat) : List Nat -> Bool
| [] => false
| [v] => check == v
| a :: b :: rest =>
  calibrate1 check ((a + b) :: rest)
  || calibrate1 check ((a * b) :: rest)
termination_by l => l.length
decreasing_by all_goals simp

def calibrate2 (check : Nat) : List Nat -> Bool
| [] => false
| [v] => check == v
| a :: b :: rest =>
  calibrate2 check ((a + b) :: rest)
  || calibrate2 check ((a * b) :: rest)
  || calibrate2 check (s!"{a}{b}".toNat! :: rest)
termination_by l => l.length
decreasing_by all_goals simp

def sumCalibrations (calibrations: List Calibration) (calibrate : Nat -> List Nat -> Bool) :=
  calibrations.foldl (init := 0) (λ acc {check, values} =>
    if calibrate check values
    then acc + check
    else acc
  )

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
    | .ok calibrations =>
      IO.println  s!"Solution for {filePath}:"
      IO.println  s!"Part 1: { sumCalibrations calibrations calibrate1 }"
      IO.println  s!"Part 2: { sumCalibrations calibrations calibrate2 }"

    main rest
