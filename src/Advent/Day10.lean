import Std
import Utils

-- Types

inductive Direction where
  | left
  | right
  | up
  | down

def ProblemInput := Std.HashMap Nat (Direction × Nat)

-- Main

def main := aocMain λ file => do
  let parseResult := file.content.splitOn "\n" |>.map (·.data.map (s!"{·}".toNat!))
