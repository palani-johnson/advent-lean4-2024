import Std
import Utils

-- Types

structure ClawMachine where
  aX : Int
  aY : Int
  bX : Int
  bY : Int
  pX : Int
  pY : Int

--Parsing

section
  open Std.Internal.Parsec.String

  def inputParser : Parser (List ClawMachine)  := sepBy (do
      let _  <- skipString "Button A: X+"
      let aX <- digits
      let _  <- skipString ", Y+"
      let aY <- digits
      let _  <- ws

      let _  <- skipString "Button B: X+"
      let bX <- digits
      let _  <- skipString ", Y+"
      let bY <- digits
      let _  <- ws

      let _  <- skipString "Prize: X="
      let pX <- digits
      let _  <- skipString ", Y="
      let pY <- digits

      return ⟨aX, aY, bX, bY, pX, pY⟩
    ) (skipString "\n\n")
end

-- Functions

/--
Counts the number of button presses (A, B) required to reach the prize if the prize is reachable.
-/
def ClawMachine.solve (cm : ClawMachine) : Option (Int × Int)  :=
  -- Use Cramer's rule to solve the system of equations

  -- Calculate the determinant of the matrix
  let d := cm.aX * cm.bY - cm.aY * cm.bX

  -- Calculate dA and dB
  let dA := cm.pX * cm.bY - cm.pY * cm.bX
  let dB := cm.aX * cm.pY - cm.aY * cm.pX

  -- Calculate the solution
  let A := dA / d
  let B := dB / d

  if
    (A * cm.aX + B * cm.bX, A * cm.aY + B * cm.bY) == (cm.pX, cm.pY)
  then
    .some (A, B)
  else
    .none

/--
Calculates the number of coins required to reach the prize if the prize is reachable.
Returns 0 if the prize is unreachable.
-/
def ClawMachine.coins (cm : ClawMachine) : Int  := if let .some (A, B) := cm.solve then 3 * A + B else 0

-- Main

def main := aocMain λ file => do
  let parseResult := inputParser.run file.content

  match parseResult with
  | .error _ =>
    IO.eprintln s!"Failed to parse {file.path}"
  | .ok clawMechines =>
    let solveCM (clawMechines : List ClawMachine) : Int := clawMechines.foldl (init := 0) (· + ·.coins)

    IO.println s!"Part 1: {solveCM clawMechines}"

    let clawMechines := clawMechines.map λ cm =>
      { cm with pX := cm.pX + 10000000000000, pY := cm.pY + 10000000000000 }

    IO.println s!"Part 2: {solveCM clawMechines}"
