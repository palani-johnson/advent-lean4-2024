import Std
import Utils

-- Types

structure ClawMachine where
  aX : Nat
  aY : Nat
  bX : Nat
  bY : Nat
  pX : Nat
  pY : Nat

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

def prizeLost (clawMechine : ClawMachine) (pX pY : Nat) :=
  (pX > clawMechine.pX) || (pY > clawMechine.pY)

def prizeAt (clawMechine : ClawMachine) (pX pY : Nat) :=
  (pX == clawMechine.pX) && (pY == clawMechine.pY)

partial def minmax
  (cm : ClawMachine)
  (pX pY : Nat)
  (spentCoins : Float)
  : Float
:=
  if prizeLost cm pX pY || spentCoins > 305 then
    Float.inf

  else if prizeAt cm pX pY then
    spentCoins

  else
    let a := minmax cm (pX + cm.aX) (pY + cm.aY) (spentCoins + 3)
    let b := minmax cm (pX + cm.bX) (pY + cm.bY) (spentCoins + 1)

    min a b

def solve1 (clawMechines : List ClawMachine) :=
  clawMechines.foldl (init := 0) λ acc cm =>
    let mm := minmax cm 0 0 0
    if mm == Float.inf then
      acc
    else
      mm + acc


-- Main

def main := aocMain λ file => do
  let parseResult := inputParser.run file.content

  match parseResult with
  | .error _ =>
    IO.eprintln s!"Failed to parse {file.path}"
  | .ok problemInput =>
    IO.println s!"Part 1: {solve1 problemInput}"
