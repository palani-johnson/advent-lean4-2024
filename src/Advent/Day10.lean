import Std
import Utils

-- Types

structure TrailMarker where
  x : Nat
  y : Nat
deriving BEq, Hashable

def TopoMap := Std.HashMap TrailMarker (Std.HashMap Direction (TrailMarker × Nat))

-- Functions

--Parsing

section
  open Std.Internal.Parsec
  open Std.Internal.Parsec.String

  def TopoMap.parser : Parser TopoMap := do
    let charToNat a := a.toNat - '0'.toNat
    let digitParser := do
      let a <- digit
      return charToNat a

    let grid <- sepBy (many digitParser) (skipChar '\n')

    let grid : List (TrailMarker × Nat) := grid.enum
      |>.map (λ (x, row) => row.toList.enum.map (λ (y, height) => (⟨x, y⟩, height)))
      |>.flatten

    return sorry
end



-- Main

def main := aocMain λ file => do
  let parseResult := TopoMap.parser.run file.content
