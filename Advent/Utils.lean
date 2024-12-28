import Std

open Std.Internal.Parsec.String
open Std.Internal.Parsec

def NotImplemented := "Not Implemented"

def sepBy (p : Parser α) (sep : Parser β) : Parser (List α) := (do
    let x ← p
    let xs ← many (sep *> p)
    pure (x :: xs.toList)
  ) <|> pure []


def get2D (input : List <| List α) (x y : Int) := do
  if x < 0 || y < 0 then Option.none

  let row <- input.get? x.toNat
  row.get? y.toNat

def set2D (input : List <| List α) (x y : Int) (a : α) := do
  if x < 0 || y < 0 || x >= input.length then Option.none

  let row <- input.get? x.toNat

  if y >= row.length then Option.none

  let row := row.set y.toNat a

  return input.set x.toNat row
