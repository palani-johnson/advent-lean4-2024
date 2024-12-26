import Std

open Std.Internal.Parsec.String
open Std.Internal.Parsec

def sepBy (p : Parser α) (sep : Parser β) : Parser (List α) := (do
    let x ← p
    let xs ← many (sep *> p)
    pure (x :: xs.toList)
  ) <|> pure []
