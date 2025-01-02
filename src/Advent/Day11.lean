import Std
import Utils

--Parsing

section
  open Std.Internal.Parsec.String

  def inputParser : Parser (List Nat) := sepBy (digits) (skipChar ' ')
end

-- Functions

def blink : Nat -> List Nat
| 0 => [1]
| stone =>
  let rep := stone.repr
  if rep.length % 2 == 0 then
    let (s1, s2) := rep.data.splitAt (rep.length / 2)
    [s1, s2].map (λ s => s.map (·.toString) |> String.join |>.toNat! )
  else
    [stone * 2024]

def blinkN (n : Nat) (input : List Nat) :=
  List.range n |>.foldl (init := input) (λ acc _ => acc.flatMap (blink ·))

-- Main

def main := aocMain λ file => do
  let parseResult := inputParser.run file.content

  match parseResult with
  | .error _ =>
    IO.eprintln s!"Failed to parse {file.path}"
  | .ok problemInput =>
    IO.println s!"Part 1: {blinkN 25 problemInput |>.length}"
    IO.println s!"Part 1: {blinkN 75 problemInput |>.length}"
