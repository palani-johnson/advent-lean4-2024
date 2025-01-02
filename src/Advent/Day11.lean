import Std
import Utils

-- Types

inductive BlinkCases
| one (n : Nat)
| two (n1 n2 : Nat)

--Parsing

section
  open Std.Internal.Parsec.String

  def inputParser : Parser (List Nat) := sepBy (digits) (skipChar ' ')
end

-- Functions

partial def countDigits : Nat -> Nat
| 0 => 0
| n => 1 + countDigits (n / 10)

def splitDigits (n splitAt : Nat) :=
  let d := 10 ^ splitAt
  (n / d, n % d)

def blinkCases : Nat-> BlinkCases
  | 0 => .one 1
  | stone =>
    let digits := countDigits stone
    if digits % 2 == 0 then
      let (s1, s2) := splitDigits stone (digits / 2)
      .two s1 s2
    else
      .one (stone * 2024)

abbrev BlinkCache := Std.HashMap (Nat × Nat) Nat

partial def blinkMemoized (n count acc : Nat ) (cache : BlinkCache) : Nat × BlinkCache :=
  let key := (n, count)

  if let .some result := cache.get? key then
    (result + acc, cache)
  else if count == 0 then
    (acc + 1, cache)
  else match blinkCases n with
    | .one n' =>
      blinkMemoized n' (count - 1) acc cache
    | .two n1' n2' =>
      let (res1, cache1) := blinkMemoized n1' (count - 1) 0 cache
      let (res2, cache2) := blinkMemoized n2' (count - 1) 0 cache1
      let total := res1 + res2 + acc
      (total, cache2.insert key total)

def blink (n count : Nat) : Nat := blinkMemoized n count 0 {} |>.fst

-- Main

def main := aocMain λ file => do
  let parseResult := inputParser.run file.content

  match parseResult with
  | .error _ =>
    IO.eprintln s!"Failed to parse {file.path}"
  | .ok problemInput =>
    IO.println s!"Part 1: {problemInput.foldl (init := 0) (· + blink · 25)}"
    IO.println s!"Part 2: {problemInput.foldl (init := 0) (· + blink · 75)}"
