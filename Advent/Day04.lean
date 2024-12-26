import Std
import Advent.Utils

open Std.Internal.Parsec.String

def ProblemInput := List <| List Char

def splitInput (input : String) : ProblemInput := input.splitOn "\n" |>.map (·.data)

def exampleInput := "....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX" |> splitInput



def get2D (input : ProblemInput) (x y : Int) := do
  if x < 0 || y < 0 then
    Option.none
  else
    let row <- input.get? x.toNat
    row.get? y.toNat

def findXMAS (input : ProblemInput) (x y dx dy : Int) :=
  let offsets : List (Char × Int) := "XMAS".data.zip [0, 1, 2, 3]

  offsets.map (λ (char, offset) =>
    (get2D input (x + offset * dx) (y + offset * dy)) == (Option.some char)
  )
  |>.all (·)

def solve1 (input : ProblemInput) :=
  let numRows := input.length
  let numCols := input.head! |>.length

  let offsets := [
    (1, 0), (-1, 0),
    (0, 1), (0, -1),
    (1, 1), (-1, -1),
    (1, -1), (-1, 1),
  ]

  List.range numRows |>.map (λ x =>
    List.range numCols |>.map (λ y =>
      offsets.map (λ (dx, dy) =>
        findXMAS input x y dx dy
      )
      |>.count true
    )
    |>.sum
  )
  |>.sum

def solve2 (_ : ProblemInput) :=
  NotImplemented

def main (args : List String) : IO Unit := do
  match args with
  | [] => return
  | filePath :: rest =>
    let fileContent <- IO.FS.readFile filePath
    let input := fileContent |> splitInput

    String.intercalate "\n  " [
        s!"Solution for {filePath}:",
        s!"Part 1: {input |> solve1}",
        s!"Part 2: {input |> solve2}"
    ] |> IO.println

    main rest
