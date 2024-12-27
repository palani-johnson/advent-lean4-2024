import Std
import Advent.Utils

open Std.Internal.Parsec.String

def ProblemInput := List <| List Char

def splitInput (input : String) : ProblemInput := input.splitOn "\n" |>.map (·.data)

def solve1 (input : ProblemInput) :=
  let numRows := input.length
  let numCols := input.head! |>.length

  let offsets := [
    (1, 0), (-1, 0),
    (0, 1), (0, -1),
    (1, 1), (-1, -1),
    (1, -1), (-1, 1),
  ]

  let xmasOffsets : List (Char × Int) := "XMAS".data.zip [0, 1, 2, 3]

  List.range numRows |>.map (λ x =>
    List.range numCols |>.map (λ y =>
      offsets.map (λ (dx, dy) =>
        xmasOffsets.map (λ (char, offset) =>
          get2D input (x + offset * dx) (y + offset * dy) == Option.some char
        )
        |>.all (·)
      )
      |>.count true
    )
    |>.sum
  )
  |>.sum

def solve2 (input : ProblemInput) :=
  let numRows := input.length
  let numCols := input.head! |>.length

  let masOffsets := [
    [('A', 0, 0), ('M', -1, -1), ('S', 1, 1), ('M', -1, 1), ('S', 1, -1)],
    [('A', 0, 0), ('S', -1, -1), ('M', 1, 1), ('M', -1, 1), ('S', 1, -1)],
    [('A', 0, 0), ('S', -1, -1), ('M', 1, 1), ('S', -1, 1), ('M', 1, -1)],
    [('A', 0, 0), ('M', -1, -1), ('S', 1, 1), ('S', -1, 1), ('M', 1, -1)],
  ]

  List.range numRows |>.map (λ x =>
    List.range numCols |>.map (λ y =>
      masOffsets.map (λ masoffset =>
        masoffset.map (λ (char, dx, dy) =>
          get2D input (x + dx) (y + dy) == Option.some char
        )
        |>.all (·)
      )
      |>.any (·)
    )
    |>.count true
  )
  |>.sum

def main (args : List String) : IO Unit := do
  match args with
  | [] => return
  | filePath :: rest =>
    let fileContent <- IO.FS.readFile filePath
    let input := fileContent.trim |> splitInput

    String.intercalate "\n  " [
      s!"Solution for {filePath}:",
      s!"Part 1: {input |> solve1}",
      s!"Part 2: {input |> solve2}",
    ] |> IO.println

    main rest
