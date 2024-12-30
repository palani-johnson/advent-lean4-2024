import Std
import Utils

-- Types

def ProblemInput := List <| List Char

-- Parsing

def splitInput (input : String) : ProblemInput := input.splitOn "\n" |>.map (·.data)

-- Functions

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

-- Main

def main := aocMain λ file => do
  let input := file.content |> splitInput

  IO.println  s!"Solution for {file.path}:"
  IO.println s!"Part 1: {solve1 input}"
  IO.println s!"Part 2: {solve2 input}"
