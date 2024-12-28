import Std
import Advent.Utils

open Std.Internal.Parsec.String

structure Point where
  x : Int
  y : Int
deriving BEq, Hashable

def getAntinode (p1 p2 : Point) : List Point :=
  [
    {
      x := 2 * p1.x - p2.x,
      y := 2 * p1.y - p2.y
    },
    {
      x := 2 * p2.x - p1.x,
      y := 2 * p2.y - p1.y
    },
  ]

def countAntinodes
  (antennaPoints : Std.HashMap Char (Std.HashSet Point))
  (size : Point)
  : IO Int
:= do
  let mut antinodes : Std.HashSet Point := {}

  for (_, points) in antennaPoints do
    for p1 in points do
      for p2 in points do
        if p1 != p2 then
          antinodes := antinodes.insertMany (
              getAntinode p1 p2
              |>.filter ( λ p =>
                p.x >= 0
                && p.x <= size.x
                && p.y >= 0
                && p.y <= size.y
              )
            )

  return antinodes.size

def main (args : List String) : IO Unit := do
  match args with
  | [] => return
  | filePath :: rest =>
    let fileContent <- IO.FS.readFile filePath

    let mut antennaPoints : Std.HashMap Char (Std.HashSet Point) := {}
    let mut sizeX := 0
    let mut sizeY := 0

    for (x, row) in fileContent.trim.splitOn "\n" |>.enum do
      for (y, antenna) in row.trim.data.enum do
        if antenna != '.' then
          antennaPoints := antennaPoints.insert antenna (
              antennaPoints.getD antenna {}
              |>.insert {x, y}
            )


        sizeY := max sizeY y
      sizeX := max sizeX x

    let size := {x:=sizeX, y:=sizeY}

    IO.println s!"Solution for {filePath} {sizeX} by {sizeY}:"
    IO.println s!"Part 1: {<- countAntinodes antennaPoints size}"
    IO.println s!"Part 2: {NotImplemented}"

    main rest
