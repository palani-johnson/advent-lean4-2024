import Std
import Utils

-- Types

structure Point where
  x : Int
  y : Int
deriving BEq, Hashable

-- Functions

def getAntinode (p1 p2 : Point) : Point :=
  { x := 2 * p1.x - p2.x, y := 2 * p1.y - p2.y }

def inBounds (size p : Point) := p.x >= 0 && p.x < size.x && p.y >= 0 && p.y < size.y

def countAntinodesV1
  (antennaPoints : Std.HashMap Char (Std.HashSet Point))
  (size : Point)
  : Int
:= Id.run do
  let mut antinodes : Std.HashSet Point := {}
  let getAntinodes (p1 p2 : Point) : List Point := [
    getAntinode p1 p2,
    getAntinode p2 p1,
  ].filter (inBounds size ·)

  for (_, points) in antennaPoints do
    for p1 in points do
      for p2 in points do
        if p1 != p2 then
          antinodes := antinodes.insertMany (getAntinodes p1 p2)

  return antinodes.size


def countAntinodesV2
  (antennaPoints : Std.HashMap Char (Std.HashSet Point))
  (size : Point)
  : IO Int
:=  do
  let mut antinodes : Std.HashSet Point := {}
  let getAntinodes (p1 p2 : Point) : List Point := Id.run do
    let mut antinodes := [p1, p2]

    let mut n1 := getAntinode p1 p2
    let mut n2 := p1

    while inBounds size n1 do
      antinodes := n1 :: antinodes
      (n1, n2) := (getAntinode n1 n2, n1)

    n1 := getAntinode p2 p1
    n2 := p2

    while inBounds size n1 do
      antinodes := n1 :: antinodes
      (n1, n2) := (getAntinode n1 n2, n1)

    antinodes


  for (_, points) in antennaPoints do
    for p1 in points do
      for p2 in points do
        if p1 != p2 then
          antinodes := antinodes.insertMany (getAntinodes p1 p2)

  return antinodes.size

-- Main

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
        if antenna != '.' && antenna != '#' then
          antennaPoints := antennaPoints.insert antenna (
              antennaPoints.getD antenna {}
              |>.insert {x, y}
            )


        sizeY := max sizeY y
      sizeX := max sizeX x

    let size := { x := sizeX + 1, y := sizeY + 1 }

    IO.println s!"Solution for {filePath} {sizeX} by {sizeY}:"
    IO.println s!"Part 1: {countAntinodesV1 antennaPoints size}"
    IO.println s!"Part 2: {<- countAntinodesV2 antennaPoints size}"

    main rest
