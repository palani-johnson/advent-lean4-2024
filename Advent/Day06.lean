import Std
import Advent.Utils

open Std.Internal.Parsec.String


inductive Tile where
  | object
  | empty
  | exit
deriving BEq

instance : ToString Tile where
  toString : _
  | .object => "□"
  | .empty => "·"
  | .exit => "x"


inductive Direction where
  | north
  | south
  | east
  | west
deriving Repr

def Direction.fromChar : Char -> Option Direction
  | '^' => .some .north
  | 'v' => .some .south
  | '>' => .some .east
  | '<' => .some .west
  | _   => .none

instance : ToString Direction where
  toString : _
  | .north => "^"
  | .south => "v"
  | .east => ">"
  | .west => "<"


structure Point where
  x : Int
  y : Int
deriving BEq


structure RoomState where
  guardLocation : Point
  guardDirection : Direction
  room : List (List Tile)

instance : ToString RoomState where
  toString r := r.room.enum.map (λ (x, row) => row.enum.map ( λ (y, tile) =>
    if r.guardLocation == { x := x, y := y } then
      toString r.guardDirection
    else
      toString tile
  ) |> String.intercalate "")
    |> String.intercalate "\n"

def RoomState.fromString (roomString : String) : Option RoomState := do
  let splitRoomString := roomString.trim.splitOn "\n"
  let room := splitRoomString.map (λ row =>
    row.data.map (λ tile => match tile with
      | '#' => Tile.object
      | _   => Tile.empty
    )
  )

  for (row, x) in splitRoomString.zip (List.range splitRoomString.length) do
    for (tile, y) in row.data.zip (List.range row.length) do
      if let .some direction := Direction.fromChar tile then
        return {
          room := room
          guardLocation := {
            x := x
            y := y
          }
          guardDirection := direction
        }

  .none

def RoomState.getTile (roomState : RoomState) (point : Point) :=
  match get2D roomState.room point.x point.y with
  | .some tile => tile
  | .none      => .exit

def Point.getNextPoint (point : Point) : Direction → Point
| .north => { point with x := point.x - 1}
| .south => { point with x := point.x + 1}
| .east  => { point with y := point.y + 1}
| .west  => { point with y := point.y - 1}

def Direction.rotate : Direction -> Direction
| .north => .east
| .east  => .south
| .south => .west
| .west  => .north

def RoomState.getGuardTile (roomState : RoomState) := roomState.getTile roomState.guardLocation


def countTiles (initRoomState : RoomState) : IO Int := do
  let mut roomState := initRoomState
  let mut visited : Array Point := #[]

  while roomState.getGuardTile != Tile.exit do
    match
      roomState.getTile <| roomState.guardLocation.getNextPoint roomState.guardDirection
    with
    | .object =>
      roomState := {
        roomState with
        guardDirection := roomState.guardDirection.rotate
      }
    | _ =>
      if not (visited.contains roomState.guardLocation) then
        visited := visited.push roomState.guardLocation

      roomState := {
        roomState with
        guardLocation := roomState.guardLocation.getNextPoint roomState.guardDirection
      }

  return visited.toList.length


def solve2 (initRoomState : RoomState) :=
  NotImplemented

def main (args : List String) : IO Unit := do
  match args with
  | [] => return
  | filePath :: rest =>
    let fileContent <- IO.FS.readFile filePath

    match RoomState.fromString fileContent.trim with
    | .none => IO.eprintln s!"Failed to parse {filePath}"
    | .some roomState =>
      String.intercalate "\n  " [
        s!"Solution for {filePath}:",
        s!"Part 1: {<- countTiles roomState}",
        s!"Part 2: {solve2 roomState}"
      ] |> IO.println

    main rest
