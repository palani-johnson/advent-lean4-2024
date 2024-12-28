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
deriving BEq, Repr, Hashable

def Direction.fromChar : Char -> Option Direction
  | '^' => .some .north
  | 'v' => .some .south
  | '>' => .some .east
  | '<' => .some .west
  | _   => .none

def Direction.rotate : Direction -> Direction
| .north => .east
| .east  => .south
| .south => .west
| .west  => .north

instance : ToString Direction where
  toString : _
  | .north => "^"
  | .south => "v"
  | .east => ">"
  | .west => "<"


structure Point where
  x : Int
  y : Int
deriving BEq, Hashable

def Point.nextPoint (point : Point) : Direction → Point
| .north => { point with x := point.x - 1}
| .south => { point with x := point.x + 1}
| .east  => { point with y := point.y + 1}
| .west  => { point with y := point.y - 1}

structure LocationState where
  direction : Direction
  point : Point
deriving BEq, Hashable

structure RoomState where
  guard : LocationState
  room : List (List Tile)

instance : ToString RoomState where
  toString r := r.room.enum.map (λ (x, row) => row.enum.map ( λ (y, tile) =>
    if r.guard.point == { x := x, y := y } then
      toString r.guard.direction
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
          guard.point := { x := x, y := y}
          guard.direction := direction
        }

  .none

def RoomState.getTile (roomState : RoomState) (point : Point) :=
  match get2D roomState.room point.x point.y with
  | .some tile => tile
  | .none      => .exit

def RoomState.getGuardTile (roomState : RoomState) := roomState.getTile roomState.guard.point

def RoomState.step (roomState : RoomState) :=
  let nextPoint := roomState.guard.point.nextPoint roomState.guard.direction

  match roomState.getTile <| nextPoint with
  | .object => { roomState with
      guard.direction := roomState.guard.direction.rotate
    }
  | _       => { roomState with
      guard.point := roomState.guard.point.nextPoint roomState.guard.direction
    }

def RoomState.blockPoint (initRoomState : RoomState) := do
  let blockerPoint := initRoomState.guard.point.nextPoint initRoomState.guard.direction
  let blockedRoom <- set2D initRoomState.room blockerPoint.x blockerPoint.y .object

  let mut roomState := {initRoomState with room := blockedRoom}
  let mut visited : Std.HashSet LocationState := {}

  while roomState.getGuardTile != Tile.exit do
    if visited.contains roomState.guard then
      return blockerPoint

    visited := visited.insert roomState.guard
    roomState := roomState.step

  .none


def countTiles (initRoomState : RoomState) : Int := Id.run do
  let mut roomState := initRoomState
  let mut visited : Std.HashSet Point := {}

  while roomState.getGuardTile != Tile.exit do
    if not (visited.contains roomState.guard.point) then
      visited := visited.insert roomState.guard.point

    roomState := roomState.step

  return visited.toList.length


def countLoops (initRoomState : RoomState) : IO Int := do
  let mut roomState := initRoomState
  let mut blockPoints : Std.HashSet Point := {}

  while roomState.getGuardTile != Tile.exit do
    if let .some blockPoint := roomState.blockPoint then
      if not (blockPoints.contains blockPoint) then
        blockPoints := blockPoints.insert blockPoint

    roomState := roomState.step

  return blockPoints.size

def main (args : List String) : IO Unit := do
  match args with
  | [] => return
  | filePath :: rest =>
    let fileContent <- IO.FS.readFile filePath

    match RoomState.fromString fileContent.trim with
    | .none =>
      IO.eprintln s!"Failed to parse {filePath}"
    | .some roomState =>
      IO.println s!"Solution for {filePath}:"
      IO.println s!"Part 1: { countTiles roomState }"
      IO.println s!"Part 2: { <- countLoops roomState}"

    main rest
