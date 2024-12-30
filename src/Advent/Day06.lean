import Std
import Utils

-- Types

inductive Tile where
  | object
  | empty
  | exit
deriving BEq

inductive Direction where
  | north
  | south
  | east
  | west
deriving BEq, Repr, Hashable

structure Point where
  x : Int
  y : Int
deriving BEq, Hashable

structure LocationState where
  direction : Direction
  point : Point
deriving BEq, Hashable

structure RoomState where
  guard : LocationState
  room : List (List Tile)

-- Functions

instance : ToString Tile where
  toString
  | .object => "□"
  | .empty  => "·"
  | .exit   => "x"

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
  toString
  | .north => "^"
  | .south => "v"
  | .east  => ">"
  | .west  => "<"

def Point.nextPoint (point : Point) : Direction → Point
| .north => { point with x := point.x - 1}
| .south => { point with x := point.x + 1}
| .east  => { point with y := point.y + 1}
| .west  => { point with y := point.y - 1}

instance : ToString RoomState where
  toString r := r.room.enum
    |>.map ( λ (x, row) => row.enum
      |>.map ( λ (y, tile) =>
        if r.guard.point == { x, y } then
          toString r.guard.direction
        else
          toString tile
      )
      |> String.intercalate ""
    )
    |> String.intercalate "\n"

def RoomState.fromString (roomString : String) : Option RoomState := do
  let splitRoomString := roomString.trim.splitOn "\n"
  let room := splitRoomString
    |>.map ( λ row => row.data
      |>.map ( λ tile => match tile with
        | '#' => Tile.object
        | _   => Tile.empty
      )
    )

  for (row, x) in splitRoomString.zip (List.range splitRoomString.length) do
    for (tile, y) in row.data.zip (List.range row.length) do
      if let .some direction := Direction.fromChar tile then
        return {
          room
          guard.point := { x, y }
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

def RoomState.exitRoom (initRoomState : RoomState) : Option (Std.HashSet LocationState) := do
  let mut roomState := initRoomState
  let mut visited : Std.HashSet LocationState := {}

  while roomState.getGuardTile != Tile.exit do
    if visited.contains roomState.guard then
      .none

    visited := visited.insert roomState.guard
    roomState := roomState.step

  return visited


def countLoops (roomState : RoomState) (visitedLocations : Std.HashSet LocationState) : Int := Id.run do
  let visitedPoints := visitedLocations.map (·.point)
  let mut i := 0

  for {x, y} in visitedPoints do
    if let .some modifiedRoom := set2D roomState.room x y .object then
    let modifiedRoomState := {roomState with room := modifiedRoom}

    if let .none := modifiedRoomState.exitRoom then
      i := i + 1

  return i

-- Main

def main := aocMain λ file => do
  match RoomState.fromString file.content with
  | .none =>
    IO.eprintln s!"Failed to parse {file.path}"
  | .some roomState =>
    IO.println s!"Solution for {file.path}:"

    if let .some visitedLocations := roomState.exitRoom then
      IO.println s!"Part 1: { visitedLocations.map (·.point) |>.size }"
      IO.println s!"Part 2: { countLoops roomState visitedLocations }"
    else
      IO.println s!"Part 1: guard cannot exit room"
