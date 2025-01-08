import Std
import Utils

-- Types
abbrev RoomState := Nat × Nat

structure RobotState where
  pos : Int × Int
  vel : Int × Int
deriving BEq

--Parsing

section
  open Std.Internal.Parsec
  open Std.Internal.Parsec.String

  def parseRoomState : Parser RoomState :=
    orElse (do
      let _ <- skipString "Room size: "
      let px <- digits
      let _ <- ws
      let py <- digits
      let _ <- skipChar '\n'

      return (px, py)
    ) (λ _ => pure (101, 103))

  def parseRobotState : Parser RobotState := do
    let _ <- skipString "p="
    let px <- digits
    let _ <- skipChar ','
    let py <- digits

    let _ <- skipString " v="
    let nvx <- optional (skipChar '-')
    let vx : Int <- digits
    let _ <- skipChar ','
    let nvy <- optional (skipChar '-')
    let vy : Int <- digits

    let vx := if nvx.isSome then -vx else vx
    let vy := if nvy.isSome then -vy else vy

    return { pos := (px, py), vel := (vx, vy) }

  def inputParser : Parser (RoomState × List RobotState) := do
    let roomState <- parseRoomState
    let robotStates <- sepBy parseRobotState (skipChar '\n')

    return (roomState, robotStates)
end

-- Functions

namespace RobotState
  instance : ToString RobotState where
    toString r := s!"pos={r.pos} vel={r.vel}"

  def move (robotState : RobotState) (roomState : RoomState) (time : Nat) :=
    let (rX, rY) := roomState
    let (pX, pY) := robotState.pos
    let (vX, vY) := robotState.vel

    (
      (pX + vX * time) % rX,
      (pY + vY * time) % rY
    )
end RobotState

def display (robotStates : List RobotState) (roomState : RoomState) : IO Unit := do
  let init := robotStates
  let mut robotStates := robotStates
  let (rX, rY) := roomState

  for t in List.range (rX * rY) do
    if init == robotStates && t > 0 then
      IO.println s!"All robots are back to their initial positions"
      break

    if t > 0 && ((t : Int) - 28) % 103 == 0 && ((t : Int) - 77) % 101 == 0 then
      IO.println s!"Time: {t}"
      for y in List.range rY do
        for x in List.range rX do
          let c := if robotStates.any (λ r => r.pos == ((x : Int), (y : Int)))
            then '#'
            else '.'
          IO.print c

        IO.println ""

      break

    robotStates := robotStates.map (λ r => { r with pos := r.move roomState 1 })

-- Main

def main := aocMain λ file => do
  let parseResult := inputParser.run file.content

  match parseResult with
  | .error _ =>
    IO.eprintln s!"Failed to parse {file.path}"
  | .ok (roomState, robotStates) =>
    let time := 100
    let positions := robotStates.map (·.move roomState time)

    let (rX, rY) := roomState
    let (qX, qY) := (rX / 2, rY / 2)

    let q1 := positions.filter (λ (x, y) => x >= 0 && x < qX  && y >= 0 && y < qY)  |>.length
    let q2 := positions.filter (λ (x, y) => x > qX && x <= rX && y >= 0 && y < qY)  |>.length
    let q3 := positions.filter (λ (x, y) => x >= 0 && x < qX  && y > qY && y <= rY) |>.length
    let q4 := positions.filter (λ (x, y) => x > qX && x <= rX && y > qY && y <= rY) |>.length

    let safetyFactor := q1 * q2 * q3 * q4

    IO.println s!"Part 1: {safetyFactor}"

    IO.println s!"Part 2: "

    display robotStates roomState
