import Std
import Utils

-- Types

def TopoMap := Std.HashMap Point Nat
deriving EmptyCollection

--Parsing

namespace TopoMap

section
  open Std.Internal.Parsec
  open Std.Internal.Parsec.String

  def parser : Parser TopoMap := do
    let charToNat a := a.toNat - '0'.toNat
    let digitParser := do
      let a <- digit
      return charToNat a

    let grid <- sepBy (many digitParser) (skipChar '\n')

    let grid : Std.HashMap Point Nat := grid.enum
      |>.map (λ (x, row) => row.toList.enum.map (λ (y, height) => (⟨x, y⟩, height)))
      |>.flatten
      |> Std.HashMap.ofList

    let mut topoMap : TopoMap := {}

    for (point, height) in grid do
      let mut el := Std.HashMap.empty

      for direction in Direction.all do
        let nextPoint := point.nextPoint direction
        if grid.contains nextPoint then
          el := el.insert direction nextPoint

      topoMap := topoMap.insert point height

    return topoMap
end

def nextPoints (topoMap : TopoMap) (point : Point) : List Point :=
  if let .some height := topoMap.get? point then
    Direction.all.flatMap ( λ direction =>
      let nextPoint := point.nextPoint direction
      if let .some nextHeight := topoMap.get? nextPoint then
        if nextHeight == height + 1 then [nextPoint] else []
      else
        []
    )
  else
    []

def dijkstra (topoMap : TopoMap) (point : Point) := Id.run do
  let mut dist : Std.HashMap Point Float := topoMap.map λ _ _ => Float.inf
  let mut prev : Std.HashMap Point (Option Point) := topoMap.map λ _ _ => .none
  let mut queue := topoMap.keys

  dist := dist.insert point 0

  while queue.length > 0 do
    let (_, u) := queue.foldl (init := (Float.inf, ⟨-1, -1⟩)) (λ (d, p) p' =>
      let d' := dist.get! p'
      if d' <= d then
        (d', p')
      else
        (d, p)
    )

    queue := queue.erase u

    for v in topoMap.nextPoints u do
      let alt := 1 + dist.get! u
      if alt < dist.get! v then
        dist := dist.insert v alt
        prev := prev.insert v (.some u)

  return (dist, prev)

def sumTrailScores (topoMap : TopoMap) : Nat :=
  let sources := topoMap.filter (λ _ d => d == 0) |>.keys
  let targets := topoMap.filter (λ _ d => d == 9) |>.keys

  sources.map ( topoMap.dijkstra ·
    |>.fst.filter (λ t d => d < Float.inf && targets.contains t)
    |>.size
  )
  |>.sum

end TopoMap

-- Main

def main := aocMain λ file => do
  match TopoMap.parser.run file.content with
  | .error _ =>
    IO.eprintln s!"Failed to parse {file.path}"
  | .ok topoMap =>
    IO.println s!"Part 1: {topoMap.sumTrailScores}"
    -- IO.println s!"Part 2: {}"
