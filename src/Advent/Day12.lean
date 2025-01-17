import Std
import Utils

-- Types

abbrev Garden := Std.HashMap Point Char
abbrev Group := List Point

-- Functions

def Std.HashSet.pop! [BEq α] [Hashable α] [Inhabited α] (hashSet : Std.HashSet α) : α × Std.HashSet α :=
  match hashSet.toList with
    | head :: _ => (head, hashSet.erase head)
    | _ => panic! "hashSet is empty"

def Garden.group (garden : Garden) (point : Point) : Group := Id.run do
  if let .some char := garden.get? point then
    let mut checked : Std.HashSet Point := {}
    let mut queue   : Std.HashSet Point := {point}
    let mut group   : Std.HashSet Point := {}

    while queue.size > 0 do
      let (point, queue') := queue.pop!
      queue := queue'

      if !checked.contains point then
        checked := checked.insert point

        if let .some char' := garden.get? point then
          if char == char' then
            queue := queue.insertMany point.neighbors
            group := group.insert point

    return group.toList
  else
    return []

def Group.fence (group : Group) : Group :=
  group
    |>.flatMap (·.neighbors)
    |>.filter (λ p => !group.contains p)

def Group.perimeter (group : Group) := group.fence.length

partial def Garden.price (garden : Garden) : Nat :=
  match garden.keys with
    | [] => 0
    | point :: _ =>
      let group := garden.group point
      let garden : Garden := garden.filter (λ p _ => !group.contains p)

      let area := group.length
      let perimeter := group.perimeter

      (area * perimeter) + garden.price

def countSides (group : Group) : Nat := Id.run do
  let mut sides := 0

  for point in group do
    for vert in [.north, .south] do
      for hori in [.west, .east] do
        let p1 := point.nextPoint vert
        let p2 := point.nextPoint hori
        let p3 := p2.nextPoint vert

        if
          (!group.contains p1 && !group.contains p2)
          || (group.contains p1 && group.contains p2 && !group.contains p3)
        then
          sides := sides + 1

  return sides

partial def Garden.bulkPrice (garden : Garden) : Nat :=
  match garden.keys with
    | [] => 0
    | point :: _ =>
      let group := garden.group point

      let area := group.length
      let sides := countSides group


      let garden : Garden := garden.filter (λ p _ => !group.contains p)

      (area * sides) + garden.bulkPrice


-- Main

def main := aocMain λ file => do
  let mut garden : Garden := { }

  for (x, row) in file.content.splitOn "\n" |>.enum do
    for (y, char) in row.data.enum do
      garden := garden.insert ⟨x, y⟩ char

  IO.println s!"Part 1: {garden.price}"
  IO.println s!"Part 2: {garden.bulkPrice}"
