import Std

-- Main

structure File where
  path : System.FilePath
  content : String

partial def aocMain (mainFn : File -> IO Unit) (args : List String) : IO Unit := do
  for path in args do
    let path : System.FilePath := path

    if <- (path : System.FilePath).isDir then
      for path in <- path.readDir do
        aocMain mainFn [path.path.toString]

    else
      let fileContent <- IO.FS.readFile path
      IO.println s!"Solution for {path}:"
      mainFn ⟨path, fileContent.trim⟩

-- Parsing

section
  open Std.Internal.Parsec.String
  open Std.Internal.Parsec

  def sepBy (p : Parser α) (sep : Parser β) : Parser (List α) := (do
      let x ← p
      let xs ← many (sep *> p)
      pure (x :: xs.toList)
    ) <|> pure []
end


def get2D (input : List <| List α) (x y : Int) := do
  if x < 0 || y < 0 then Option.none

  let row <- input.get? x.toNat
  row.get? y.toNat

def set2D (input : List <| List α) (x y : Int) (a : α) := do
  if x < 0 || y < 0 || x >= input.length then Option.none

  let row <- input.get? x.toNat

  if y >= row.length then Option.none

  let row := row.set y.toNat a

  return input.set x.toNat row

def Std.HashSet.map
  [BEq α] [Hashable α] [BEq β] [Hashable β]
  (hashSet : Std.HashSet α) (f : α -> β): Std.HashSet β
:= Id.run do
  let mut newHashSet : Std.HashSet β := {}

  for a in hashSet do
    newHashSet := newHashSet.insert (f a)

  return newHashSet

-- points

structure Point where
  x : Int
  y : Int
deriving BEq, Hashable

inductive Direction where
  | north
  | south
  | east
  | west
deriving BEq, Repr, Hashable

def Point.nextPoint (point : Point) : Direction → Point
| .north => { point with x := point.x - 1}
| .south => { point with x := point.x + 1}
| .east  => { point with y := point.y + 1}
| .west  => { point with y := point.y - 1}
