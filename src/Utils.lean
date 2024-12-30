import Std

def NotImplemented := "Not Implemented"

structure File where
  path : System.FilePath
  content : String

def aocMain (mainFn : File -> IO Unit) (args : List String) : IO Unit := do
  for path in args do
    let path : System.FilePath := path

    if <- path.isDir then
      for path in <- path.readDir do
        let fileContent <- IO.FS.readFile path.path
        mainFn ⟨path.path, fileContent.trim⟩

    else
      let fileContent <- IO.FS.readFile path
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
