import Std
import Utils

-- Types

inductive FileBlock where
  | empty (size : Nat)
  | file (id : Nat) (size : Nat)

def FileArray := Array FileBlock
deriving Inhabited

inductive DiskBlock where
  | empty
  | file (id : Nat)

def DiskArray := Array DiskBlock
deriving Inhabited

-- Functions

def FileArray.fromString (string : String) : FileArray := string.trim.data.enum
  |>.map ( λ (i, c) =>
    let size := c.toString.toNat!
    if i.mod 2 == 0 then
      .file (i / 2) size
    else
      .empty size
  )
  |>.toArray

def FileArray.mergeEmpty (fileArray : FileArray) : FileArray := fileArray.foldl (init := []) (λ
    | acc,              .empty 0 => acc
    | .empty a :: acc,  .empty b => .empty (a+b) :: acc
    | acc,              item     => item :: acc
  )
  |>.reverse.toArray

partial def FileArray.condense! (fileArray : FileArray) (id left right: Nat) : FileArray :=
  if id < 0 then
    fileArray
  else if left >= right then
    fileArray.condense! (id - 1) 0 (fileArray.size - 1)
  else
    let nextLeft  := left  + 1
    let nextRight := right - 1

    match fileArray.get? right with
    | .some (.file fileId fileSize)  =>
      -- keep searching for file
      if id != fileId then
        fileArray.condense! id left nextRight

      -- found the file
      else match fileArray.get? left with
        | .some (.empty emptySize) =>
          -- found suitable empty space
          if emptySize >= fileSize then
            -- let fileArray := fileArray.map (#[.])
            --   |>.set! left #[.file fileId fileSize, .empty (emptySize - fileSize)]
            --   |>.set! right #[.empty fileSize]
            --   |>.flatten
            --   |> FileArray.mergeEmpty

            fileArray.condense! (id - 1) 0 (fileArray.size - 1)
          else
            fileArray.condense! id nextLeft right
        | .some _ =>
          -- keep searching for empty
          fileArray.condense! id nextLeft right

        -- cannot hit?
        | _ => panic! "Should not be able to get here 2"

    -- keep searching for file
    | .some _ =>
      fileArray.condense! id left nextRight

    -- cannot hit?
    | _ => panic! "Should not be able to get here"

/--
Turn
  `00...111...2...333.44.5555.6666.777.888899`
into
  `00992111777.44.333....5555.6666.....8888..`
-/
def FileArray.condense (fileArray : FileArray) : IO FileArray := do
  let maxId := fileArray.foldl (init := 0) (λ
    | m, .file id _ => max id m
    | m, .empty _   => m
  )

  return fileArray.condense! maxId 0 (fileArray.size - 1)

partial def DiskArray.condense! (diskArray : DiskArray) (left right : Nat) : DiskArray :=
  if left >= right then
    diskArray
  else
    let nextLeft  := left  + 1
    let nextRight := right - 1

    match diskArray.get? left, diskArray.get? right with
    -- skip front
    | .some (.file _), .some _ =>
      diskArray.condense! nextLeft right

    -- skip back
    | .some .empty, .some .empty =>
      diskArray.condense! left nextRight

    -- swap
    | .some .empty, .some (.file id) =>
      condense!
        (diskArray.set! left (.file id) |>.set! right .empty)
        nextLeft nextRight

    -- cannot hit?
    | _, _ => panic! "Should not be able to get here"

/--
Turn
  `00...111...2...333.44.5555.6666.777.888899`
into
  `0099811188827773336446555566..............`
-/
def DiskArray.condense (diskArray : DiskArray) := diskArray.condense! 0 (diskArray.size - 1)

def DiskArray.checksum (diskArray : DiskArray) := diskArray.toList.enum
  |>.map (λ (i, b) => match b with
    | .file id => i * id
    | _ => 0
  )
  |>.sum

def FileArray.intoDiskArray (fileArray : FileArray) : DiskArray := fileArray.toList
  |>.flatMap ( λ
    | .empty size => List.range size |>.map (λ _ => .empty)
    | .file id size => List.range size |>.map (λ _ => .file id)
  )
  |>.toArray

def DiskArray.fromString (string : String) : DiskArray := FileArray.fromString string |>.intoDiskArray

def FileArray.checksum (fileArray : FileArray) := fileArray.intoDiskArray.checksum

-- Main

def main := aocMain λ file => do
  let diskArray := DiskArray.fromString file.content
  IO.println s!"Part 1: {diskArray.condense.checksum}"

  let fileArray := FileArray.fromString file.content
  IO.println s!"Part 1: {(<- fileArray.condense).checksum}"
