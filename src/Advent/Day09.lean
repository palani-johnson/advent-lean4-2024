import Std
import Utils

-- Types

inductive FileBlock where
  | empty (size : Nat)
  | file (id : Nat) (size : Nat)

def FileArray := Array FileBlock

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

/--
Turn
  `00...111...2...333.44.5555.6666.777.888899`
into
  `00992111777.44.333....5555.6666.....8888..`
-/
def FileArray.condense (fileArray : FileArray) : FileArray := fileArra

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
  IO.println s!"Solution for {file.path}:"

  let diskArray := DiskArray.fromString file.content
  IO.println s!"Part 1: {diskArray.condense.checksum}"

  let fileArray := FileArray.fromString file.content
  IO.println s!"Part 1: {fileArray.checksum}"
