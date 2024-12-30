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
def FileArray.condense (fileArray : FileArray) : FileArray := Id.run do
  let mut frontPtr := 0
  let mut backPtr := fileArray.size - 1
  let mut fileArray := fileArray

  fileArray

/--
Turn
  `00...111...2...333.44.5555.6666.777.888899`
into
  `0099811188827773336446555566..............`
-/
def DiskArray.condense (diskArray : DiskArray) : DiskArray := Id.run do
  let mut frontPtr := 0
  let mut backPtr := diskArray.size - 1
  let mut diskArray := diskArray

  while frontPtr < backPtr do
    match diskArray.get? frontPtr, diskArray.get? backPtr with
    -- skip front
    | .some (.file _), .some _ =>
      frontPtr := frontPtr + 1

    -- skip back
    | .some .empty, .some .empty =>
      backPtr := backPtr - 1

    -- swap
    | .some .empty, .some (.file id) =>
      diskArray := (diskArray.set! frontPtr (.file id)).set! backPtr .empty
      frontPtr := frontPtr + 1
      backPtr := backPtr - 1

    -- cannot hit?
    | _, _ => break

  diskArray

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

def main (args : List String) : IO Unit := do
  match args with
  | [] => return
  | filePath :: rest =>
    let fileContent <- IO.FS.readFile filePath
    IO.println s!"Solution for {filePath}:"

    let diskArray := DiskArray.fromString fileContent
    IO.println s!"Part 1: {diskArray.condense.checksum}"

    let fileArray := FileArray.fromString fileContent
    IO.println s!"Part 1: {fileArray.checksum}"

    main rest
