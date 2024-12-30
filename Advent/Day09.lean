import Std
import Advent.Utils

open Std.Internal.Parsec.String


-- def main (args : List String) : IO Unit := do
--   match args with
--   | [] => return
--   | filePath :: rest =>
--     let fileContent <- IO.FS.readFile filePath
--     let parseResult := inputParser.run fileContent.trim

--     match parseResult with
--     | .error _ =>
--       IO.eprintln s!"Failed to parse {filePath}"
--     | .ok problemInput =>
--       IO.println s!"Solution for {filePath}:"
--       IO.println s!"Part 1: {problemInput |> solve1}"
--       IO.println s!"Part 2: {problemInput |> solve2}"


--     main rest
