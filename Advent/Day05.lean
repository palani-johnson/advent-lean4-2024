import Std
import Advent.Utils

open Std.Internal.Parsec.String

def OrderingRules := List (Int × Int)
def PageNumbers := List (List Int)

def orderingRuleParser : Parser (Int × Int) := do
  let a ← digits
  let _ ← pchar '|'
  let b ← digits
  return (a, b)

def rulesParser : Parser OrderingRules := sepBy orderingRuleParser (skipChar '\n')

def pageNumbersParser : Parser PageNumbers := sepBy (sepBy digits (skipChar ',')) (skipChar '\n')

def solve1 (orderingRules : OrderingRules) (pageNumbers : PageNumbers) :=
  NotImplemented

def solve2 (orderingRules : OrderingRules) (pageNumbers : PageNumbers) :=
  NotImplemented

def main (args : List String) : IO Unit := do
  match args with
  | [] => return
  | filePath :: rest =>
    let fileContent <- IO.FS.readFile filePath
    if let [orderingRulesString, pageNumbersString] := fileContent.trim |>.splitOn "\n\n" then

    let parsedOrderingRules := rulesParser.run orderingRulesString
    let parsedPageNumbers := pageNumbersParser.run pageNumbersString

    match parsedOrderingRules, parsedPageNumbers with
    | .ok orderingRules, .ok pageNumbers =>
      String.intercalate "\n  " [
        s!"Solution for {filePath}:",
        s!"Part 1: {solve1 orderingRules pageNumbers}",
        s!"Part 2: {solve2 orderingRules pageNumbers}"
      ] |> IO.println
    | _, _ => IO.eprintln s!"Failed to parse {filePath}"

    main rest
