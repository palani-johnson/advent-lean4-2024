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

def ordered (orderingRules : OrderingRules) (fst : Int) (snd : Int) :=
    orderingRules.all ( λ (a, b) => ¬(snd == a ∧ fst == b))

def isOrdered (orderingRules : OrderingRules) (pageNums : List Int) :=  match pageNums with
  | [] => true
  | head :: tail => (tail.all (ordered orderingRules head ·)) ∧ (isOrdered orderingRules tail)


def solve1 (orderingRules : OrderingRules) (pageNumbers : PageNumbers) :=
  pageNumbers.map (λ pageNums =>
    if isOrdered orderingRules pageNums then
      pageNums.get! (pageNums.length / 2)
    else
      0
  )
  |>.sum

def solve2 (orderingRules : OrderingRules) (pageNumbers : PageNumbers) :=
  let rec quickSort (pageNums : List Int) : List Int := match pageNums with
    | [] => []
    | head :: rest =>
      let lesser := rest.filter (λ e => ordered orderingRules head e)
      let greater := rest.filter (λ e => not <| ordered orderingRules head e)

      (quickSort lesser) ++ [head] ++ (quickSort greater)
  termination_by pageNums.length
  decreasing_by
    all_goals simp_wf
    . sorry
    . sorry


  pageNumbers.map (λ pageNums =>
    if not (isOrdered orderingRules pageNums) then
      let pageNums := quickSort pageNums
      pageNums.get! (pageNums.length / 2)
    else
      0
  )
  |>.sum

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
