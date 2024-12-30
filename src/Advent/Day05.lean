import Std
import Utils

-- Types

def OrderingRules := List (Int × Int)
def PageNumbers := List (List Int)

-- Parsing

section
  open Std.Internal.Parsec.String

  def orderingRuleParser : Parser (Int × Int) := do
    let a <- digits
    let _ <- pchar '|'
    let b <- digits
    return (a, b)

  def rulesParser : Parser OrderingRules := sepBy orderingRuleParser (skipChar '\n')

  def pageNumbersParser : Parser PageNumbers := sepBy (sepBy digits (skipChar ',')) (skipChar '\n')
end

-- Functions

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

section
  def solve2 (orderingRules : OrderingRules) (pageNumbers : PageNumbers) :=
    -- TODO: use mergesort
    let rec quickSort : List Int → List Int
    | [] => []
    | head :: rest =>
      let lesser := rest.filter (ordered orderingRules head ·)
      let greater := rest.filter (not <| ordered orderingRules head ·)

      (quickSort lesser) ++ [head] ++ (quickSort greater)
    termination_by l => l.length
    decreasing_by
      all_goals simp
      . sorry
      . sorry




    pageNumbers.map (λ pageNums =>
      if not (isOrdered orderingRules pageNums) then

        let pageNums := pageNums.mergeSort
        pageNums.get! (pageNums.length / 2)
      else
        0
    )
    |>.sum
end
-- Main

def main := aocMain λ file => do
    if let [orderingRulesString, pageNumbersString] := file.content |>.splitOn "\n\n" then

    let parsedOrderingRules := rulesParser.run orderingRulesString
    let parsedPageNumbers := pageNumbersParser.run pageNumbersString

    match parsedOrderingRules, parsedPageNumbers with
    | .ok orderingRules, .ok pageNumbers =>
      IO.println s!"Solution for {file.path}:"
      IO.println s!"Part 1: {solve1 orderingRules pageNumbers}"
      IO.println s!"Part 2: {solve2 orderingRules pageNumbers}"
    | _, _ =>
      IO.eprintln s!"Failed to parse {file.path}"
