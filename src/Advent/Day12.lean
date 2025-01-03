import Std
import Utils

-- Types

inductive Plot
  | empty
  | region (type : Char)

-- Functions



-- Main

def main := aocMain λ file => do
  let garden := file.content.splitOn "\n"
    |>.map λ line => line.data.map (Plot.region ·)
