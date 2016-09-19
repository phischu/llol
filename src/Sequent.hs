module Sequent where

-- Tensor, Par, With, Plus, Top, Unit, Bottom, Void
-- * | & + ^ () {}


data Statement v =
  Wire v v |
  Pair v v v |
  Match v v v |
  Copy v v v |
  Share v v v |
  Open v v |
  Close v v

test :: [Statement String]
test = [
  Open "t" "i",
  Match "x" "c" "i",
  Close "x" "c",
  Pair "p" "5" "r",
  Close "p" "t"]

test' :: [Statement String]
test' = [
  Wire "5" "r"]

