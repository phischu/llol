module Sequent where

-- Tensor, Par, With, Plus, Top, Unit, Bottom, Void
-- * | & + ^ () {}


data LHS v =
  Bind v |
  Match v v |
  Copy v v |
  Open (RHS v)

data RHS v =
  Use v |
  Pair v v |
  Share v v |
  Close (LHS v)

data Statement = (:=) (LHS String) (RHS String)

data Let = Let [Statement] (RHS String)
data Com = Com (LHS String) [Statement]

test :: [Statement]
test = [
  Open (Use "x") := Use "c",
  Bind "i" := Close (Match "x" "c"),
  Open (Pair "5" "rc") := Use "i",
  Bind "return" := Close (Bind "rc")]

test' :: [Statement]
test' = [
  Bind "return" := Use "5"]

