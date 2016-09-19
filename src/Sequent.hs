module Sequent where

-- Tensor, Par, With, Plus, Top, Unit, Bottom, Void
-- * | & + ^ () {}

import Data.Maybe (
  listToMaybe, maybeToList)
import Control.Monad (
  guard)


data Statement =
  Wire Variable Variable |
  Introduction Introduction |
  Elimination Elimination

-- TODO: Remove wire (wire is variable)

data Introduction =
  Pair Variable Variable Variable |
  Copy Variable Variable Variable |
  Open Variable Variable |
  Input Variable

data Elimination =
  Match Variable Variable Variable |
  Share Variable Variable Variable |
  Close Variable Variable |
  Output Variable

type Variable = String

step :: [Statement] -> Maybe [Statement]
step statements = listToMaybe (do
  (statement1, statements1) <- select statements
  (statement2, statements2) <- select statements1
  statements' <- maybeToList (act statement1 statement2)
  return (statements' ++ statements2))


act :: Statement -> Statement -> Maybe [Statement]
act (Introduction introduction) (Elimination elimination) =
  case (introduction, elimination) of
    (Pair p p1 p2, Match m1 m2 m) -> do
      guard (p == m)
      return [Wire p1 m2, Wire p2 m2]
act _ _ = Nothing


select :: [a] -> [(a, [a])]
select [] = []
select (a : as) = [(a, as)] ++ map (\(b, bs) -> (b, a : bs)) (select as)


evaluate :: [Statement] -> [Statement]
evaluate statements = case step statements of
  Nothing -> statements
  Just statements' -> evaluate statements'


input x = Introduction (Input x)
open x y = Introduction (Open x y)
match x y z = Elimination (Match x y z)
close x y = Elimination (Close x y)
pair x y z = Introduction (Pair x y z)
output x = Elimination (Output x)
wire x y = Wire x y


test :: [Statement]
test = [
  input "5",
  open "t" "i",
  match "x" "c" "i",
  close "x" "c",
  pair "p" "5" "r",
  close "p" "t",
  output "r"]


test' :: [Statement]
test' = [
  input "5",
  wire "5" "r",
  output "r"]

