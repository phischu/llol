module Sequent where

-- Tensor, Par, With, Plus, Top, Unit, Bottom, Void
-- * | & + ^ () {}
{-
import Data.Maybe (
  listToMaybe, maybeToList)
import Control.Monad (
  guard)


data Equation = Equation LHS RHS

data LHS =
  Bind Variable |
  Match Variable Variable |
  Copy Variable Variable |
  Force Variable

data RHS =
  Use Variable |
  Pair Variable Variable |
  Share Variable Variable |
  Thunk Variable

data Agent =
  LeftAgent Variable RHS |
  RightAgent Variable LHS

type Variable = String


type Statement = Equation


step :: [Statement] -> Maybe [Statement]
step statements = listToMaybe (do
  (statement1, statements1) <- select statements
  (statement2, statements2) <- select statements1
  statements' <- maybeToList (act statement1 statement2)
  return (statements' ++ statements2))


act :: Statement -> Statement -> Maybe [Statement]
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

-}
