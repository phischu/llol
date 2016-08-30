module Main where

import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)

-- Tensor, Par, With, Plus, Top, Unit, Bottom, Void
-- * | & + ^ () {}

type Variable =
  String

data Statement = (:=) LHS RHS deriving (Show, Eq, Ord)

data LHS =
  Bind Variable |
  Match Variable Variable |
  Copy Variable Variable
    deriving (Show, Eq, Ord)

data RHS =
  Use Variable |
  Pair Variable Variable |
  Share Variable Variable
    deriving (Show, Eq, Ord)

type Program = [Statement]


data Cell =
  Wire Variable Variable |
  Construct Variable Variable Variable |
  Duplicate Variable Variable Variable
    deriving (Show, Eq, Ord)

type InteractionNet = [Cell]


act :: Cell -> Cell -> [Cell]
act (Wire _ y) (Wire _ z) = [Wire y z]
act (Wire _ y) (Construct _ z w) = [Construct y z w]
act (Wire _ y) (Duplicate _ z w) = [Duplicate y z w]
act (Construct _ a b) (Wire _ w) = [Construct w a b]
act (Construct _ a b) (Construct _ x y) = [Wire a x, Wire b y]
act (Construct _ a b) (Duplicate _ x y) = [
  Duplicate a a1 a2, Duplicate b b1 b2,
  Construct x a1 b1, Construct y a2 b2] where
    (a1, a2) = tick a
    (b1, b2) = tick b
act (Duplicate _ y z) (Wire _ w) = [Duplicate w y z]
act (Duplicate _ y z) (Construct _ a b) = [
  Duplicate a a1 a2, Duplicate b b1 b2,
  Construct y a1 b1, Construct z a2 b2] where
    (a1, a2) = tick a
    (b1, b2) = tick b
act (Duplicate _ y z) (Duplicate _ a b) = [Wire y a, Wire z b]

reduce :: [Cell] -> [Cell]
reduce [] = error "reduce empty list"
reduce [cell] = [cell]
reduce [cell1, cell2] = act cell1 cell2
reduce _ = error "reduce long list"

step :: [Cell] -> [Cell]
step = concatMap reduce . groupByPrincipalPort

groupByPrincipalPort :: [Cell] -> [[Cell]]
groupByPrincipalPort =
  groupBy ((==) `on` principalPort) . sortBy (comparing principalPort)

principalPort :: Cell -> Variable
principalPort (Wire x _) = x
principalPort (Construct x _ _) = x
principalPort (Duplicate x _ _) = x

evaluate :: InteractionNet -> InteractionNet
evaluate = step

tick :: Variable -> (Variable, Variable)
tick x = (x ++ "1", x ++ "2")

test :: InteractionNet
test = [
  Construct "p" "p1" "p2",
  Construct "p" "a1" "a2"]

test' :: InteractionNet
test' = [
  Wire "p1" "a1",
  Wire "p2" "a2"]

main :: IO ()
main = do
  print (evaluate test)
  print (evaluate test == test')

