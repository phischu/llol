module Main where

import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)
import Control.Applicative (empty)

-- Tensor, Par, With, Plus, Top, Unit, Bottom, Void
-- * | & + ^ () {}

type Variable = String

data Cell =
  Wire Variable Variable |
  Construct Variable Variable Variable |
  Duplicate Variable Variable Variable
    deriving (Show, Eq, Ord)

-- TODO: Delete Variable
-- TODO: Duplicate Int Variable Variable Variable

type InteractionNet = [Cell]

findActiveWires :: [Cell] -> [(Variable, Cell)]
findActiveWires wires = do
  Wire x y <- wires
  Wire a b <- wires
  if (x == a) then (return (x, Wire y b)) else empty

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
step = map flipWire . concatMap reduce . groupByPrincipalPort . substituteWires

substituteWires :: [Cell] -> [Cell]
substituteWires [] = []
substituteWires (Wire x y : cells) = substituteWire x y cells
substituteWires (cell : cells) = cell : substituteWires cells

substituteWire :: Variable -> Variable -> [Cell] -> [Cell]
substituteWire x y (Wire a b : cells)
  | x == b = Wire a y : cells
  | otherwise = Wire a b : substituteWire x y cells
substituteWire x y (cell : cells) =
  cell : substituteWire x y cells
substituteWire x y [] =
  Wire x y : []

groupByPrincipalPort :: [Cell] -> [[Cell]]
groupByPrincipalPort =
  groupBy ((==) `on` principalPort) . sortBy (comparing principalPort)

principalPort :: Cell -> Variable
principalPort (Wire x _) = x
principalPort (Construct x _ _) = x
principalPort (Duplicate x _ _) = x

flipWire :: Cell -> Cell
flipWire (Wire x y) = Wire y x
flipWire cell = cell

evaluate :: InteractionNet -> InteractionNet
evaluate = steps . steps . steps . steps where
  steps = step . step . step . step . step . step . step . step

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

twice :: InteractionNet
twice = [
  Construct "twice" "f" "ff",
  Duplicate "f" "f1" "f2",
  Construct "f1" "f1x" "f1r",
  Construct "f2" "f2x" "f2r",
  Wire "f1r" "f2x",
  Construct "ff" "f1x" "f2r"]

testTwice :: InteractionNet
testTwice = [
  Construct "i" "x" "r",
  Wire "x" "r",
  Construct "twice" "i" "test"] ++ twice

testTwice' :: InteractionNet
testTwice' = [
  Construct "test" "x" "r",
  Wire "x" "r"]

main :: IO ()
main = do
  print (evaluate testTwice)
  print (evaluate testTwice == testTwice')

