module Main where

import Sequent ()

import Data.Maybe (listToMaybe)
import Control.Monad (guard)

type Variable = String

data Cell =
  Wire Variable Variable |
  Construct Variable Variable Variable |
  Duplicate Variable Variable Variable
    deriving (Show, Eq, Ord)

-- TODO: Constant Variable
-- TODO: Delete Variable
-- TODO: Duplicate Int Variable Variable Variable

-- TODO: Name generation

type InteractionNet = [Cell]

select :: [a] -> [(a, [a])]
select [] = []
select (a : as) = [(a, as)] ++ map (\(b, bs) -> (b, a : bs)) (select as)

step :: [Cell] -> Maybe [Cell]
step cells = listToMaybe (do
  (cell1, cells1) <- select cells
  (cell2, cells2) <- select cells1
  newCells <- act cell1 cell2
  return (newCells ++ cells2))

act :: Cell -> Cell -> [[Cell]]
act (Wire i' j') (Wire k' l') = do
  (i, j) <- trySwap (i', j')
  (k, l) <- trySwap (k', l')
  guard (j == k)
  return [Wire i l]
act (Wire i' j') (Construct x y z) = do
  (i, j) <- trySwap (i', j')
  guard (i == x)
  return [Construct j y z]
act (Wire i' j') (Duplicate a b c) = do
  (i, j) <- trySwap (i', j')
  guard (i == a)
  return [Duplicate j b c]
act (Construct x y z) (Wire i' j') = do
  (i, j) <- trySwap (i', j')
  guard (i == x)
  return [Construct j y z]
act (Construct a b c) (Construct x y z) = do
  guard (a == x)
  return [Wire b y, Wire c z]
act (Construct a b c) (Duplicate x y z) = do
  guard (a == x)
  let (b1, b2) = tick b
      (c1, c2) = tick c
  return [Duplicate b b1 b2, Duplicate c c1 c2, Construct y b1 c1, Construct z b2 c2]
act (Duplicate x y z) (Wire i' j') = do
  (i, j) <- trySwap (i', j')
  guard (x == i)
  return [Duplicate j y z]
act (Duplicate a b c) (Construct x y z) = do
  guard (a == x)
  let (b1, b2) = tick b
      (c1, c2) = tick c
  return [Construct b b1 b2, Construct c c1 c2, Duplicate y b1 c1, Duplicate z b2 c2]
act (Duplicate a b c) (Duplicate x y z) = do
  guard (a == x)
  return [Wire b y, Wire c z]

trySwap :: (a, a) -> [(a, a)]
trySwap (x, y) = [(x, y), (y, x)]

evaluate :: InteractionNet -> InteractionNet
evaluate net = case step net of
  Nothing -> net
  Just net' -> evaluate net'

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

