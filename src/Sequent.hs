{-# LANGUAGE OverloadedStrings #-}
module Sequent where

-- Tensor, Par, With, Plus, Top, Unit, Bottom, Void
-- * | & + ^ () {}

import Data.Maybe (
  listToMaybe, maybeToList)
import Control.Monad (
  guard, mplus)


data Statement =
  (:=) Binder Value
    deriving (Show)

data Binder =
  Bind Variable |
  Match Variable Variable |
  Copy Variable Variable |
  Force Value
    deriving (Show)

data Value =
  Use Variable |
  Pair Variable Variable |
  Share Variable Variable |
  Thunk Binder
    deriving (Show)

type Variable = String


substituteStep :: [Statement] -> Maybe [Statement]
substituteStep statements = listToMaybe (do
  (statement1, statements1) <- select statements
  (statement2, statements2) <- select statements1
  statement' <- maybeToList (substitute statement1 statement2)
  return ([statement'] ++ statements2))


substitute :: Statement -> Statement -> Maybe Statement
substitute (t1 := Use x1) (Bind x2 := t2) = guard (x1 == x2) >> (Just (t1 := t2))
substitute _ _ = Nothing

actStep :: [Statement] -> Maybe [Statement]
actStep statements = listToMaybe (do
  (statement1, statements1) <- select statements
  statements' <- maybeToList (act statement1)
  return (statements' ++ statements1))

act :: Statement -> Maybe [Statement]
act (Match x1 x2 := Pair p1 p2) = Just [Bind x1 := Use p1, Bind x2 := Use p2]
act (Force v1 := Thunk x2) = Just [x2 := v1]
act _ = Nothing


select :: [a] -> [(a, [a])]
select [] = []
select (a : as) = [(a, as)] ++ map (\(b, bs) -> (b, a : bs)) (select as)


evaluate :: [Statement] -> [Statement]
evaluate statements = case mplus (substituteStep statements) (actStep statements) of
  Nothing -> statements
  Just statements' -> evaluate statements'


test :: [Statement]
test = [
  Bind "i" := Thunk (Match "x" "r"),
  Force (Use "x") := (Use "r"),
  Bind "o" := Thunk (Bind "hi"),
  Bind "p" := Pair "5" "o",
  Force (Use "p") := Use "i"]


test' :: [Statement]
test' = [
  Bind "o" := Use "5"]



