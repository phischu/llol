{-# LANGUAGE OverloadedStrings #-}
module Sequent where

-- Tensor, Par, With, Plus, Top, Unit, Bottom, Void
-- * | & + ^ () {}

import Data.Maybe (
  listToMaybe, maybeToList)
import Control.Monad (
  guard, mplus, msum)


-- Actually we have four (three) kinds of statements. Perhaps make that explicit?
-- this would also prevent (Force (Thunk (Force (...))))
data Statement =
  (:=) Binder Value
    deriving (Show, Eq, Ord)

data Binder =
  Bind Variable |
  Match Variable Variable |
  Copy Variable Variable |
  Force Value
    deriving (Show, Eq, Ord)

data Value =
  Use Variable |
  Pair Variable Variable |
  Share Variable Variable |
  Thunk Binder
    deriving (Show, Eq, Ord)

type Variable = String


substituteStep :: [Statement] -> Maybe [Statement]
substituteStep statements = listToMaybe (do
  (statement1, statements1) <- select statements
  (statement2, statements2) <- select statements1
  statement' <- maybeToList (
    substitute statement1 statement2 `mplus` substitute statement2 statement1)
  return ([statement'] ++ statements2))


substitute :: Statement -> Statement -> Maybe Statement
substitute statement1 statement2 = msum [
  substituteNormal statement1 statement2,
  substituteThunk statement1 statement2,
  substituteForce statement1 statement2]

substituteNormal :: Statement -> Statement -> Maybe Statement
substituteNormal (t1 := Use x1) (Bind x2 := t2) =
  guard (x1 == x2) >> (Just (t1 := t2))
substituteNormal _ _ =
  Nothing

substituteThunk :: Statement -> Statement -> Maybe Statement
substituteThunk (t1 := Use x1) (t2 := Thunk (Bind x2)) =
  guard (x1 == x2) >> (Just (t1 := Thunk t2))
substituteThunk _ _ =
  Nothing

substituteForce :: Statement -> Statement -> Maybe Statement
substituteForce (Force (Use x1) := t1) (Bind x2 := t2) =
  guard (x1 == x2) >> (Just (Force t1 := t2))
substituteForce _ _ =
  Nothing

actStep :: [Statement] -> Maybe [Statement]
actStep statements = listToMaybe (do
  (statement1, statements1) <- select statements
  statements' <- maybeToList (act statement1)
  return (statements' ++ statements1))

act :: Statement -> Maybe [Statement]
act (Match x1 x2 := Pair p1 p2) = Just [
  Bind x1 := Use p1,
  Bind x2 := Use p2]
act (Copy x1 x2 := Share s1 s2) = Just [
  Bind x1 := Use s1,
  Bind x2 := Use s2]
act (Force v1 := Thunk x2) = Just [
  x2 := v1]
act (Copy a b := Pair x y) = Just [
  Copy x1 x2 := Use x,
  Copy y1 y2 := Use y,
  Bind a := Pair x1 y1,
  Bind b := Pair x2 y2]
    where (x1, x2) = tick x ; (y1, y2) = tick y
act (Match x y := Share a b) = Just [
  Match a1 a2 := Use a,
  Match b1 b2 := Use b,
  Bind x := Share a1 b1,
  Bind y := Share a2 b2]
    where (a1, a2) = tick a ; (b1, b2) = tick b
act (Force c := Pair a1 a2) = Just [
  Match a1' a2' := c,
  Force (Use a1') := Use a1,
  Force (Use a2') := Use a2]
    where (a1', a2') = (prime a1, prime a2)
act (Force c := Share a1 a2) = Just [
  Copy a1' a2' := c,
  Force (Use a1') := Use a1,
  Force (Use a2') := Use a2]
    where (a1', a2') = (prime a1, prime a2)
act (Match x1 x2 := Thunk a) = Just [
   a := Pair x1' x2',
   Bind x1 := Thunk (Bind x1'),
   Bind x2 := Thunk (Bind x2')]
     where (x1', x2') = (prime x1, prime x2)
act (Copy x1 x2 := Thunk a) = Just [
   a := Share x1' x2',
   Bind x1 := Thunk (Bind x1'),
   Bind x2 := Thunk (Bind x2')]
     where (x1', x2') = (prime x1, prime x2)
act (Bind _ := _) = Nothing
act (_ := Use _) = Nothing

tick :: Variable -> (Variable, Variable)
tick x = (x ++ "1", x ++ "2")

prime :: Variable -> Variable
prime x = x ++ "'"

select :: [a] -> [(a, [a])]
select [] = []
select (a : as) = [(a, as)] ++ map (\(b, bs) -> (b, a : bs)) (select as)


evaluate :: [Statement] -> [Statement]
evaluate statements = case mplus (substituteStep statements) (actStep statements) of
  Nothing -> statements
  Just statements' -> evaluate statements'


