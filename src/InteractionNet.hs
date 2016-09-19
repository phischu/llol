module InteractionNet where

import Sequent ()

import Data.Map (Map)
import qualified Data.Map as Map (
  empty, lookup, insert, delete)

import Debug.Trace

type Variable = String


data Term =
  Variable Variable |
  Construct Variable Variable |
  Duplicate Variable Variable
    deriving (Show, Eq, Ord)


data Statement =
  (:=) Term Term
    deriving (Show, Eq, Ord)

-- TODO: Constant Variable
-- TODO: Delete Variable
-- TODO: Duplicate Int Variable Variable Variable

-- TODO: Name generation

type Environment = Map Variable Term

type InteractionNet = [Statement]

emptyEnvironment :: Environment
emptyEnvironment = Map.empty

evaluate :: Environment -> InteractionNet -> Environment
evaluate environment [] =
  environment
evaluate environment (statement : statements) =
  traceShow statement $ case statement of
    (Variable variable1) := (Variable variable2) ->
      evaluate environment' statements' where
        environment' = case Map.lookup variable1 environment of
          Nothing -> case Map.lookup variable2 environment of
            Nothing -> Map.insert variable2 (Variable variable1) environment
            Just _ -> Map.delete variable2 environment
          Just _ -> case Map.lookup variable2 environment of
            Nothing -> Map.delete variable1 environment
            Just _ -> Map.delete variable1 (Map.delete variable2 environment)
        statements' = case Map.lookup variable1 environment of
          Nothing -> case Map.lookup variable2 environment of
            Nothing -> statements
            Just term2 -> [Variable variable1 := term2] ++ statements
          Just term1 -> case Map.lookup variable2 environment of
            Nothing -> [term1 := Variable variable2] ++ statements
            Just term2 -> [term1 := term2] ++ statements
    (Variable variable) := term ->
      evaluate environment' statements' where
        environment' = case Map.lookup variable environment of
          Nothing -> Map.insert variable term environment
          Just _ -> Map.delete variable environment
        statements' = case Map.lookup variable environment of
          Nothing -> statements
          Just term' -> [term' := term] ++ statements
    term := (Variable variable) ->
      evaluate environment' statements' where
        environment' = case Map.lookup variable environment of
          Nothing -> Map.insert variable term environment
          Just _ -> Map.delete variable environment
        statements' = case Map.lookup variable environment of
          Nothing -> statements
          Just term' -> [term := term'] ++ statements
    term1 := term2 ->
      evaluate environment (act term1 term2 ++ statements)

act :: Term -> Term -> [Statement]
act (Construct a b) (Construct x y) = [
  (Variable a) := (Variable x),
  (Variable b) := (Variable y)]
act (Duplicate a b) (Duplicate x y) = [
  (Variable a) := (Variable x),
  (Variable b) := (Variable y)]
act (Construct a b) (Duplicate x y) = [
  (Variable a) := (Duplicate a1 a2),
  (Variable b) := (Duplicate b1 b2),
  (Construct a1 b1) := (Variable x),
  (Construct a2 b2) := (Variable y)] where
    (a1, a2) = tick a
    (b1, b2) = tick b
act (Duplicate x y) (Construct a b) = [
  (Variable a) := (Duplicate a1 a2),
  (Variable b) := (Duplicate b1 b2),
  (Construct a1 b1) := (Variable x),
  (Construct a2 b2) := (Variable y)] where
    (a1, a2) = tick a
    (b1, b2) = tick b
act _ _ = error "Acting on variable"

tick :: Variable -> (Variable, Variable)
tick x = (x ++ "1", x ++ "2")

