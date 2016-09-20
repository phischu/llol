module InteractionNet where

import Sequent ()

import Data.Map (Map)
import qualified Data.Map as Map (
  empty, lookup, insert, delete)
import Control.Monad.Trans.State (
  State, execState, get, put)


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

type Evaluator = State (Environment, Map Variable Variable)


lookupVariable :: Variable -> Evaluator (Maybe Term)
lookupVariable variable = do
  (environment, backlinks) <- get
  case Map.lookup variable environment of
    Nothing -> do
      case Map.lookup variable backlinks of
        Nothing -> do
          return Nothing
        Just variable2 -> do
          put (Map.delete variable2 environment, Map.delete variable backlinks)
          return (Just (Variable variable2))
    Just (Variable variable2) -> do
      put (Map.delete variable environment, Map.delete variable2 backlinks)
      return (Just (Variable variable2))
    Just term -> do
      put (Map.delete variable environment, backlinks)
      return (Just term)


insertVariable :: Variable -> Term -> Evaluator ()
insertVariable variable1 (Variable variable2) = do
  (environment, backlinks) <- get
  let environment' = Map.insert variable1 (Variable variable2) environment
      backlinks' = Map.insert variable2 variable1 backlinks
  put (environment', backlinks')
insertVariable variable term = do
  (environment, backlinks) <- get
  let environment' = Map.insert variable term environment
  put (environment', backlinks)


execEvaluator :: Evaluator a -> Environment
execEvaluator evaluator = fst (execState evaluator (Map.empty, Map.empty))


evaluate :: InteractionNet -> Evaluator ()
evaluate [] =
  return ()
evaluate (statement : statements) = case statement of

  Variable variable1 := Variable variable2 -> do
    maybeTerm1 <- lookupVariable variable1
    maybeTerm2 <- lookupVariable variable2
    case (maybeTerm1, maybeTerm2) of
      (Nothing, Nothing) -> do
        insertVariable variable1 (Variable variable2)
        evaluate statements
      (Just term1, Nothing) -> do
        evaluate (term1 := Variable variable2 : statements)
      (Nothing, Just term2) -> do
        evaluate (Variable variable1 := term2 : statements)
      (Just term1, Just term2) -> do
        evaluate (term1 := term2 : statements)

  Variable variable1 := term2 -> do
    maybeTerm1 <- lookupVariable variable1
    case maybeTerm1 of
      Nothing -> do
        insertVariable variable1 term2
        evaluate statements
      Just term1 -> do
        evaluate (term1 := term2 : statements)

  term1 := Variable variable2 -> do
    maybeTerm2 <- lookupVariable variable2
    case maybeTerm2 of
      Nothing -> do
        insertVariable variable2 term1
        evaluate statements
      Just term2 -> do
        evaluate (term1 := term2 : statements)

  term1 := term2 -> do
    evaluate (act term1 term2 ++ statements)


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

