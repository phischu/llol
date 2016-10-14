module Lambda where

import InteractionNet (
  InteractionNet, Statement(..), Term(..), Variable)

import Morte.Core (
  Expr(Var, Lam, App), X,
  pretty)

import Control.Monad.Trans.State (
  State, execState, get, put, modify)
import Data.Map (
  Map)
import qualified Data.Map as Map (
  empty, findWithDefault, delete, insert)
import Data.Text.Lazy (
  unpack)


morteNet :: Variable -> Expr X -> InteractionNet
morteNet name expression =
  runGenerator (morteNetForward name expression)

type Generator = State (InteractionNet, Integer, Map Variable Integer)

runGenerator :: Generator () -> InteractionNet
runGenerator generator = case (execState generator ([], 0, Map.empty)) of
  (net, _, _) -> net

tellNet :: InteractionNet -> Generator ()
tellNet net = do
  modify (\(net', i, occurences) -> (net' ++ net, i, occurences))

freshContinuation :: Generator Variable
freshContinuation = do
  (net, i, occurences) <- get
  put (net, (i + 1), occurences)
  return ("r" ++ show i)

freshVariableOccurence :: Variable -> Generator Variable
freshVariableOccurence variable = do
  (net, i, occurences) <- get
  let n = Map.findWithDefault 0 variable occurences
  put (net, i, Map.insert variable (n + 1) occurences)
  return (numbered variable (n + 1))

replace :: Variable -> Variable -> InteractionNet -> InteractionNet
replace variable1 variable2 statements =
  map (replaceInStatement variable1 variable2) statements

replaceInStatement :: Variable -> Variable -> Statement -> Statement
replaceInStatement variable1 variable2 (term1 := term2) =
  replaceInTerm variable1 variable2 term1 := replaceInTerm variable1 variable2 term2

replaceInTerm :: Variable -> Variable -> Term -> Term
replaceInTerm variable1 variable2 (Variable variable)
  | variable == variable1 = Variable variable2
  | otherwise = Variable variable
replaceInTerm variable1 variable2 (Construct variableA variableB)
  | variableA == variable1 = Construct variable2 variableB
  | variableB == variable1 = Construct variableA variable2
  | otherwise = Construct variableA variableB
replaceInTerm variable1 variable2 (Duplicate variableA variableB)
  | variableA == variable1 = Duplicate variable2 variableB
  | variableB == variable1 = Duplicate variableA variable2
  | otherwise = Duplicate variableA variableB


numbered :: Variable -> Integer -> Variable
numbered variable n = variable ++ show n

establishSharing :: Variable -> Generator ()
establishSharing variable = do
  (net, i, occurences) <- get
  case Map.findWithDefault 0 variable occurences of
        0 -> error "variable not used"
        1 -> do
          let net' = replace (numbered variable 1) variable net
          put (net', i, Map.delete variable occurences)
        2 -> do
          tellNet [Duplicate (numbered variable 1) (numbered variable 2) := Variable variable]
        _ -> error "More than two occurences"

morteNetForward :: Variable -> Expr X -> Generator ()
morteNetForward continuation (Var variable) = do
  unusedVariable <- freshVariableOccurence (unpack (pretty variable))
  tellNet [Variable continuation := Variable unusedVariable]
morteNetForward continuation (Lam variable _ body) = do
  continuation2 <- freshContinuation
  tellNet [Construct (unpack variable) continuation2 := Variable continuation]
  morteNetForward continuation2 body
  establishSharing (unpack variable)
morteNetForward continuation (App function argument) = do
  functionVariable <- morteNetBackward function
  argumentVariable <- morteNetBackward argument
  tellNet [Variable functionVariable := Construct argumentVariable continuation]

morteNetBackward :: Expr X -> Generator Variable
morteNetBackward (Var variable) = do
  unusedVariable <- freshVariableOccurence (unpack (pretty variable))
  return unusedVariable
morteNetBackward (App function argument) = do
  functionContinuation <- morteNetBackward function
  argumentContinuation <- morteNetBackward argument
  continuation <- freshContinuation
  tellNet [Variable functionContinuation := Construct argumentContinuation continuation]
  return continuation

