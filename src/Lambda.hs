module Lambda where

import InteractionNet (
  InteractionNet, Statement(..), Term(..), Variable)

import Morte.Core (
  Expr(Var, Lam, App), X,
  pretty)

import Control.Monad.Trans.State (
  State, execState, get, put, modify)
import Data.Text.Lazy (
  unpack)


morteNet :: Variable -> Expr X -> InteractionNet
morteNet name expression =
  runGenerator (morteNetForward name expression)

type Generator = State (InteractionNet, Integer)

runGenerator :: Generator () -> InteractionNet
runGenerator generator = fst (execState generator ([],0))

tellNet :: InteractionNet -> Generator ()
tellNet net = do
  modify (\(net', i) -> (net' ++ net, i))

freshContinuation :: Generator Variable
freshContinuation = do
  (net, i) <- get
  put (net, (i + 1))
  return ("r" ++ show i)

morteNetForward :: Variable -> Expr X -> Generator ()
morteNetForward continuation (Var variable) = do
  tellNet [Variable continuation := Variable (unpack (pretty variable))]
morteNetForward continuation (Lam variable _ body) = do
  continuation2 <- freshContinuation
  tellNet [Construct (unpack variable) continuation2 := Variable continuation]
  morteNetForward continuation2 body
morteNetForward continuation (App function argument) = do
  functionVariable <- morteNetBackward function
  argumentVariable <- morteNetBackward argument
  tellNet [Variable functionVariable := Construct argumentVariable continuation]

morteNetBackward :: Expr X -> Generator Variable
morteNetBackward (Var variable) = return (unpack (pretty variable))

